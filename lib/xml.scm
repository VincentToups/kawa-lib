(define-library (lib xml)
  (export read-xml sexp->xml sexp->xml-string read-sexp write-sexp
	  sexp-file->xml-file sexp->xml-file xml-file->sexp-file)
  (import (except (kawa base) match)
	  (class javax.xml.parsers
	    DocumentBuilderFactory
	    DocumentBuilder)
	  (class org.w3c.dom
	    Document
	    NodeList
	    Node
	    Element
	    NamedNodeMap)
	  (class java.io
	    File)
	  (lib shadchen)
	  (lib util)
	  (lib funny)
	  (only (kawa pprint) pprint))
  (begin


    (define (read-sexp f)
      (with-input-from-file f
	(lambda (port)
	  (read port))))

    (define (write-sexp sexp f)
      (let* ((old-value *print-right-margin*)
	     (ignore (set! *print-right-margin* 64))
	     (result     
	      (call-with-output-file f
		(lambda (port)
		  (pprint sexp port))))
	     (ignore2 (set! *print-right-margin* old-value)))
	result))

    (define (sexp-file->xml-file f-in f-out)
      (chain $
	     (read-sexp f-in)
	     (sexp->xml-file $ f-out)))

    (define (xml-file->sexp-file f-in f-out)
      (chain $
	     (read-xml f-in)
	     (write-sexp $ f-out)))

    (define (port2str f)
      (lambda (a)
	(call-with-output-string (lambda (pt)
				   (f a pt)))))
    
    (define (read-to-document file)
      (let ((d (.. DocumentBuilderFactory
		    (newInstance)
		    (newDocumentBuilder)
		    (parse (File file)))))
	(.. d (normalizeDocument))
	d))

    (define (node-attributes->sexp (node :: Node))
      (let* ((mp :: NamedNodeMap (.. node (getAttributes)))
	     (n (.. mp (getLength))))
	(let loop ((i 0)
		   (acc (list)))
	  (cond ((< i n)
		 (let* ((item :: Node (.. mp (item i)))
			(name (.. item (getNodeName)))
			(value (.. item (getNodeValue))))
		   (loop (+ i 1)
			 (append (list value (string->symbol name)) acc))))
		(else
		 (cons 'attributes (reverse acc)))))))

    (define (node-type (node :: Node))
      (let ((node-code (.. node (getNodeType))))
	(cond
	 ((eq? node-code Node:ATTRIBUTE_NODE) 'attribute)
	 ((eq? node-code Node:CDATA_SECTION_NODE) 'cdata_section)
	 ((eq? node-code Node:COMMENT_NODE) 'comment)
	 ((eq? node-code Node:DOCUMENT_FRAGMENT_NODE) 'document_fragment)
	 ((eq? node-code Node:DOCUMENT_NODE) 'document)
	 ((eq? node-code Node:DOCUMENT_TYPE_NODE) 'document_type)
	 ((eq? node-code Node:ELEMENT_NODE) 'element)
	 ((eq? node-code Node:ENTITY_NODE) 'entity)
	 ((eq? node-code Node:ENTITY_REFERENCE_NODE) 'entity_reference)
	 ((eq? node-code Node:NOTATION_NODE) 'notation)
	 ((eq? node-code Node:PROCESSING_INSTRUCTION_NODE) 'processing_instruction)
	 ((eq? node-code Node:TEXT_NODE) 'text)
	 (else (error "node-type" "Unrecognized node code:" node-code)))))

    (define *pass* '(pass))
    (define (map-child-nodes f (nd :: Node))
      (let* ((nl :: NodeList (.. nd (getChildNodes)))
	     (n (.. nl (getLength))))
	(let loop ((i 0)
		   (acc '()))
	  (cond
	   ((= i n) (reverse acc))
	   ((< i n)	    
	    (let ((v (f (.. nl (item i)))))
	      (if (not (eq? v *pass*))
		  (loop (+ i 1)
			(cons v acc))
		  (loop (+ i 1)
			acc))))))))

    (define (node->sexp (node :: Node))
      (let ((type (node-type node)))
	(case type
	  ((document)
	   (cons '!document (map-child-nodes node->sexp node)))
	  ((element)
	   (append
	    (list (string->symbol (.. node (getNodeName)))
		  (node-attributes->sexp node))
	    (map-child-nodes node->sexp node)))
	  ((text)
	   (let ((txt (.. node (getTextContent) (strip))))
	     (if (equal? txt "") *pass* txt)))
	  ((comment)
	   (list '!-- (.. node (getTextContent))))
	  (else
	   (display (format "Warning: node-sexp: unknown node type ~a.\n" type))
	   *pass*))))

    (define (read-xml fn)
      (chain $
	     (read-to-document fn)
	     (node->sexp $)))

    (define (tag-has-attributes? sexp)
      (and (list? sexp)
	   (not (eq? sexp '()))
	   (not (eq? (cdr sexp) '()))
	   (let ((snd (list-ref sexp 1)))
	     (and (not (eq? snd '()))
		  (eq? (car snd) 'attributes)))))

    (define (regularize-element-wrt-attributes sexp)
      (if (tag-has-attributes? sexp)
	  sexp
	  (append (list (car sexp) '(attributes))
		  (cdr sexp))))

    (define (attributes->xml-fragment attributes port)
      (match attributes
	((list) 'done)
	((list* name value rest)
	 (display (format "~a=~s" name (format "~a" value)) port)
	 (when (not (eq? rest '()))
	   (display (format " ") port))
	 (attributes->xml-fragment rest port))))

    (define (sexp->xml sexp port indent)
      (define (out . args)
	(display (make-string indent #\space) port)
	(display (apply format args) port))
      (define (out* . args)
	(display (apply format args) port))
      (cond
       ((or (string? sexp) (number? sexp))
	(out sexp)
	(out "\n"))
       ((and (list? sexp)
	     (not (eq? sexp '())))
	(case (car sexp)
	  ((!--)
	   (out "<!-- \n") 
	   (for-each (lambda (x)
		       (display x port))
		     (cdr sexp))
	   (out "-->\n"))
	  ((!document)
	   (for-each (lambda (sub-element)
		       (sexp->xml sub-element port indent))
		     (cdr sexp)))
	  (else (let* ((element (regularize-element-wrt-attributes sexp))
		       (tag (car element))
		       (attributes (cdr (cadr element)))
		       (elements (cdr (cdr element))))
		  (out "<~a" tag)
		  (when (not (eq? attributes '()))
		    (out* " ")
		    (attributes->xml-fragment attributes port))
		  (cond ((not (eq? elements '()))
			 (out* ">\n")
			 (for-each (lambda (sub-element)
				     (sexp->xml sub-element port (+ indent 1)))
				   elements)
			 (out "</~a>\n" tag))
			(else
			 (out* "/>\n")))))))))

    (define (sexp->xml-file sexp filename)
      (call-with-output-file filename
	(lambda (port)
	  (sexp->xml sexp port 0))))


    (define (sexp->xml-string sexp indent)
      (call-with-output-string (lambda (port)
				 (sexp->xml sexp port indent))))))
