(define-library (lib mecs-protocols)
  (export mecs-protocol make-system-protocol make-tracked-system-protocol)
  (import (except (kawa base) match)
	  (lib shadchen)
	  (lib util)
	  (rnrs hashtables (6))
	  (lib mecs-syntax))
  
  (begin

    (define (cons* . args)
      (match (reverse args)
	((list* (? list? the-list) (call reverse other-things))
	 (append other-things the-list))
	(anything-else
	 (error "cons* (mecs-protocol)" "Last element must be a list." args))))

    (define (parse-component-args arglist)
      (define (helper al ac)
	(match al
	  ((list) (reverse ac))
	  ((list (symbol? component-name))
	   (reverse (cons (list
			   'name component-name
			   'constructor (lambda (e v) v))
			  ac)))
	  ((list* (symbol? component-name) (symbol? another-name) rest)
	   (helper (cons another-name rest)
		   (cons (list
			  'name component-name
			  'constructor (lambda (e v) v))
			 ac)))
	  ((list* (symbol? component-name)
		  (procedure? constructor)
		  rest)
	   (helper rest
		   (cons (list 'name component-name
			       'constructor constructor)
			 ac)))
	  ((list* (symbol? component-name)
		  (list? plist)
		  rest)
	   (helper rest
		   (cons (append (list 'name component-name) plist)
			 ac)))
	  ((list* (and (list* (symbol? component-name) other-comp-args)
		       wrapped) rest)
	   (helper (append wrapped rest) ac))))
      (helper arglist '()))
    
    (define (symbolic-plist? lst)
      (match lst
	((list) #t)
	((list x) #f)
	((list* (symbol? x) value rest)
	 (symbolic-plist? rest))
	(anything-else #f)))
    
    (define (parse-system-args lst)
      (if (not (list? lst))
	  (error "parse-system-args" "Object must be a list." lst))
      (let loop ((rest lst)
		 (ac (list)))
	(match rest
	  ((list) (reverse ac))
	  ((or (list* (symbol? system-name)
		      (? list-of-symbols? components)
		      (? symbolic-plist? additional-args)
		      rest)
	       (and (let (additional-args '()))
		    (list* (symbol? system-name)
			   (? list-of-symbols? components)
			   rest)))
	   (loop rest (cons (list system-name components additional-args) ac)))
	  (anything-else (error "parse-system-args" "Can't parse the system argument list during mecs construction starting at irritant" anything-else lst)))))

  (define (default-string-converter o)
    (call-with-output-string (lambda (port)
			     (display o port))))

  (define (default-reader s)
    (call-with-input-string s read))

  (define logor bitwise-ior)

    
  (define (mecs-protocol make-mecs-raw make-component-raw make-tracked-system)
    (lambda (components . systems)
      (let* ((systems (if (eq? systems '()) '() (car systems)))
	     (ctable (make-eq-hashtable))
	     (parsed (parse-component-args components))
	     (cvector (make-vector (length parsed)))
	     (parsed-systems (parse-system-args systems))
	     (stable (make-eq-hashtable))
	     (n-systems (length parsed-systems))
	     (svector (make-vector n-systems))
	     (the-mecs (make-mecs-raw (make-eq-hashtable) 				   
					cvector
					ctable
					svector
					stable)))
	  (let loop ((rest parsed)
		     (i 0))
	    (match rest
	      ((list) the-mecs)
	      ((list* (and (list? plist)
			   (let (name (plist-get plist 'name #f))
			     (constructor (plist-get plist 'constructor (value-component)))
			     (cleanup (plist-get plist 'cleanup (lambda (e) #f)))
			     (->string (plist-get plist '->string default-string-converter))
			     (string-> (plist-get plist 'string-> default-reader)))) rest)
	       (when (eq? name #f) (error "make-mecs (mecs-protocol)" "No component name in component plist" plist name))
	       (hashtable-set! ctable name i)
	       (vector-set! cvector i (make-component-raw the-mecs i name (expt 2 i) constructor cleanup ->string string->))
	       (loop rest (+ i 1)))))
	  (let loop ((rest parsed-systems)
		     (i 0))
	    (match rest
	      ((list) the-mecs)
	      ((list* (list system-name components-list additional-args) rest)
	       (let ((the-system (apply make-tracked-system (cons* the-mecs components-list additional-args))))
		 (vector-set! svector i the-system)
		 (hashtable-set! stable system-name i))
	       (loop rest (+ i 1)))))
	  the-mecs)))
    
    (define (make-system-protocol make-system-raw mecs-component component-mask)
      (lambda (the-mecs . components)
	(let* ((n (length components))
	       (cc (make-vector n))
	       (mask (let loop ((s 0)
				(rest components)
				(i 0))
		       (match rest
			 ((list) s)
			 ((list* (and (symbol? component-name)
				      (-> name (mecs-component the-mecs name) c)) rest)
			  (vector-set! cc i c)
			  (loop (logor s (component-mask c))
				rest
				(+ i 1)))))))
	  (make-system-raw the-mecs cc mask))))

    (define (nop . args)
      #f)
    
    (define (make-tracked-system-protocol parent-constructor)
      (lambda (the-mecs components . opts)
	(let ((make-tracked-system-raw (apply parent-constructor (cons the-mecs components))))
	  (let loop ((rest opts)
		     (tmp (make-eq-hashtable)))	  
	    (match rest
	      ((list) (make-tracked-system-raw
		       (hashtable-ref tmp 'enter nop)
		       (hashtable-ref tmp 'exit nop)
		       (hashtable-ref tmp 'update nop)
		       (make-eq-hashtable)))
	      ((list* (and
		       the-keyword
		       (or 'enter 'exit 'update))
		      (procedure? callback) rest)
	       (hashtable-set! tmp the-keyword callback)
	       (loop rest tmp))
	      ((list* (symbol? unrecognized-kw)
		      (procedure? prc) rest)
	       (error "make-tracked-system-protocol" (format "Unrecognized keyword in make-tracked-system argument list ~a" unrecognized-kw) unrecognized-kw opts))
	      (anything-else (error "make-tracked-system-protocol" (format "Cannot parse arguments to make-tracked-system-protocol") opts)))))))))
