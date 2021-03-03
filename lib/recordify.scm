(define-library (lib recordify)
  (export recordify record-meta-data-table get-record-meta-data record-slot-count record-protocol record-raw-setter record-raw-maker hashtable-set! hashtable-ref split-list-n)
  (import (kawa base)
	  (rnrs hashtables (6)))

  (begin
    (define (split-list-n l n)
      (let loop ((lista '())
		 (listb l)
		 (n n))
	(cond ((or (eq? listb '())
		   (= n 0))
	       (list (reverse lista) listb))
	      (#t
	       (loop (cons (car listb) lista)
		     (cdr listb)
		     (- n 1))))))
    (define-simple-class record-meta-data ()
      (the-record-class)
      (the-parent-record-class)
      (slot-count)
      (raw-maker)
      (raw-setter)
      (protocol))
    (define record-meta-data-table (make-eq-hashtable))
    (define (get-record-meta-data class-name)
      (if (hashtable-contains? record-meta-data-table class-name)
	  (hashtable-ref record-meta-data-table class-name #f)
	  (error (format "No record for ~a." record))))
    (define record-slot-count (lambda (record)
				(if (hashtable-contains? record-meta-data-table record)
				    (hashtable-ref record-meta-data-table record #f):slot-count
				    (error (format "No record for ~a" record)))))
    (define record-protocol (lambda (record)
			      (if (hashtable-contains? record-meta-data-table record)
				  (hashtable-ref record-meta-data-table record 0):protocol
				  (error (format "No record for ~a" record)))))
    (define record-raw-setter (lambda (record)
			      (if (hashtable-contains? record-meta-data-table record)
				  (hashtable-ref record-meta-data-table record 0):raw-setter
				  (error (format "No record for ~a" record)))))
    (define record-raw-maker (lambda (record)
			      (if (hashtable-contains? record-meta-data-table record)
				  (hashtable-ref record-meta-data-table record 0):raw-maker
				  (error (format "No record for ~a" record)))))
    (define-syntax recordify
      (lambda (stx)
	(define (plist-get plst k alt)
	  (cond ((or (eq? '() plst)
		     (eq? '() (cdr plst)))
		 alt)
		((eq? (car plst) k)
		 (cadr plst))
		(else
		 (plist-get (cdr (cdr plst)) k alt))))
	(define (looks-like stx sym)
	  (eq? (syntax->datum stx) sym))
	(define (field-stx? stx)
	  (syntax-case stx ()
	    ((id args ...)
	     #'(identifier? id)
	     #t)
	    (anything-else #f)))
	(define (public-field-stx? stx)
	  (if (and (field-stx? stx)
		(let ((access (plist-get (cdr (syntax->datum stx)) 'access: '(quote public))))
		  (or (equal? access '(quote public))
		      (memq 'public (cadr access)))))
	      #t
	      #f))
	(define (public-field-name stx)
	  (if (public-field-stx? stx)
	      (car stx)
	      (error "recordify public-field-name" "Tried to get a field name from a non-public field description." stx)))
	(define (make-accessors class-name-stx field-name-stx mutable?)
	  (let ((getter-name (datum->syntax-object class-name-stx)))))
	(syntax-case stx ()
	  ((_ (define-simple-class class-name (superclass ...) form ...) (protocol protocol-fn) ...)
	   (let ((n-extra-forms (length #'((protocol protocol-fn) ...)))
		 (n-super-classes (length #'(superclass ...))))
	     (and (identifier? #'class-name)
		  (or (= 0 n-super-classes)
		      (identifier? (car #'(superclass ...))))
		  (or (and (= 1 n-extra-forms)
			   (looks-like (car (car #'((protocol protocol-fn) ...))) 'protocol))
		      (= 0 n-extra-forms))))
	   #'(begin (define-simple-class class-name (superclass ...) form ...))))))
    ;; (define-syntax recordify
    ;;   (lambda (stx)
    ;; 	(define (symbol-append delim . args)
    ;; 	  (let loop ((acc '())
    ;; 		     (rest args))
    ;; 	    (cond
    ;; 	     ((eq? rest '()) (string->symbol (apply string-append (reverse acc))))
    ;; 	     ((eq? (cdr rest) '())
    ;; 	      (loop (cons (symbol->string (car rest)) acc)
    ;; 		    (cdr rest)))
    ;; 	     (else
    ;; 	      (let ((hd (car rest))
    ;; 		    (tl (cdr rest)))
    ;; 		(loop (cons (symbol->string delim) (cons (symbol->string hd) acc))
    ;; 		      tl))))))
    ;; 	(define (mapcat f l)
    ;; 	  (apply append (map f l)))
    ;; 	(syntax-case stx (define-simple-class protocol)
    ;; 	  ((_ (define-simple-class class-name () forms ...))
    ;; 	   #'(recordify (define-simple-class class-name () forms ...) (protocol (lambda (maker-raw)
    ;; 										  maker-raw))))
    ;; 	  ((_ (define-simple-class class-name (parent-class) forms ...))
    ;; 	   (identifier? #'parent-class)
    ;; 	   #'(recordify (define-simple-class class-name () forms ...) (protocol (lambda (parent-class-protocol)
    ;; 										  (lambda args
    ;; 										    (let* ((splt (split-list-n args (record-slot-count parent-class)))
    ;; 											   (pc-args (car splt))
    ;; 											   (cc-args (cadr splt))
    ;; 											   (cc-maker (apply parent-class-protocol pc-args)))
    ;; 										      (apply cc-maker cc-args)))))))
    ;; 	  ((_ (define-simple-class class-name (superclass ...) forms ...) (protocol fn))
    ;; 	   (let ((has-superclass (not (= 0 (length #'(superclass ...)))))
    ;; 		 (build-accessors (lambda (form)
    ;; 				    (syntax-case form ()
    ;; 				      ((id0 term ...)
    ;; 				       (identifier? #'id0)
    ;; 				       (let* ((cn (syntax->datum #'class-name))
    ;; 					      (sn (syntax->datum #'id0)))
    ;; 					 (with-syntax ((getter-name (datum->syntax-object #'class-name (symbol-append '- cn sn)))
    ;; 						       (setter-name (datum->syntax-object #'class-name (symbol-append '- cn sn 'set!))))
    ;; 					   #'(begin (define (getter-name (obj ::class-name))
    ;; 						      obj:id0)
    ;; 						    (define (setter-name (obj ::class-name) v)
    ;; 						      (set! obj:id0 v))))))
    ;; 				      ((_ ...)
    ;; 				       #'(begin))))))
    ;; 	     (with-syntax (;((accessor-form ...) (map build-accessors #'(forms ...)))
    ;; 			   (maker-name
    ;; 			    (datum->syntax-object
    ;; 			     #'class-name
    ;; 			     (string->symbol (format "make-~a" (syntax->datum #'class-name)))))
    ;; 			   ((arg0 ...) (mapcat (lambda (form)
    ;; 						 (syntax-case form ()
    ;; 						   ((id0 id ...)
    ;; 						    (identifier? #'id0)
    ;; 						    #'(id0))
    ;; 						   (() #'()))) #'(forms ...)))
    ;; 			   ;; (maker-form (if has-superclass
    ;; 			   ;; 		   #'(define maker-name (let ((n (let* ((pc-meta :: record-meta-data (get-record-meta-data superclass ...))
    ;; 			   ;; 							(cc-meta :: record-meta-data (get-record-meta-data class-name))
    ;; 			   ;; 							(make-pc-raw-args (pc-meta:protocol list)))
    ;; 			   ;; 						   (lambda pc-args
    ;; 			   ;; 						     (lambda cc-args
    ;; 			   ;; 						       (let ((v :: class-name (cc-meta:the-record-class)))
    ;; 			   ;; 							 (v:set-all-slots! (append (make-pc-raw-args pc-args)
    ;; 			   ;; 										   cc-args))))))))
    ;; 			   ;; 					  (cc-meta:protocol n)))
    ;; 			   ;; 		   #'(define maker-name ((record-protocol class-name) (record-raw-maker class-name)))))
    ;; 			   ;; (set-all-slots-form (if has-superclass
    ;; 			   ;; 			   #'((set-all-slots! args)
    ;; 			   ;; 			      (let* ((parent-slot-count (record-slot-count superclass ...))
    ;; 			   ;; 				     (value-list args)
    ;; 			   ;; 				     (splits (split-list-n value-list parent-slot-count))
    ;; 			   ;; 				     (this-cc :: class-name (this))
    ;; 			   ;; 				     (this-pc :: superclass ... (this)))
    ;; 			   ;; 				(invoke-special superclass ... this-pc 'set-immediate-slots! (car splits))
    ;; 			   ;; 				(invoke this 'set-immediate-slots! (cadr splits))))
    ;; 			   ;; 			   #'((set-all-slots! args)
    ;; 			   ;; 			      (define ths :: class-name (this))
    ;; 			   ;; 			      (invoke ths 'set-immediate-slots! args))))
    ;; 			   (parent-class-expr (if has-superclass
    ;; 						  (car #'(superclass ...))
    ;; 					     #'#f)))
    ;; 	       #'(begin
    ;; 		   (define-simple-class class-name (superclass ...)
    ;; 		     ;; ((set-immediate-slots! args)
    ;; 		     ;;  (define ths :: class-name (this))
    ;; 		     ;;  (apply (lambda (arg0 ...)
    ;; 		     ;; 	       (set! ths:arg0 arg0)
    ;; 		     ;; 	       ...)
    ;; 		     ;; 	     args))
    ;; 		     ;set-all-slots-form
    ;; 		     forms ...)
    ;; 		   ;;accessor-form ...
    ;; 		   ;; (hashtable-set! record-meta-data-table class-name
    ;; 		   ;; 		   (record-meta-data the-record-class: class-name
    ;; 		   ;; 				     the-parent-record-class: parent-class-expr
    ;; 		   ;; 				     slot-count: (length '(accessor-form ...))
    ;; 		   ;; 				     raw-maker: (lambda (arg0 ...)
    ;; 		   ;; 						  (let ((v (class-name)))
    ;; 		   ;; 						    ((record-raw-setter class-name) v arg0 ...)))
    ;; 		   ;; 				     raw-setter: (lambda (v arg0 ...)
    ;; 		   ;; 						   (set! v:arg0 arg0)
    ;; 		   ;; 						   ...)
    ;; 		   ;; 				     protocol: fn))
    ;; 		   ;;maker-form
    ;; 		   class-name)))))))
    ))