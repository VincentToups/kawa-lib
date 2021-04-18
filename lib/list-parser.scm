(define-library (lib list-parser)
  (export
   parse-empty?
   parse-car
   peak-car
   parse-rest
   parse-predicate
   parse-predicate-value
   parse-number
   parse-numberish
   parse-symbol
   parse-symbol
   parse-symbolish
   parse-string
   parser-return
   parse-repeatedly
   begin-maybe-state
   maybe-state-return
   maybe-state-bind
   maybe-state-failure
   maybe-state-success
   maybe-state-failure?
   maybe-state-success?
   Maybe-State-Success
   Maybe-State-Failure   
   State
   parser
   parse-fail
   define-parser
   retrieve
   store)
  (import (except (kawa base) match format)
	  (rename (lib xformat) (xformat format))
	  (lib shadchen)
	  (lib maybe-state))

  (define (parse-fail . reason)
    (lambda ((state :: State))
      (apply maybe-state-failure reason)))

  (define (describe-function (f :: gnu.mapping.Procedure))
    (let ((name (or (f:getName) "anonymous-function"))
	  (loc (or (f:getSourceLocation) "unknown-location")))
      (format "<~a: ~a>" name loc)))
  
  (begin
    (define parse-empty?
      (begin-maybe-state
       (v <- retrieve)
       (if (eq? v '())
	   (maybe-state-return #t)
	   (parse-fail "Expected an empty list but got ~a" v))))

    (define parse-car
      (begin-maybe-state
       (v <- retrieve)
       (if (and (list? v)
		(not (eq? v '())))
	   (store-and-return (cdr v) (car v))
	   (parse-fail "The empty list has no car."))))

    (define peak-car
      (begin-maybe-state
       (v <- retrieve)
       (if (and (list? v)
		(not (eq? v '())))
	   (maybe-state-return (car v))
	   (parse-fail "The empty list has no car."))))

    (define parse-rest
      (parser
       (v <- retrieve)
       (if (list? v)
	   (store-and-return (list) v)
	   (parse-fail "parse-rest expected a list."))))

    (define (parse-predicate pred)
      (begin-maybe-state
       (hd <- parse-car)
       (if (pred hd)
	   (maybe-state-return hd)
	   (parse-fail "Predicate ~a failed on ~a" (describe-function pred) hd))))

    (define (parse-predicate-value pred)
      (begin-maybe-state
       (hd <- parse-car)
       (let ((v (pred hd)))
	 (if v
	     (maybe-state-return v)
	     (parse-fail "Predicate ~a failed on ~a" (describe-function pred) hd)))))

    (define (parse-predicate-transformed transform pred)
      (begin-maybe-state
       (hd <- parse-car)
       (let ((tr (transform hd)))
	 (if (pred tr)
	     (maybe-state-return tr)
	     (parse-fail "Predicate ~a failed on transformed value ~a -> ~a" (describe-function pred) hd (describe-function tr))))))

    (define (->number o)
      (cond
       ((number? o) o)
       ((string? o) (string->number o))
       ((symbol? o) (string->number (symbol->string o)))
       (else #f)))

    (define (numberish? o)
      (number? (->number o)))

    (define parse-number (parse-predicate number?))
    
    (define parse-numberish (parse-predicate-transformed
			     ->number
			     number?))    

    (define parse-string (parse-predicate string?))
    (define parse-symbol (parse-predicate symbol?))
    (define parse-symbolish
      (begin-maybe-state
       (o <- parse-car)
       (cond
	((numberish? o)
	 (parse-fail "Expected a non-numeric value that can be converted to or is a symbol but got ~a instead" o))
	((symbol? o)
	 (maybe-state-return o))
	((string? o)
	 (maybe-state-return (string->symbol o))))))

    (define (parse-repeatedly a-parser)
      (lambda (st :: State)
	(let loop ((acc (list))
		   (st :: State st))
	  (match (a-parser st)
	    ((? maybe-state-success? s)
	     (let ((s :: Maybe-State-Success s))
	       (loop (cons s:value acc)
		     s:state)))
	    ((? maybe-state-failure? f)
	     (maybe-state-success (reverse acc) st))))))

    (define parser-return maybe-state-return)
    
    (define-syntax parser
      (lambda (expr)
	(syntax-case expr ()
	  ((_ e0 e ...)
	   #'(begin-maybe-state e0 e ...)))))

    (define-syntax define-parser
      (lambda (expr)
	(syntax-case expr ()
	  ((_ name e0 e ...)
	   (identifier? #'name)
	   #'(define name (parser e0 e ...)))
	  ((_ (name arg ...) e0 e ...)
	   #'(identifier? name)
	   #'(define (name arg ...)
	       (parser e0 e ...))))))))
