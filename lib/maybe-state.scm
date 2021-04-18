(define-library (lib maybe-state)
  (export
   State
   Maybe-State-Result
   Maybe-State-Success
   Maybe-State-Failure
   begin-maybe-state
   maybe-state-and
   maybe-state-bind
   maybe-state-failure
   maybe-state-failure?
   maybe-state-or
   maybe-state-return
   maybe-state-success
   maybe-state-success?
   retrieve
   store
   store-and-return)
  (import (except (kawa base) match)
	  ;(rename (lib xformat) (xformat format))
	  (lib do-notation)
	  (lib shadchen)
	  (only (lib state) State state-instance?))
  (begin
    (define-simple-class Maybe-State-Result ())
    (define-simple-class Maybe-State-Failure (Maybe-State-Result)
      (reason)
      ((*init* input-reason)
       (set! reason input-reason))
      ((explain)
       (apply format reason))
      ((toString)
       (format "(Maybe-State-Failure ~s)"
	       (apply format reason))))

    (define (maybe-state-failure . reason)
      (Maybe-State-Failure reason))
    
    (define-simple-class Maybe-State-Success (Maybe-State-Result)
      (state :: State)
      (value)
      ((*init* v st)
       (if (instance? st State)
	   (set! state st)
	   (set! state (State st)))
       (set! value v))
      ((toString)
       (format "(Maybe-State-Success ~s ~s)" value state)))

    (define (maybe-state-success retval newstate)
      (Maybe-State-Success retval newstate))

    (define (maybe-state-failure? o)
      (instance? o Maybe-State-Failure))

    (define (maybe-state-success? o)
      (instance? o Maybe-State-Success))

    (define (maybe-state-bind v f)
      (lambda ((state :: State))
	(let ((r :: Maybe-State-Result (v state)))
	  (cond
	   ((maybe-state-success? r)
	    (let ((r :: Maybe-State-Success r))
	      ((f r:value) r:state)))
	   ((maybe-state-failure? r)
	    r)))))

    (define (maybe-state-or . msvs)
      (cond
       ((eq? msvs '())	
	(error "maybe-state-or" "maybe-state-or-requires at least two arguments." msvs))
       ((eq? (cdr msvs) '())
	(car msvs))
       (else
	(let ((msf1 (car msvs))
	      (rest (cdr msvs)))
	  (lambda (st :: State)
	    (let ((r (msf1 st)))
	      (cond
	       ((maybe-state-success? r)
		r)
	       ((maybe-state-failure? r)
		((apply maybe-state-or rest) st)))))))))

    (define (maybe-state-and . msvs)
      (cond
       ((eq? msvs '())
	(error "maybe-state-and" "maybe-state-and requires at least one argument." msvs))
       ((eq? (cdr msvs) '())
	(car msvs))
       (else
	(let ((msf1 (car msvs))
	      (rest (cdr msvs)))
	  (lambda (st :: State)
	    (let ((r (msf1 st)))
	      (cond
	       ((maybe-state-success? r)
		((apply maybe-state-and rest) st))
	       ((maybe-state-failure? r)
		r))))))))
    
    (define (maybe-state-return v)
      (lambda (state :: State)
	(Maybe-State-Success v state)))

    (define (store value)
      (lambda ((state :: State))
	(Maybe-State-Success value (State value))))

    (define (store-and-return value retval)
      (lambda ((state :: State))
	(Maybe-State-Success retval (State value))))
    
    (define retrieve
      (lambda ((state :: State))
	(Maybe-State-Success state:value state)))

    (define-syntax begin-maybe-state
      (lambda (expr)
	(syntax-case expr ()
	  ((_ e0 e ...)
	   #'(begin-in maybe-state-bind e0 e ...)))))))
