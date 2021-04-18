(define-library (lib state)
  (export
   begin-state
   state-instance?
   state-result-instance?
   state-result-state
   state-result-value
   store
   retrieve
   State
   State-Result
   state-return
   state-bind)
  (import (except (kawa base) match)
	  (lib shadchen)
	  (lib do-notation))
  (begin
    (define-simple-class State ()
      (value)
      ((*init* v)
       (set! value v))
      ((*init*)
       (set! value #!void))
      ((toString)
       (format "(State ~a)" value)))

    (define (state-instance? o)
      (instance? o State))

    (define-simple-class State-Result ()
      (value)
      (state :: State)
      ((*init* v (st :: State))
       (set! value v)
       (set! state st))
      ((toString)
       (format "(State-Result ~a ~a)" value state)))

    (define (state-result-state (s :: State-Result))::State
      s:state)

    (define (state-result-value (s :: State-Result))
      s:value)

    (define (state-result-instance? x)
      (instance? x State-Result))

    (define (state-bind v f)
      (lambda ((state :: State))
	(let ((r :: State-Result (v state)))
	  ((f r:value) r:state))))

    (define (state-return v)
      (lambda (state)
	(State-Result v state)))

    (define (store x)
      (lambda (state :: State)
	(State-Result x (State x))))

    (define retrieve
      (lambda ((state :: State))
	(State-Result state:value state)))

    (define-syntax begin-state
      (lambda (expr)
	(syntax-case expr ()
	  ((_ e0 e ...)
	   #'(begin-in state-bind e0 e ...)))))))
