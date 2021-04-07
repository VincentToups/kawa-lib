(define-library (lib maybe)
  (export begin-maybe just-instance? nothing-instance? maybe-return Maybe Just Nothing just nothing maybe-bind)
  (import (except (kawa base) match)
	  (lib shadchen)
	  (lib do-notation))
  (begin
    (define-simple-class Maybe ())
    (define-simple-class Just (Maybe)
      (value))
    (define-simple-class Nothing (Maybe))

    (define (just-instance? x)
      (instance? x Just))

    (define (nothing-instance? x)
      (instance? x Nothing))

    (define (maybe-return x)
      (Just value: x))

    (define maybe-zero (Nothing))

    (define maybe-bind
      (lambda (v f)
	(match v
	  ((? just-instance? x) (f (let ((just :: Just v))
				     just:value)))
	  ((? nothing-instance? x) x))))

    (define (just x)
      (Just value: x))

    (define nothing (Nothing))

    (define-syntax begin-maybe
      (lambda (expr)
	(syntax-case expr ()
	  ((_ expr0 expr ...)
	   #'(begin-in maybe-bind expr0 expr ...)))))))
