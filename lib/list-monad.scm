(define-library (lib list-monad)
  (export begin-list
	  list-bind)
  (import (kawa base)
	  (lib do-notation))
  (begin
    (define (list-bind v f)
      (apply append (map f v)))
    (define-syntax begin-list
      (lambda (expr)
	(syntax-case expr ()
	  ((_ e0 e ...)
	   #'(begin-in list-bind e0 e ...)))))))
