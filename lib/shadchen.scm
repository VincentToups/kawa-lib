(define-library
    (lib shadchen)
  (export match match1-or-fail shadchen-tests)
  (import (except (kawa base) match test-begin)
	  (srfi 28)
	  (srfi 64)
	  (rnrs sorting (6))
	  (rnrs hashtables (6))
	  (lib shadchen-helpers))
  (begin    
    (define *shadchen-fail* (list 'shadchen-fail))

    (define-syntax if* (lambda (expr)
			 (syntax-case expr ()
			   ((_ c a)
			    #'(error "if*" "Malformed if*."))
			   ((_ c a b)
			    #'(if c a b)))))

    (define-syntax match-predicate
      (lambda (expr)
	(syntax-case expr ()
	  ((_ value (predicate pattern) body0 body ...)
	   #'(let ((pv predicate)
		   (v value))
	       (if* (pv v) (match1-or-fail v pattern body0 body ...)
		    *shadchen-fail*)))
	  ((_ value (predicate) body0 body ...)
	   #'(let ((pred predicate)
		   (v value))
	       (if* (pred v)
		    (begin body0 body ...)
		    *shadchen-fail*))))))

    (define-syntax match-call
      (lambda (expr)
	(syntax-case expr ()
	  ((_ value (proc pattern) body0 body ...)
	   #'(let ((pv proc)
		   (v value))
	       (match1-or-fail (pv v) pattern body0 body ...))))))

    (define-syntax match-list
      (lambda (expr)
	(syntax-case expr ()
	  ((_ value () body0 body ...)
	   #'(let ((v value))
	       (if* (and (list? v)
			 (eq? v '()))
		    (begin body0 body ...)
		    *shadchen-fail*)))
	  ((_ value (pattern0 pattern ...) body0 body ...)
	   #'(let ((v value))
	       (if* (and (list? v)
			 (not (eq? v '())))
		    (match1-or-fail (car v) pattern0
				    (match-list (cdr v) (pattern ...) body0 body ...))
		    *shadchen-fail*))))))

    (define-syntax match-list*
      (lambda (expr)
	(syntax-case expr ()
	  ((_ value () body0 body ...)
	   #'(let ((v value))
	       (if* (and (list? v)
			 (eq? v '()))
		    (begin body0 body ...)
		    *shadchen-fail*)))
	  ((_ value (pattern) body0 body ...)
	   #'(match1-or-fail value pattern body0 body ...))
	  ((_ value (pattern0 pattern ...) body0 body ...)
	   #'(let ((v value))
	       (if* (and (list? v)
			 (not (eq? v '())))
		    (match1-or-fail (car v) pattern0
				    (match-list* (cdr v) (pattern ...) body0 body ...))
		    *shadchen-fail*))))))

    (define-syntax match-transform
      (lambda (expr)
	(syntax-case expr ()
	  ((_ value (name tr-expr pattern) body0 body ...)
	   (identifier? #'name)
	   #'(match1-or-fail ((lambda (name) tr-expr) value) pattern body0 body ...)))))

    (define-syntax match-and
      (lambda (expr)
	(syntax-case expr ()
	  ((_ value () body0 body ...) #'(begin body0 body ...))
	  ((_ value (p0 p ...) body0 body ...)
	   #'(let ((v value))
	       (match1-or-fail v p0 (match-and v (p ...) body0 body ...)))))))

    (define-syntax match-or*
      (lambda (expr)
	(syntax-case expr ()
	  ((_ value () body0 body ...)
	   #'*shadchen-fail*)
	  ((_ value (p0 p ...) body0 body ...)
	   #'(let* ((v value)
		    (r (match1-or-fail v p0 body0 body ...)))
	       (if* (eq? r *shadchen-fail*)
		    (match-or* v (p ...) body0 body ...)
		    r))))))

    (define-syntax match-or
      (lambda (expr)
	(syntax-case expr ()
	  ((_ value (pattern ...) body0 body ...)
	   (pattern-bindings-equal? (syntax->datum #'(pattern ...)))
	   #'(match-or* value (pattern ...) body0 body ...))
	  (anything-else (error "match-or"  (format "Bad match-or form ~a" expr) expr)))))
    
    (define-syntax match-let
      (lambda (expr)
	(syntax-case expr ()
	  ((_ value ((id0 expr0) ...) body0 body ...)
	   (all-identifiers? #'(id0 ...))
	   #'(let ((id0 expr0) ...) body0 body ...)))))

    (define-syntax match-hashtable-ref
      (lambda (expr)
	(syntax-case expr ()
	  ((_ value (key) body0 body ...)
	   (identifier? #'key)
	   #'(let ((v value))
	       (if* (and (hashtable? v)
			 (hashtable-contains? v (quote key)))
		    (let ((key (hashtable-ref v (quote key) 'should-never-happen)))
		      body0 body ...)
		    *shadchen-fail*)))
	  ((_ value (key pat) body0 body ...)
	   #'(let ((v value))
	       (if* (and (hashtable? v)
			 (hashtable-contains? v (quote key)))
		    (let ((ref (hashtable-ref v (quote key) 'should-never-happen)))
		      (match1-or-fail ref pat body0 body ...))
		    *shadchen-fail*))))))

    (define-syntax match-hashtable-ref-or
      (lambda (expr)
	(syntax-case expr ()
	  ((_ value (key dft) body0 body ...)
	   (identifier? #'key)
	   #'(let ((v value))
	       (if* (hashtable? v)
		    (let ((key (hashtable-ref v (quote key) dft)))
		      body0 body ...)
		    *shadchen-fail*)))
	  ((_ value (key dft pat) body0 body ...)
	   #'(let ((v value))
	       (if* (hashtable? v)
		    (let ((ref (hashtable-ref v (quote key) dft)))
		      (match1-or-fail ref pat body0 body ...))
		    *shadchen-fail*))))))

    (define (truthy? x)
      (not (eq? x #f)))


    (define-syntax match1-or-fail
      (lambda (expr)
	(syntax-case expr (assert number? string? char? list? procedure? symbol? list vector? vector list* -> call and or quote hashtable-ref hashtable-ref-or let)
	  ((_ value x body0 body ...)
	   (identifier? #'x)
	   #'(let ((x value)) body0 body ...))
	  ((_ value x body0 body ...)
	   (number? (syntax->datum #'x))
	   #'(let ((v value))
	       (if* (and (number? v)
			 (= v x))
		    (begin body0 body ...)
		    *shadchen-fail*)))
	  ((_ value x body0 body ...)
	   (string? (syntax->datum #'x))
	   #'(let ((v value))
	       (if* (and (string? v)
			 (string=? v #'x))
		    (begin body0 body ...)
		    *shadchen-fail*)))
	  ((_ value #f body0 body ...)
	   #'(let ((v value))
	       (if* (eq? v #f)
		    (begin body0 body ...)
		    *shadchen-fail*)))
	  ((_ value #t body0 body ...)
	   #'(let ((v value))
	       (if* (eq? v #f)
		    (begin body0 body ...)
		    *shadchen-fail*)))
	  ((_ value (hd tl ...) body0 body ...)
	   (let ((hd-symbol (syntax->datum #'hd)))
	     (case hd-symbol
	       ((assert ?) #'(match-predicate value (tl ...) body0 body ...))
	       ((number?) #'(match1-or-fail value (? number? tl ...) body0 body ...))
	       ((integer?) #'(match1-or-fail value (? integer? tl ...) body0 body ...))
	       ((symbol?) #'(match1-or-fail value (? symbol? tl ...) body0 body ...))
	       ((string?) #'(match1-or-fail value (? string? tl ...) body0 body ...))
	       ((list?) #'(match1-or-fail value (? list? tl ...) body0 body ...))
	       ((vector?) #'(match1-or-fail value (? vector? tl ...) body0 body ...))
	       ((char?) #'(match1-or-fail value (? char? tl ...) body0 body ...))
	       ((truthy?) #'(match1-or-fail value (? truthy? tl ...) body0 body ...))
	       ((procedure?) #'(match1-or-fail value (? procedure? tl ...) body0 body ...))
	       ((hashtable?) #'(match1-or-fail value (? hashtable? tl ...) body0 body ...))
	       ((call) #'(match-call value (tl ...) body0 body ...))
	       ((quote) #'(if* (eq? value (quote tl ...))
			       (begin body0 body ...)
			       *shadchen-fail*))
	       ((list) #'(match-list value (tl ...) body0 body ...))
	       ((list*) #'(match-list* value (tl ...) body0 body ...))
	       ((->) #'(match-transform value (tl ...) body0 body ...))
	       ((and) #'(match-and value (tl ...) body0 body ...))
	       ((or) #'(match-or value (tl ...) body0 body ...))
	       ((let) #'(match-let value (tl ...) body0 body ...))
	       ((hashtable-ref) #'(match-hashtable-ref value (tl ...) body0 body ...))
	       ((hashtable-ref-or) #'(match-hashtable-ref-or value (tl ...) body0 body ...))))))))

    (define-syntax match-helper
      (lambda (expr)
	(syntax-case expr ()
	  ((_ original-expression value)
	   #'(error "match" (format "Match failure in form ~a" 'original-expression) value))
	  ((_ original-expression value (pattern body0 body ...) term ...)
	   #'(let ((match-helper-test-value (match1-or-fail value pattern body0 body ...)))
	       (if (not (eq? match-helper-test-value *shadchen-fail*))
		   match-helper-test-value
		   (match-helper original-expression value term ...)))))))

    (define-syntax match
      (lambda (expr)
	(syntax-case expr ()
	  ((_ value term ...)
	   #'(let ((v value)) (match-helper (match value term ...) v term ...))))))

    (define (shadchen-tests)
      (begin
      (define-syntax test (lambda (expr)
			    (syntax-case expr ()
			      ((_ name body0 body ...)
			       #'(let ((nm name)) (begin (test-begin nm)
							 body0 body ...
							 (test-end nm)))))))
      (test "match identifier"
	    (test-eq 10 (match 10 (x x))))
      (test "match list"
	    (test-equal '(z y x)
	      (match '(x y z) ((list a b c) (list c b a)))))
      (test "match number"
	    (test-equal 101
	      (match 101 ((number? x) x))))
      (test "fail to match list"
	    (test-error (match 10 ((list a b c) (list c b a)))))
      (test "match predicate no pattern"
	    (test-equal #t
	      (match '(x y z) ((list?) #t))))
      (test "match predicate with pattern"
	    (test-equal 'q
	      (match '(q r z) ((list? (list a b c)) a))))
      (test "match quote"
	    (test-equal #t
	      (match 'x
		('y #f)
		('x #t))))
      (test "transform solo macro"
	    (test-equal #t
	      (match-transform '(a b c) (x (car x) 'a) #t)))
      (test "transformation"
	    (test-equal #t
	      (match '(a b c) ((-> x (cadr x) 'b) #t))))
      (test "list*"
	    (test-equal '(d e f)
	      (match '(a b c d e f)
		((list* 'a 'b 'c r) r))))
      (test "let match"
	    (test-equal '(1 2 3 4)
	      (match 'who-cares ((let (x '(1 2 3 4))
				   (y '(4 5 6 7))) x))))
      (test "and pattern"
	    (test-equal '((a b) (c d e))
	      (match '(q c d e)
		((and (let (x '(a b)))
		      (list* _ r))
		 (list x r)))))
      (test "pattern bindings equal test"
	    (equal? #t (pattern-bindings-equal? '((list? o) (number? o)))))
      (test "or patterns"
	    (test-equal 10
	      (match 10
		((or
		  (number? o)
		  (list? o)) o))))
      (test "or patterns 2"
	    (test-equal 10
	      (match 10
		((or	     
		  (list? o)
		  (number? o)) o))))
      (test "or patterns 3"
	    (test-equal 10
	      (match 10
		((or	     
		  (list? o)
		  (number? o)
		  (string? o)) o))))
      (test "call"
	    (test-equal 11
	      (match 10 ((call (lambda (x) (+ x 1)) y) y))))
      (test "hashtable-ref"
	    (test-equal "hello world"
	      (match (let ((t (make-eq-hashtable)))
		       (hashtable-set! t 'x "hello world")
		       t)
		((hashtable-ref x) x))))
      (test "hashtable-ref w/ pattern"
	    (test-equal '(d e f)
	      (match (let ((t (make-eq-hashtable)))
		       (hashtable-set! t 'x '(a b c d e f))
		       t)
		((hashtable-ref x (list* _ _ _ r)) r))))

      (test "hashtable-ref-or"
	    (test-equal "hello bad world"
	      (match (let ((t (make-eq-hashtable)))
		       (hashtable-set! t 'x "hello world")
		       t)
		((hashtable-ref-or y "hello bad world") y))))
      (test "hashtable-ref w/ pattern"
	    (test-equal '(d e f)
	      (match (let ((t (make-eq-hashtable)))
		       (hashtable-set! t 'x '(a b c x e f))
		       t)
		((hashtable-ref-or y '(a b c d e f) (list* _ _ _ r)) r))))
      (test "or + and as a way of binding defaults"
	    (test-equal (list 1 2 3 10)
	      (match (list 1 2 3)
		((or (list a b c d)
		     (and (list a b c)
			  (let (d 10))))
		 (list a b c d)))))
      (test "or and and test binding"
	    (test-equal 'exit
	      (match 'exit
		((and kw
		      (or 'exit 'enter 'update))
		 kw))))
      (test "or and and test binding in list*"
	    (test-equal 'exit
	      (match '(exit)
		((list* (and kw
			     (or 'exit 'enter 'update)) rest)
		 kw))))))))
