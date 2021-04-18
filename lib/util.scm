(define-library (lib util)
  (export
   ..
   all-satisfy?
   assert
   between-inclusive?
   chain
   dbg
   dsp
   empty?
   fland
   flor
   get-class-loader
   indexed-for-each
   interactive-select
   java-null?
   lambda-named
   list-empty?
   list-of-numbers?
   list-of-symbols?
   modulo=0
   not-java-null?
   order-table
   pi
   plist-get
   plist-get-or-error
   print-tabulation
   random-float
   random-int
   repeat
   repeat-accumulate
   table
   table$
   tabulate)
  (import (except (kawa base) match)
	  (rnrs hashtables (6))
	  (rnrs sorting (6))
	  (only (srfi 1) fold)
	  (lib shadchen)
	  (class java.io IOException File)
	  (class java.net URLClassLoader URL)
	  (class java.lang.reflect Method)
	  (class java.lang Thread))
  (begin

    (define-syntax lambda-named
      (lambda (expr)
	(syntax-case expr ()
	  ((_ name (arg ...) body ...)
	   (identifier? #'name)
	   #'(begin
	       (define (name arg ...)
		 body ...)
	       name)))))

    (define pi 3.1415926535)

    (define-syntax ..
      (lambda (expr)
	(syntax-case expr ()
	  ((_ e) #'e)
	  ((_ e property-name rest ...)
	   (identifier? #'property-name)
	   #'(.. e:property-name rest ...))
	  ((_ e (method-name arg ...) rest ...)
	   (identifier? #'method-name)
	   #'(.. (e:method-name arg ...) rest ...)))))

    (define-syntax chain
      (lambda (expr)
	(syntax-case expr ()
	  ((_ id expr0 expr ...)
	   (identifier? #'id)
	   #'(let ((id expr0))
	       (set! id expr)
	       ...
	       id)))))

    (define (java-null? o)
      (eq? #!null o))
    (define (not-java-null? o)
      (not (java-null? o)))

    (define (table . args)
      (let ((tbl (make-eq-hashtable)))
	(let loop ((args args))
	  (match args
	    ((list) tbl)
	    ((list _) (error "table" (format "table requires an even number of arguments but we had a trailing singlet: ~a" _) _))
	    ((list* key value rest)
	     (hashtable-set! tbl key value)
	     (loop rest))))))

    (define (order-table . symbols)
      (let ((htbl (make-eq-hashtable)))
	(let loop ((i 0)
		   (ss symbols))
	  (cond
	   ((empty? ss) htbl)
	   (else
	    (hashtable-set! htbl (car ss) i)
	    (loop (+ i 1)
		  (cdr ss)))))))

    (define-syntax table$
      (lambda (expr)
	(define (transform tbl-stx stx ac)
	  (syntax-case stx ()
	    (() ac)
	    ((id expr rest ...)
	     (identifier? #'id)
	     (transform tbl-stx
			#'(rest ...)
			(with-syntax ((tbl-stx- tbl-stx)
				      ((ac0 ...) ac))
			  #'(ac0 ... (hashtable-set! tbl-stx- (quote id) expr)))))
	    (other-syntax #'(error "table$" "Can't expand table$ syntax, which must be alternating symbols and values." 'other-syntax))))
	(syntax-case expr ()
	  ((_ form ...)
	   (with-syntax ((tbl-stx #'tbl))
	     (with-syntax (((setter-form ...)
			    (transform #'tbl-stx #'(form ...) #'())))
	       #'(let ((tbl-stx (make-eq-hashtable)))
		   setter-form
		   ...
		   tbl-stx)))))))


    (define-syntax assert
      (lambda (expr)
	(syntax-case expr ()
	  ((_ expr location message irritant ...)
	   #'(let ((v expr))
	       (when (not v)
		 (error location message (quote expr) v irritant ...))))
	  ((_ expr location)
	   #'(assert expr location "Assertion failed.")))))

    (define a-random (java.util.Random))

    (define (random-float)
      (a-random:nextDouble))

    (define (random-int from to)
      (+ from (a-random:nextInt (+ to 1))))

    (define (between-inclusive? n a b)
      (and (>= n a)
	   (<= n b)))

    (define (modulo=0 n m)
      (= 0 (modulo n m)))

    (define (plist-get l s alt)    
      (let ((sl (memq s l)))
	(if sl (cadr sl) alt)))
    
    (define (plist-get-or-error l s)
      (let* ((alt (list 'what))
	     (r (plist-get l s alt)))
	(if (eq? r alt)
	    (error "plist-get-or-error" "Key not found." l s)
	    r)))

    (define (empty? l)
      (cond ((vector? l) (= 0 (vector-length l)))
	    ((list? l) (= 0 (length l)))))

    (define (all-satisfy? l p)
      (cond ((empty? l) #t)
	    ((p (car l)) (all-satisfy? (cdr l) p))
	    (else #f)))

    (define (list-of-numbers? l)
      (all-satisfy? l number?))

    (define (list-of-symbols? l)
      (all-satisfy? l symbol?))

    (define (list-empty? l)
      (eq? l '()))

    (define (flor . functions)
      (lambda args
	(let loop ((fs functions))
	  (cond ((eq? fs '()) #f)
		(else
		 (let ((r (apply (car fs) args)))
		   (if r r (loop (cdr fs)))))))))

    (define (fland . functions)
      (lambda args
	(let loop ((fs functions))
	  (cond ((eq? fs '()) #t)
		(else
		 (let ((r (apply (car fs) args)))
		   (if (not r) r (loop (cdr fs)))))))))

    (define (dsp . args)
      (display (apply format args))
      (newline))

    (define (dbg . args)
      (match args
	((list) 'done)
	((list* name val rest)
	 (display (format "~a ~a\n" name val))
	 (apply dbg rest))))

    (define (print-tabulation tabulation fill-column)
      (let* ((total (fold (lambda (it ac)
			    (+ (cadr it) ac))
			  0
			  tabulation))
	     (max-key-length (fold (lambda (it ac)
				     (let ((l (string-length (format "~a (~a)" (car it) (cadr it)))))
				       (if (> l ac) l ac)))
				   -1
				   tabulation))
	     (mk (lambda ()
		   (make-string max-key-length)))
	     (padded-keys
	      (map (lambda (k)
		     (let ((ss (format "~a (~a)" (car k) (cadr k)))
			   (i 0)
			   (out (mk)))
		       (string-for-each
			(lambda (c)
			  (string-set! out i c)
			  (set! i (+ i 1)))
			ss)
		       out))
		   tabulation))
	     (count-reps (map (lambda (entry)
				(let ((normed (round (* fill-column (/ (cadr entry) total)))))
				  (make-string normed #\*)))
			      tabulation)))
	(for-each (lambda (pk cnt)
		    (display (format "~a : ~a\n" pk cnt)))
		  padded-keys count-reps)))

    (define (indexed-for-each f l)
      (let ((i 0))
	(for-each (lambda (el)
		    (f el i)
		    (set! i (+ i 1)))
		  l))
      (if #f #t))

    (define (tabulate lst)
      (if (vector? lst)
	  (tabulate (vector->list lst))
	  (let ((tmp (make-hashtable equal-hash equal?))
		(key-order (make-hashtable equal-hash equal?))
		(index 0))
	    (for-each (lambda (el)
			(when (not (hashtable-contains? tmp el))
			  (hashtable-set! key-order el index)
			  (set! index (+ 1 index)))
			(hashtable-update! tmp el
					   (lambda (v) (+ v 1))
					   0))
		      lst)
	    (list-sort
	     (lambda (a b)
	       (< (hashtable-ref key-order (car a) 0)
		  (hashtable-ref key-order (car b) 0)))
	     (map (lambda (k) (list k (hashtable-ref tmp k 0)))
		  (vector->list (hashtable-keys tmp)))))))

    (define (repeat-accumulate f n)
      (let ((out (make-vector n))
	    (i 0))
	(repeat (lambda ()
		  (vector-set! out i (f))
		  (set! i (+ 1 i)))
		n)
	(vector->list out)))

    (define (get-class-loader . things)
      (let ((class-loader (.. Thread (currentThread) (getContextClassLoader))))
	class-loader))

    (define (repeat f n)
      (cond ((<= n 0) 'done)
	    (else
	     (f)
	     (repeat f (- n 1)))))

    (define (interactive-select options)
      (cond
       ((eq? options '()) #f)
       ((eq? '() (cdr options))
	(car options))
       (else
	(display "Select One: ") (newline)
	(indexed-for-each (lambda (opt i)
			    (display (format "~a : ~a\n" i opt)))
			  options)
	(display ":: ")
	(let ((n (read)))
	  (cond
	   ((and (number? n)
		 (between-inclusive? n 0 (- (length options) 1)))
	    (list-ref options n))
	   (else
	    (display "Bad choice!!")
	    (newline)
	    (interactive-select options)))))))))

