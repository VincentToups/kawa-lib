(define-library (lib mecs-omnibus)
  (export
   mecs
   component
   entity
   system
   make-mecs
   mecs-entities
   mecs-entities-query
   mecs-components
   mecs-component-name-map
   make-entity
   mecs-get-entities-from
   mecs-get-tracked-system-entities
   mecs-get-tracked-system
   mecs-update-system
   mecs-entities-count
   entity-mask
   entity-components
   entity-components-vector-ref
   entity-add-component!
   entity-add-components!
   entity-delete-components!
   entity-delete-component!
   entity-get-component
   entity-has-component?
   entity-component
   entity-set-component!
   entity-component-set!
   entity-empty?
   delete-entity!
   entity-mecs
   non-empty-entity?
   entity?
   collect-systems
   make-system
   system?
   system-mask
   entity-in-system?
   for-each-with-components
   map-with-components
   with-components
   ;value-component
   mecs-tests
   lookup-tracked-system)
  (import
    (except (kawa base) match test-begin system)
    (rename (only (kawa base) bitwise-ior) (bitwise-ior logor))
    (srfi 1)
    (srfi 64)
    (rnrs hashtables (6))
    (lib shadchen)
    (lib mecs-syntax)
    (lib mecs-protocols)
    (lib mecs-model)
    (only (lib util) list-of-symbols?))  
  (begin

    (define-syntax chain (lambda (expr)
			 (syntax-case expr ()
			   ((_ name form) #'form)
			   ((_ name form0 form1 ...)
			    #'(let ((name #f))
				(set! name form0)
				(set! name form1)
				...
				name)))))


    
    (define *end* -1)
  
    (define *empty* (list '*empty*))

  (define (tracked-system-remove-entity s e)
    (hashtable-delete! (tracked-system-entities s) e)
    (when (tracked-system-exit s)
      ((tracked-system-exit s) e))
    e)

  (define (tracked-system-add-entity s e)
    (hashtable-set! (tracked-system-entities s) e e)
    (when (tracked-system-enter s)
      ((tracked-system-enter s) e))
    e)

  (define (mecs-get-tracked-system m s-name)
    (chain v_
	   (mecs-system-name-map m)
	   (hashtable-ref v_ s-name #f)
	   (if (not v_) (error "mecs-get-tracked-system" "Unknown system" s-name) v_)
	   (vector-ref (mecs-systems m) v_)))

  (define (mecs-get-tracked-system-entities m s-name)
    (chain v_
	   (mecs-get-tracked-system m s-name)
	   (hashtable-keys (tracked-system-entities v_))))

  (define (mecs-update-system m s-name)
    (let* ((s (mecs-get-tracked-system m s-name))
	   (update (tracked-system-update s)))
      (vector-for-each update (hashtable-keys (tracked-system-entities s)))))

  (define mecs-get-entities-from mecs-get-tracked-system-entities)

  (define (mecs-get-tracked-system-entities-list m s-name)
    (vector->list (mecs-get-tracked-system-entities m s-name)))

  (define (entity-components-vector-ref entity i)
    (chain v_  (entity-components entity)
	   (vector-ref v_ i)))

  (define (entity-components-vector-set! entity i v)
    (chain v_ (entity-components entity)
	   (begin (vector-set! v_ i v)
		  entity)))

  (define (entity-mask-logor! entity n)
    (chain v_ (entity-mask entity)
	   (begin
	     (entity-mask-set! entity (logor v_ n))
	     entity)
	   entity))
  
  (define (mecs-entities-count the-mecs)
    (vector-length (hashtable-keys (mecs-entities the-mecs))))

  (define (mecs-entities-query the-mecs filtration)
    (cond
     ((system? filtration)
      (let ((out '()))
	(vector-for-each
	 (lambda (e)
	   (when (entity-in-system-raw? e filtration)
	     (set! out (cons e out))))
	 (hashtable-keys (mecs-entities the-mecs)))
	out))
     ((symbol? filtration)
      (mecs-entities-query the-mecs (mecs-get-tracked-system the-mecs filtration)))
     ((list-of-symbols? filtration)
      (mecs-entities-query the-mecs (apply make-system (cons the-mecs filtration))))
     ((procedure? filtration)
      (let ((out '()))
	(vector-for-each
	 (lambda (e)
	   (when (filtration e)
	     (set! out (cons e out))))
	 (hashtable-keys (mecs-entities the-mecs)))
	out))
     (else (error "mecs-entities-query" "Can't query with irritant (which should be a function, a mecs-system, a list of symbols or a symbol denoting a tracked system." filtration))))

 (define (mecs-add-entity-to-entities! the-mecs entity)
   (let ((entities-table (mecs-entities the-mecs)))
     (when (not (hashtable-contains? entities-table entity))
       (hashtable-set! entities-table entity entity)
       entity)))

 (define (mecs-delete-entity-from-entities! the-mecs entity)
   (let ((entities-table (mecs-entities the-mecs)))
     (when (hashtable-contains? entities-table entity)
       (hashtable-delete! entities-table entity))))

 (define (mecs-list-component-names m)
   (vector->list (hashtable-keys (mecs-component-name-map m))))

 (define (delete-entity! e)
   (apply entity-delete-components!
	  (cons e (filter (lambda (c-name)
			    (entity-has-component? e c-name))
			  (mecs-list-component-names (entity-mecs e)))))
   (mecs-delete-entity-from-entities! (entity-mecs e) e))

  (define (mecs-component-index the-mecs component-name)
    (chain v_ (mecs-component-name-map the-mecs)
	   (hashtable-ref v_ component-name -1)
	   (if (not (= -1 v_))
	       v_
	       (error "mecs-component-index" "Mecs world has no such component." the-mecs component-name))))

  (define (mecs-component-mask the-mecs component-name)
    (chain v_ (mecs-component the-mecs component-name)
	   (component-mask v_)))

  (define (entity-component-mask entity component-name)
    (chain v_ (entity-mecs entity)
	   (mecs-component-mask v_ component-name)))

  (define (entity-component-index entity component-name)
    (chain v_ (entity-mecs entity)
	   (mecs-component-index v_ component-name)))

  (define (entity-has-component?* entity component-name)
    (let* ((ci (entity-component-index entity component-name))
	   (c? (entity-components-vector-ref entity ci)))
      (if (not (eq? *empty* c?)) c? #f)))

  (define (entity-get-component entity component-name)
    (let ((x (entity-has-component?* entity component-name)))
      (if x x (error "entity-get-component" (format "Entity does not contain the component ~a." component-name) entity component-name))))

  (define entity-component entity-get-component)

  (define (entity-set-component! entity component-name value)
    (let ((x (entity-has-component?* entity component-name)))
      (if x (entity-components-vector-set! entity (entity-component-index entity component-name) value)
	  (error "entity-set-component!" (format "Entity does not contain the component ~a." component-name) entity component-name))))

  (define entity-component-set! entity-set-component!)
  
  (define (entity-has-component? entity component-name)
    (let ((r (entity-has-component?* entity component-name)))
      (if r #t #f)))

  (define (entity-empty? entity)
    (= 0 (entity-mask entity)))

  (define (non-empty-entity? thing)
    (and (entity? thing)
	 (not (entity-empty? thing))))

  (define (entity-add-component! entity component-name arguments)
    (let ((ci (entity-component-index entity component-name))
	  (m (entity-component-mask entity component-name)))
      (chain v_ (entity-mecs entity)
	     (mecs-component v_ component-name)
	     (component-constructor v_)
	     (apply v_ (cons entity arguments))
	     (entity-components-vector-set! entity ci v_))
      (entity-mask-logor! entity m)))

  (define (entity-delete-component-raw! entity component-name)
    (if (not (symbol? component-name))
	(error "entity-delete-component-raw!" "Component name must be a symbol." component-name))
    (let ((ci (entity-component-index entity component-name))
	  (m (entity-component-mask entity component-name)))
      (entity-components-vector-set! entity ci *empty*)
      (entity-mask-set! entity (logand (entity-mask entity) (lognot m)))))

  (define (entity-delete-components-raw! entity . components)
    (map
     (lambda (component)
       (entity-delete-component-raw! entity component))
     components)
    entity)

  (define (get-component-masks the-mecs clist)
    (map (lambda (cname)
	   (chain v_ (mecs-component the-mecs cname)
		  (component-mask v_)))
	 clist))
  
  (define (calculate-system-deltas an-entity added-components deleted-components systems)
    (let* ((m (entity-mecs an-entity))
	   (mask (entity-mask an-entity))
	   (added-masks (apply logor (get-component-masks m added-components)))
	   (removed-masks (apply logor (get-component-masks m deleted-components)))
	   (mask-w/-added (logor mask added-masks))
	   (mask-w/-removed (logand mask-w/-added (lognot removed-masks))))
      (vector-map (lambda (system)
		    (let ((before (mask-in-system? mask system))
			  (after (mask-in-system? mask-w/-removed system)))
		      (cond
		       ((and before (not after))
			'exit)
		       ((and (not before) after)
			'enter)
		       (else 'no-change))))
		  systems)))

  (define (entity-delete-components! entity . components)
    (let* ((systems (mecs-systems (entity-mecs entity)))
	   (deltas (calculate-system-deltas entity (list) components
					    (mecs-systems (entity-mecs entity)))))
      (vector-map (lambda (status system)
		    (when (eq? status 'exit)
		      (tracked-system-remove-entity system entity)))
		  deltas systems)
      (apply entity-delete-components-raw! entity components)
      (if (= 0 (entity-mask entity))
	  (mecs-delete-entity-from-entities! (entity-mecs entity) entity))
      entity))

  (define (entity-delete-component! entity component)
    (entity-delete-components! entity component))

  (define (entity-in-system-raw? entity system)
    (let ((em (entity-mask entity))
	  (sm (system-mask system)))
      (= sm (logand em sm))))

  (define (mask-in-system? mask system)
    (let ((sm (system-mask system)))
      (= sm (logand mask sm))))

  (define (lookup-tracked-system m name)
    (chain v_ (mecs-system-name-map m)
	   (hashtable-ref v_ name #f)
	   (if v_ (vector-ref (mecs-systems m) v_)
	       (error "lookup-tracked-system" (format "No tracked system named ~a" name) name))))

  (define (entity-in-system? entity system)
    (cond ((symbol? system)
	   (entity-in-system-raw? entity (lookup-tracked-system (entity-mecs entity) system)))
	  ((system? system)
	   (entity-in-system-raw? entity system))
	  (else (error "entity-in-system?" "system is neither a symbol nor a bonafide system." system))))

  (define-syntax vector-iterate (lambda (expr)
				  (syntax-case expr ()
				    ((_ (value-name index-name the-vector) body0 body ...)
				     #'(let* ((v the-vector)
					      (n (vector-length v)))
					 (let loop ((index-name 0))
					   (cond ((< index-name n)
						  (let ((value-name (vector-ref v index-name)))
						    body0 body ...)
						  (loop (+ index-name 1)))
						 (else 'done))))))))

  ;; (define-syntax when (lambda (expr)
  ;; 			(syntax-case expr ()
  ;; 			  ((_ condition body0 body ...)
  ;; 			   #'(if condition (begin body0 body ...) #f)))))

  (define (make-entity the-mecs . components)
    (let* ((n-comps-in-world (vector-length (mecs-components the-mecs))))      
      (let ((entity (make-entity-raw the-mecs
				     (make-vector n-comps-in-world *empty*)
				     0
				     0)))
	(apply entity-add-components! (cons entity components))
	(when (not (entity-empty? entity))
	  (mecs-add-entity-to-entities! the-mecs entity)))))

  (define (entity-add-components-raw! entity . components)
    (let loop ((rest components))
      (match rest
	((list)
	 entity)
	((list* (list* (symbol? component-name) args) rest)
	 (entity-add-component! entity component-name args)
	 (loop rest)))))

  (define (entity-add-components! entity . components)
    (let* ((tracked-systems (mecs-systems (entity-mecs entity)))
	   (before (vector-map (lambda (s) (entity-in-system? entity s)) tracked-systems))
	   (_ (apply entity-add-components-raw! (cons entity components)))
	   (after (vector-map (lambda (s) (entity-in-system? entity s)) tracked-systems)))
      (vector-map (lambda (b a s)
		    (cond
		     ((and b (not a))
		      (tracked-system-remove-entity s entity))
		     ((and (not b) a)
		      (tracked-system-add-entity s entity))))
		  before after tracked-systems)
      entity))

  (define (displayf . args)
    (display (apply format args)))

  (define-syntax
    with-components
    (lambda (expr)
      (define (regularize-binding-form form)
	(syntax-case form ()
	    ((binding component-name)
	     (and (identifier? #'binding)
		  (identifier? #'component-name))
	     #'(binding (quote component-name)))
	    ((component-name)
	     (identifier? #'component-name)
	     #'(component-name (quote component-name)))
	    (component-name
	     (identifier? #'component-name)
	     #'(component-name (quote component-name)))))
      (syntax-case expr ()
	((_ (entity binding-form ...) body0 body ...)
	 (with-syntax ((((binding component-name) ...)
			(map regularize-binding-form #'(binding-form ...))))
	   #'(let ((entity-value entity))
	       (let ((binding (entity-get-component entity-value component-name))
		     ...)
		 body0 body ...)))))))

  (define-syntax for-each-with-components
    (lambda (expr)
      (syntax-case expr ()
	((_ entity-list (entity-name binding-form ...) body0 body ...)
	 (identifier? #'entity-name)
	 #'(for-each (lambda (entity-name)
		       (with-components
			(entity-name binding-form ...)
			body0 body ...))
		     entity-list)))))

    (define-syntax map-with-components
      (lambda (expr)
	(syntax-case expr ()
	((_ entity-list (entity-name binding-form ...) body0 body ...)
	 (identifier? #'entity-name)
	 #'(map (lambda (entity-name)
		       (with-components
			(entity-name binding-form ...)
			body0 body ...))
		entity-list)))))
  
    (define (collect-systems the-mecs . systems)
      (define (collect-system-info mecs systems)
	(let ((out (make-vector (length systems))))
	  (let loop ((rest systems)
		     (indexes (list))
		     (i 0))
	    (match rest
	      ((list) (list out (list->vector systems) (list->vector indexes)))
	      ((list* (symbol? tracked-system-name) rest)
	       (vector-set! out i (mecs-get-tracked-system-entities-list mecs tracked-system-name))
	       (loop rest indexes (+ i 1)))
	      ((list* (? system? s) rest)
	       (vector-set! out i (list))
	       (loop rest (cons i indexes) (+ i 1)))))))
      (let ((entities (hashtable-keys (mecs-entities the-mecs))))
	(match (collect-system-info the-mecs systems)
	  ((list output systems-vector index-vector)
	   (vector-for-each (lambda (entity)
			      (vector-for-each
			       (lambda (index)
				 (when (entity-in-system? entity (vector-ref systems-vector index))
				   (vector-set! output index (cons entity (vector-ref output index))))) index-vector))
			    entities)
	   (vector->list output)))))

  ;; (define-syntax test (lambda (expr)
  ;; 			(syntax-case (expr) ()
  ;; 			  ((_ test-name body0 body ...)
  ;; 			   #'#f;; (if (eq? #t (begin body0 body ...))
  ;; 			       ;; 	 (display (format "... : ~a: success" test-name))
  ;; 			       ;; 	 (display (format "!!! : ~a: failure" test-name)))
  ;; 			   ))))

  ;; (define (test test-name value)
  ;;   (if (eq? #t value)
  ;; 	(display (format "... : ~a : success\n" test-name))
  ;; 	(display (format "!!! : ~a : failure\n" test-name))))
    
  (define (mecs-tests)
    (import (srfi :64))
    (define-syntax test (lambda (expr)
			  (syntax-case expr ()
			    ((_ name body0 body ...)
			     #'(let ((nm name)) (begin (test-begin nm)
						       body0 body ...
						       (test-end nm)))))))
    (test "Basic system functionality."
	  (let* ((m (make-mecs
			       (list 'hp (value-component)
				     'mp (value-component)
				     'max-hp (value-component))))
		 (bounded-hp (make-system m 'hp 'max-hp))
		 (magical (make-system m 'mp)))
	    (make-entity m
			 (list 'hp 10)
			 (list 'max-hp 5)
			 (list 'mp 10))
	    (let* ((collections (collect-systems m bounded-hp magical))
		   (bhp (car collections))
		   (m (cadr collections)))
	      (test-equal 1 (length bhp))
	      (test-equal 1 (length m))
	      (test-equal #t (entity-has-component? (car bhp) 'hp))
	      (test-equal #t (entity-has-component? (car bhp) 'max-hp))
	      (test-equal #t (entity-has-component? (car m) 'mp)))))
    (test "Empty system functionality."
	  (let* ((m (make-mecs
			       (list 'hp (value-component)
				     'mp (value-component)
				     'max-hp (value-component))))
		 (bounded-hp (make-system m 'hp 'max-hp))
		 (magical (make-system m 'mp)))
	    (make-entity m
			 (list 'hp 10)
			 (list 'max-hp 5))
	    (let* ((collections (collect-systems m bounded-hp magical))
		   (bhp (car collections))
		   (m (cadr collections)))
	      (test-equal 1 (length bhp))
	      (test-equal 0 (length m))
	      (test-equal #t (entity-has-component? (car bhp) 'hp))
	      (test-equal #t (entity-has-component? (car bhp) 'max-hp))
	      (test-equal #t (not (entity-has-component? (car bhp) 'mp))))))
    (test "Entity-count"
	  (let* ((m (make-mecs
			       (list
				'hp (value-component)
				'mp (value-component)
				'max-hp (value-component))))
		 (bounded-hp (make-system m 'hp 'max-hp))
		 (magical (make-system m 'mp)))
	    (make-entity m
			 (list 'hp 10)
			 (list 'max-hp 5)
			 (list 'mp 10)) 
	    (make-entity m (list 'hp 10))
	    (make-entity m (list 'mp 25))
	    (make-entity m (list 'hp 10) (list 'max-hp 15))
	    (make-entity m (list 'hp 13) (list 'max-hp 15))
	    (make-entity m (list 'hp 10) (list 'max-hp 15) (list 'mp 3))
	    (displayf "entity-count ~a\n" (mecs-entities-count m))
	    (test-equal 6 (mecs-entities-count m))))
    (test "Repeated collections"
	  (let* ((m (make-mecs
			       (list
				'hp (value-component)
				'mp (value-component)
				'max-hp (value-component))))
		 (bounded-hp (make-system m 'hp 'max-hp))
		 (magical (make-system m 'mp)))
	    (make-entity m
			 (list 'hp 10)
			 (list 'max-hp 5)
			 (list 'mp 10)) 
	    (make-entity m (list 'hp 10))
	    (make-entity m (list 'mp 25))
	    (make-entity m (list 'hp 10) (list 'max-hp 15))
	    (make-entity m (list 'hp 13) (list 'max-hp 15))
	    (make-entity m (list 'hp 10) (list 'max-hp 15) (list 'mp 3))
	    (let* ((collections (collect-systems m bounded-hp magical))
		   (collections (collect-systems m bounded-hp magical))
		   (bhp (car collections))
		   (m (cadr collections)))
	      (displayf "bhp: ~a, m ~a\n" (length bhp) (length m))
	      (test-equal 4 (length bhp))
	      (test-equal 3 (length m))
	      (test-equal #t (entity-has-component? (car bhp) 'hp))
	      (test-equal #t (entity-has-component? (car bhp) 'max-hp))
	      (test-equal #t (entity-has-component? (car m) 'mp)))))
    (test "Adding components after creation."
	  (let* ((m (make-mecs
			       (list
				'hp (value-component)
				'mp (value-component)
				'max-hp (value-component))))
		 (bounded-hp (make-system m 'hp 'max-hp))
		 (magical (make-system m 'mp))
		 (e1 (make-entity m (list 'hp 10))))
	    (make-entity m
			 (list 'hp 10)
			 (list 'max-hp 5)
			 (list 'mp 10)) 
	    (make-entity m (list 'hp 10))
	    (make-entity m (list 'mp 25))
	    (make-entity m (list 'hp 13) (list 'max-hp 15))
	    (make-entity m (list 'hp 10) (list 'max-hp 15) (list 'mp 3))
	    (entity-add-components! e1 (list 'max-hp 10))
	    (let* ((collections (collect-systems m bounded-hp magical))
		   (collections (collect-systems m bounded-hp magical))
		   (bhp (car collections))
		   (m (cadr collections)))
	      (displayf "bhp: ~a, m ~a\n" (length bhp) (length m))
	      (test-equal 4 (length bhp))
	      (test-equal 3 (length m))
	      (test-equal #t (entity-has-component? (car bhp) 'hp))
	      (test-equal #t (entity-has-component? (car bhp) 'max-hp))
	      (test-equal #t (entity-has-component? (car m) 'mp)))))
    (test "with-components"
	  (let* ((m (make-mecs
			       (list
				'hp (value-component)
				'mp (value-component)
				'max-hp (value-component))))
		 (bounded-hp (make-system m 'hp 'max-hp))
		 (magical (make-system m 'mp))
		 (e1 (make-entity m (list 'hp 13))))
	    (make-entity m
			 (list 'hp 10)
			 (list 'max-hp 5)
			 (list 'mp 10)) 
	    (make-entity m (list 'hp 10))
	    (make-entity m (list 'mp 25))
	    (make-entity m (list 'hp 13) (list 'max-hp 15))
	    (make-entity m (list 'hp 10) (list 'max-hp 15) (list 'mp 3))
	    (entity-add-components! e1 (list 'max-hp 11))
	    (with-components
	     (e1 hp max-hp)
	     (test-equal hp 13)
	     (test-equal max-hp 11))))
    (test "for-each-with-components"
	  (let* ((m (make-mecs
			       (list
				'hp (value-component)
				'mp (value-component)
				'max-hp (value-component))))
		 (bounded-hp (make-system m 'hp 'max-hp))
		 (magical (make-system m 'mp))
		 (e1 (make-entity m (list 'hp 13))))
	    (make-entity m
			 (list 'hp 10)
			 (list 'max-hp 5)
			 (list 'mp 10)) 
	    (make-entity m (list 'hp 10))
	    (make-entity m (list 'mp 25))
	    (make-entity m (list 'hp 13) (list 'max-hp 15))
	    (make-entity m (list 'hp 10) (list 'max-hp 15) (list 'mp 3))
	    (entity-add-components! e1 (list 'max-hp 11))
	    (let ((entity-count 0))
	      (for-each-with-components
	       (car (collect-systems m bounded-hp)) (e (h hp) (m max-hp))
	       (set! entity-count (+ 1 entity-count)))
	      (test-equal entity-count 4))))
    (test "for-each-with-components-collecting-values"
	  (let* ((m (make-mecs
			       (list
				'hp (value-component)
				'mp (value-component)
				'max-hp (value-component))))
		 (bounded-hp (make-system m 'hp 'max-hp))
		 (magical (make-system m 'mp))
		 (e1 (make-entity m (list 'hp 13))))
	    (make-entity m
			 (list 'hp 10)
			 (list 'max-hp 5)
			 (list 'mp 10)) 
	    (make-entity m (list 'hp 10))
	    (make-entity m (list 'mp 25))
	    (make-entity m (list 'hp 13) (list 'max-hp 15))
	    (make-entity m (list 'hp 10) (list 'max-hp 15) (list 'mp 3))
	    (entity-add-components! e1 (list 'max-hp 11))
	    (let ((entities (list)))
	      (for-each-with-components
	       (car (collect-systems m bounded-hp)) (e (h hp) (m max-hp))
	       (set! entities (cons (list h m) entities)))
	      (test-equal (length entities) 4))))
    (test "for-each-with-components-collecting-values but no aliasing"
	  (let* ((m (make-mecs
			       (list 'hp (value-component)
				     'mp (value-component)
				     'max-hp (value-component))))
		 (bounded-hp (make-system m 'hp 'max-hp))
		 (magical (make-system m 'mp))
		 (e1 (make-entity m (list 'hp 13))))
	    (make-entity m
			 (list 'hp 10)
			 (list 'max-hp 5)
			 (list 'mp 10)) 
	    (make-entity m (list 'hp 10))
	    (make-entity m (list 'mp 25))
	    (make-entity m (list 'hp 13) (list 'max-hp 15))
	    (make-entity m (list 'hp 10) (list 'max-hp 15) (list 'mp 3))
	    (entity-add-components! e1 (list 'max-hp 11))
	    (let ((entities (list)))
	      (for-each-with-components
	       (car (collect-systems m bounded-hp)) (e hp max-hp)
	       (set! entities (cons (list hp max-hp) entities)))
	      (test-equal (length entities) 4))))
    (test "basic tracked systems test"
	  (let* ((m (make-mecs
			       (list 'hp (value-component)
				     'mp (value-component)
				     'max-hp (value-component))
			       (list 'bounded-hp
				     '(hp max-hp)
				     'magical
				     '(mp))))
		 (e1 (make-entity m (list 'hp 13))))
	    (make-entity m
			 (list 'hp 10)
			 (list 'max-hp 5)
			 (list 'mp 10)) 
	    (make-entity m (list 'hp 10))
	    (make-entity m (list 'mp 25))
	    (make-entity m (list 'hp 13) (list 'max-hp 15))
	    (make-entity m (list 'hp 10) (list 'max-hp 15) (list 'mp 3))
	    (entity-add-components! e1 (list 'max-hp 11))
	    (let ((entities (mecs-get-tracked-system-entities-list m 'bounded-hp)))
	      (test-equal (length entities) 4))))
    (test "basic tracked systems test w/ component deletion"
	  (let* ((m (make-mecs
			       (list 'hp (value-component)
				     'mp (value-component)
				     'max-hp (value-component))
			       (list 'bounded-hp
				     '(hp max-hp)
				     'magical
				     '(mp))))
		 (e1 (make-entity m (list 'hp 13))))
	    (make-entity m
			 (list 'hp 10)
			 (list 'max-hp 5)
			 (list 'mp 10)) 
	    (make-entity m (list 'hp 10))
	    (make-entity m (list 'mp 25))
	    (make-entity m (list 'hp 13) (list 'max-hp 15))
	    (make-entity m (list 'hp 10) (list 'max-hp 15) (list 'mp 3))
	    (entity-add-components! e1 (list 'max-hp 11))
	    (entity-delete-components! e1 'max-hp)
	    (let ((entities (mecs-get-tracked-system-entities-list m 'bounded-hp)))
	      (test-equal (length entities) 3))))
    (test "exit method called before component removed"
	  (let* ((success #f)
		 (m (make-mecs
			       (list 'hp (value-component)
				     'mp (value-component)
				     'max-hp (value-component))
			       (list 'bounded-hp
				     '(hp max-hp)
				     (list 'exit (lambda (e)
					      (if (entity-has-component? e 'max-hp)
						  (set! success #t)
						  (set! success #f))))
				     'magical
				     '(mp))))
		 (e1 (make-entity m (list 'hp 13))))
	    (make-entity m
			 (list 'hp 10)
			 (list 'max-hp 5)
			 (list 'mp 10)) 
	    (make-entity m (list 'hp 10))
	    (make-entity m (list 'mp 25))
	    (make-entity m (list 'hp 13) (list 'max-hp 15))
	    (make-entity m (list 'hp 10) (list 'max-hp 15) (list 'mp 3))
	    (entity-add-components! e1 (list 'max-hp 11))
	    (entity-delete-components! e1 'max-hp)
	    (test-equal success #t)))
    (test "exit method called after wholesale delete"
	  (let* ((success #f)
		 (m (make-mecs
			       (list 'hp (value-component)
				     'mp (value-component)
				     'max-hp (value-component))
			       (list 'bounded-hp
				     '(hp max-hp)
				     (list 'exit (lambda (e)
					      (if (entity-has-component? e 'max-hp)
						  (set! success #t)
						  (set! success #f))))
				     'magical
				     '(mp))))
		 (e1 (make-entity m (list 'hp 13))))
	    (make-entity m
			 (list 'hp 10)
			 (list 'max-hp 5)
			 (list 'mp 10)) 
	    (make-entity m (list 'hp 10))
	    (make-entity m (list 'mp 25))
	    (make-entity m (list 'hp 13) (list 'max-hp 15))
	    (make-entity m (list 'hp 10) (list 'max-hp 15) (list 'mp 3))
	    (entity-add-components! e1 (list 'max-hp 11))
	    (delete-entity! e1)
	    (test-equal success #t))))
  ))
