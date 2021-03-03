(define-library (lib mecs-model)
  (export mecs entity component system tracked-system make-mecs mecs? mecs-entities mecs-entities-set! mecs-components mecs-component mecs-component-name-map mecs-systems mecs-system-name-map
	  make-component-raw component? component-mecs component-index component-name component-mask component-constructor component-cleanup component-->string component-string->
	  make-entity-raw entity? entity-mecs entity-components entity-mask entity-mask-set! entity-component-count entity-component-count-set!
	  make-system system? system-components system-mecs system-mask make-tracked-system tracked-system?
	  tracked-system-enter tracked-system-exit tracked-system-update tracked-system-entities
	  )
  (import (except (kawa base) system)
	  (rnrs hashtables (6))
	  (lib mecs-protocols))
  (begin
    
    (define-simple-class mecs ()
      (entities ::kawa.lib.kawa.hashtable$HashTable)
      (components ::gnu.lists.FVector)
      (component-name-map ::kawa.lib.kawa.hashtable$HashTable)
      (systems ::gnu.lists.FVector)
      (system-name-map ::kawa.lib.kawa.hashtable$HashTable))

    (define-simple-class entity ()
      (the-mecs :: mecs)
      (components :: gnu.lists.FVector)
      (mask :: gnu.math.IntNum)
      (component-count :: gnu.math.IntNum))

    (define-simple-class component ()
      (the-mecs :: mecs)
      (index :: gnu.math.IntNum)
      (name :: gnu.mapping.SimpleSymbol)
      (mask :: gnu.math.IntNum)
      (constructor :: gnu.expr.CompiledProc)
      (cleanup :: gnu.expr.CompiledProc)
      (->string :: gnu.expr.CompiledProc)
      (string-> :: gnu.expr.CompiledProc))

    (define-simple-class system ()
      (the-mecs :: mecs)
      (components :: gnu.lists.FVector)
      (mask :: gnu.math.IntNum))

    (define-simple-class tracked-system (system)
      (enter :: gnu.expr.CompiledProc)
      (exit :: gnu.expr.CompiledProc)
      (update :: gnu.expr.CompiledProc)
      (entities :: kawa.lib.kawa.hashtable$HashTable))


    (define (make-mecs-raw entities components component-name-map systems system-name-map)
      (mecs entities: entities
	    components: components
	    component-name-map: component-name-map
	    systems: systems
	    system-name-map: system-name-map))

    (define (mecs? o)
      (instance? o mecs))

    (define (mecs-entities (m ::mecs))
      m:entities)
    
    (define (mecs-entities-set! (m ::mecs) v)
      (set! m:entities v))

    (define (mecs-components (m ::mecs))
      m:components)

    (define (mecs-component (m ::mecs) (name ::gnu.mapping.SimpleSymbol))
      (if (hashtable-contains? (mecs-component-name-map m) name)
	  (vector-ref (mecs-components m) (hashtable-ref (mecs-component-name-map m) name #f))
	  (error "mecs-component" "Tried to get component which wasn't in the name table." name)))

    (define (mecs-component-name-map (m ::mecs))
      m:component-name-map)

    (define (mecs-systems (m ::mecs))
      m:systems)

    (define (mecs-system-name-map (m ::mecs))
      m:system-name-map)


    (define (make-component-raw the-mecs index name mask constructor cleanup ->string string->)
      (component the-mecs: the-mecs
		 index: index
		 name: name
		 mask: mask
		 constructor: constructor
		 cleanup: cleanup
		 ->string: ->string
		 string->: string->))

    (define (component? o)
      (instance? o component))

    (define (component-mecs (o :: component))
      o:the-mecs)
    (define (component-index (o :: component))
      o:index)
    (define (component-name (o :: component))
      o:name)
    (define (component-mask (o :: component))
      o:mask)
    (define (component-constructor (o :: component))
      o:constructor)
    (define (component-cleanup (o :: component))
      o:cleanup)
    (define (component-->string (o :: component))
      o:->string)
    (define (component-string-> (o :: component))
      o:string->)


    (define (make-entity-raw the-mecs components mask component-count)
      (entity the-mecs: the-mecs components: components mask: mask component-count: component-count))

    (define (entity? o)
      (instance? o entity))

    (define (entity-mecs (e ::entity))
      e:the-mecs)

    (define (entity-components (e ::entity))
      e:components)

    (define (entity-mask (e ::entity))
      e:mask)

    (define (entity-component-count (e ::entity))
      e:component-count)

    (define (entity-mask-set! (e ::entity) v)
      (set! e:mask v))

    (define (entity-component-count-set! (e ::entity) v)
      (set! e:component-count v))    


    (define (system-mecs (s ::system))
      s:the-mecs)

    
    (define (system-mask (s ::system))
      s:mask)
    
    (define (system-components (s ::system))
      s:components)
    
    (define (system? o)
      (instance? o system))

    (define (make-system-raw the-mecs components mask)
      (system the-mecs: the-mecs components: components mask: mask))

    (define make-system (make-system-protocol make-system-raw mecs-component component-mask))


    (define (tracked-system-enter (s ::tracked-system))
      s:enter)

    (define (tracked-system-exit (s ::tracked-system))
      s:exit)

    (define (tracked-system-update (s ::tracked-system))
      s:update)

    (define (tracked-system-entities (s ::tracked-system))
      s:entities)

    (define (tracked-system? o)
      (instance? o tracked-system))

    (define make-tracked-system
      (make-tracked-system-protocol
       (lambda system-args
	 (lambda tracked-system-args
	   (let ((system-args-v (list->vector (apply (make-system-protocol list mecs-component component-mask) system-args)))
		 (tracked-system-args-v (list->vector tracked-system-args)))
	     (tracked-system
	      the-mecs: (vector-ref system-args-v 0)
	      components: (vector-ref system-args-v 1)
	      mask: (vector-ref system-args-v 2)
	      enter: (vector-ref tracked-system-args-v 0)
	      exit: (vector-ref tracked-system-args-v 1)
	      update: (vector-ref tracked-system-args-v 2)
	      entities: (vector-ref tracked-system-args-v 3)))))))
    
    (define make-mecs (lambda args::mecs
			(apply (mecs-protocol make-mecs-raw make-component-raw make-tracked-system)
			       args)))))
