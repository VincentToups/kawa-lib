(define-library (lib parse-arglist)
  (export switch-info
	  command-line-spec
	  parse-command-line
	  resolve-switch-name
	  get-switch-value)
  (import (except (kawa base) match format)
	  (rename (lib xformat) (xformat format))
	  (lib shadchen)
	  (lib list-parser)
	  (rename (lib list-parser) (parser-return return))
	  (kawa regex)
	  (rnrs hashtables)
	  (lib maybe))
  (begin
    (define (command-switch? s)
      (match (regex-match (regex "(^-([a-zA-Z0-0])$|^--([-_a-zA-Z0-9]+)$)") s)
	((list a b (truthy? text) c)
	 text)
	((list a b c (truthy? text))
	 text)
	(otherwise #f)))
    
    (define parse-raw-command-switch
      (parser
       (x <- (parse-predicate-value command-switch?))
       (return (string->symbol x))))

    (define (switch-info switch abbreviations default processor documentation)
      (list (ensure-symbol switch) (map ensure-symbol abbreviations) default processor documentation))

    (define (command-line-spec . switch-infos)
      (let ((o (make-eq-hashtable)))
	(let loop ((rest switch-infos))
	  (match rest
	    ((list) o)
	    ((list* (and (list switch abbreviations default processor documentation)
			 info) rest)
	     (for-each (lambda (abr)
			 (hashtable-set! o abr switch)) abbreviations)
	     (hashtable-set! o switch info)
	     (loop rest))))))

    (define (resolve-switch-name tbl s)
      (let ((v (hashtable-ref tbl s #f)))
	(cond ((eq? v #f) #f)
	      ((list? v) v)
	      ((symbol? v) (resolve-switch-name tbl v)))))

    (define (ensure-symbol s)
      (cond
       ((string? s) (string->symbol s))
       ((symbol? s) s)
       (else
	(error (format "Can't coerce ~a into a symbol." s)))))

    (define (parse-and-resolve-command-switch table)
      (parser
       (s <- parse-raw-command-switch)
       (let ((x (resolve-switch-name table s)))
	 (match x
	   ((list name b d #f c)
	    (return (cons name #t)))
	   ((list name abbreviations default (procedure? tr) documentation)
	    (parser
	     (hd <- parse-car)
	     (match (tr hd)
	       ((truthy? v) (return (cons name v)))
	       (anything-else
		(parse-fail "Failure to extract/transform argument for ~a ~a: ~a\n  ~a\n" name abbreviations anything-else documentation)))))
	   (anything-else
	    (parse-fail "Unrecognized command line switch: ~a" s))))))

    (define-parser (command-line-parser tbl)
      (parser
       (switches <- (parse-repeatedly (parse-and-resolve-command-switch tbl)))
       (rest <- retrieve)
       (cond
	((eq? rest '())
	 (return (list (alist->hasthtable switches) (list) tbl)))
	((command-switch? (car rest))
	 (parse-fail "Unrecognized command line switch: ~a" (car rest)))
	(else
	 (return (list (alist->hasthtable switches) rest tbl))))))

    (define (alist->hasthtable alst)
      (let ((htbl (make-eq-hashtable)))
	(for-each (lambda (pr)
		    (hashtable-set! htbl (car pr) (cdr pr)))
		  alst)
	htbl))

    (define (maybe-hashtable-ref tbl name . reason)
      (let* ((sigil (list 'sigil))
	     (r (hashtable-ref tbl name sigil)))
	(if (eq? r sigil)
	    (if (eq? '() reason)
		nothing
		(Nothing (apply format reason)))
	    (maybe-return r))))

    (define (get-switch-value (parsed-command-line :: Maybe) name)
      (begin-maybe
       ((list parsed rest tbl) <- parsed-command-line)
       ((list* a b default rest) <- (maybe-hashtable-ref tbl name
							 "There is no switch named ~a" name))
       (maybe-return (hashtable-ref parsed name default))))

    (define (parse-command-line tbl lst)::Maybe
      (match ((command-line-parser tbl) (State lst))
	((? maybe-state-success? mss)
	 (let ((mss :: Maybe-State-Success mss))
	   (just mss:value)))
	((? maybe-state-failure? msf)
	 (let* ((msf :: Maybe-State-Failure msf)
		(no :: Nothing (Nothing)))
	   (set! no:meta-data (msf:explain))
	   no))))))
