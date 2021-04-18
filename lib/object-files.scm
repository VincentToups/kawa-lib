(define-library (lib object-files)
  (export
   aabbox
   file->lines
   lines->file
   center-and-floor-object
   dimensions
   snap-coordinates
   center-and-floor-file
   snap-center-and-floor-file)
  (import
    (except (kawa base) match)
    (lib shadchen)
    (lib util)
    (lib funny)
    (srfi 1)
    (only (srfi 13) string-tokenize)
    (rnrs hashtables))
  (begin
    (define (mean-of-2 a b)
      (/ (+ a b) 2))

    (define (file->lines filename)
      (call-with-input-file filename
	(lambda (port)
	  (let loop ((next-line (read-line port))
		     (lines '()))
	    (cond ((eof-object? next-line)
		   (reverse lines))
		  (else
		   (loop (read-line port)
			 (cons next-line lines))))))))

    (define (vertex-line? ln)
      (and (equal? "v" (car (string-tokenize ln)))))

    (define (parse-vertex-line ln)
      (map string->number (cdr (string-tokenize ln))))

    (define (Just x)
      (list 'Just x))

    (define (None)
      (list 'None))

    (define (maybe-parse-vertex-line ln)
      (if (vertex-line? ln)
	  (Just (parse-vertex-line ln))
	  (None)))

    (define (ensure-lines lines)
      (if (string? lines) (file->lines lines) lines))

    (define (extract-vertex-lines file)
      (filter (lambda (ln)
		(match (maybe-parse-vertex-line ln)
		  ((list 'Just (list x y z))
		   #t)
		  (otherwise #f)))
	      (ensure-lines file)))

    (define (extract-vertices file)
      (let ((out '()))
	(for-each (lambda (ln)
		    (match (maybe-parse-vertex-line ln)
		      ((list 'Just (list x y z))
		       (set! out (cons (list x y z) out)))
		      (otherwise #f)))
		  (ensure-lines file))
	(reverse out)))

    (define (aabbox obj)
      (chain $ (ensure-lines obj)
	     (reduce-on-vertices
	      (lambda (vtx state)
		(match (list vtx state)
		  ((list (list vx vy vz)
			 (list min-x min-y min-z max-x max-y max-z))
		   (list
		    (if (< vx min-x) vx min-x)
		    (if (< vy min-y) vy min-y)
		    (if (< vz min-z) vz min-z)
		    (if (> vx max-x) vx max-x)
		    (if (> vy max-y) vy max-y)
		    (if (> vz max-z) vz max-z)))))
	      (list +inf.0 +inf.0 +inf.0
		    -inf.0 -inf.0 -inf.0)
	      $)
	     (match $
	       ((list min-x min-y min-z max-x max-y max-z)
		(list (list min-x max-x)
		      (list min-y max-y)
		      (list min-z max-z))))))

    (define (calculate-center lines)
      (let ((bb (aabbox lines)))
	(map (lambda (edges)
	       (apply mean-of-2 edges))
	     bb)))

    (define (coordinates->vertex-line coords)
      (match coords
	((list x y z)
	 (apply format (cons "v ~a ~a ~a" coords)))))

    (define (translate-vertices lines by)
      (map (lambda (line)
	     (match (maybe-parse-vertex-line line)
	       ((list 'Just coordinates)
		(coordinates->vertex-line (map + coordinates by)))
	       ((list 'None)
		line)))
	   (if (string? lines)
	       (file->lines lines)
	       lines)))

    (define center-object
      (// lines ->
	  (translate-vertices lines (map - (calculate-center lines)))))

    (define (lowest-z lines)
      (fold (lambda (line mz)
	      (match (maybe-parse-vertex-line line)
		((list 'Just (list x y z))
		 (if (< z mz) z mz))
		((list 'None)
		 mz)))
	    +inf.0
	    lines))

    (define (lowest-y lines)
      (fold (lambda (line my)
	      (match (maybe-parse-vertex-line line)
		((list 'Just (list x y z))
		 (if (< y my) y my))
		((list 'None)
		 my)))
	    +inf.0
	    lines))

    (define (reduce-on-vertices f-it-ac init lines)
      (fold (lambda (line state)
	      (match (maybe-parse-vertex-line line)
		((list 'Just coordinate)
		 (f-it-ac coordinate state))
		((list 'None)
		 state)))
	    init
	    lines))

    (define (zero-z lines)
      (let* ((lines (ensure-lines lines))
	     (z-min (lowest-z lines)))
	(translate-vertices lines (list 0 0 (- z-min)))))

    (define (zero-y lines)
      (let* ((lines (ensure-lines lines))
	     (y-min (lowest-y lines)))
	(translate-vertices lines (list 0 (- y-min) 0))))

    (define (snap v pm)
      (/ (round (* v pm))
	 (* 1.0 pm)))

    (define (snap-coordinates lines premult)
      (map (lambda (line)
	     (match (maybe-parse-vertex-line line)
	       ((list 'Just coordinates)
		(coordinates->vertex-line (map (lambda (c)
						 (snap c premult)) coordinates)))
	       ((list 'None)
		line)))
	   (if (string? lines)
	       (file->lines lines)
	       lines)))

    (define (lines->file lines to-file)
      (call-with-output-file to-file
	(lambda (port)
	  (let loop ((lines lines))
	    (cond ((eq? lines '()) 'done)
		  (else
		   (display (car lines) port)
		   (newline port)
		   (loop (cdr lines))))))))

    (define (center-and-floor-object file-or-lines)
      (chain $ (ensure-lines file-or-lines)
	     (center-object $)
	     (zero-y $)))

    (define (dimensions obj)
      (chain $ (ensure-lines obj)
	     (reduce-on-vertices
	      (lambda (vtx state)
		(match (list vtx state)
		  ((list (list vx vy vz)
			 (list min-x min-y min-z max-x max-y max-z))
		   (list
		    (if (< vx min-x) vx min-x)
		    (if (< vy min-y) vy min-y)
		    (if (< vz min-z) vz min-z)
		    (if (> vx max-x) vx max-x)
		    (if (> vy max-y) vy max-y)
		    (if (> vz max-z) vz max-z)))))
	      (list +inf.0 +inf.0 +inf.0
		    -inf.0 -inf.0 -inf.0)
	      $)
	     (match $
	       ((list min-x min-y min-z max-x max-y max-z)
		(list (- max-x min-x)
		      (- max-y min-y)
		      (- max-z min-z))))))

    (define (center-and-floor-file file)
      (chain $ (center-and-floor-object file)
	     (lines->file $ file)))

    (define (snap-center-and-floor-file file premult)
      (chain $
	     (snap-coordinates file premult)
	     (center-and-floor-object $)
	     (snap-coordinates $ premult)
	     (lines->file $ file)))))
