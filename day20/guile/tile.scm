
(use-modules (ice-9 rdelim)) ;; read line ?
(use-modules (ice-9 pretty-print)) ;; show stuff nicely
(define pp pretty-print) 
(use-modules (srfi srfi-1)) ;; list


; ===============================================================
;; insecure datatype
;; checks if it has the right shape and right type of components
;; general square tile
;; 
;; rotate-c rotate-cc flip-vert flip-horz
;; tile= tile-id tile-height tile-width tile?
;; ==============================================================
(define make-tile
  (lambda (id arr)
    (let ((tt (list 'tile id arr)))
      (if (tile? tt)
	  tt
	  (error "make-tile did not make a tile")))))

(define tile-data
  (lambda (tt)
    (if (tile? tt)
	(third tt)
	(error "tile-data not a tile"))))

(define tile-id
  (lambda (tt)
    (if (tile? tt)
	(second tt)
	(error "tile-id not a tile"))))

(define tile-width
  (lambda (tt)
    (if (tile? tt)
	(second (first (array-dimensions (tile-data tt))))
	(error "tile-id not a tile"))))

(define tile-height
  (lambda (tt)
    (if (tile? tt)
	(second (second (array-dimensions (tile-data tt))))
	(error "tile-id not a tile"))))

(define tile?
  (lambda (tt)
    (and
     (list? tt)
     (= (length tt) 3)
     (eq? (first tt) 'tile)
     (integer? (second tt))
     (array? (third tt)))))


(define grid-xy
  (lambda (arr x y)
    (if (array? arr)
	(array-ref arr x y)
	(error "grid-xy not an array"))))

(define grid-xy!
  (lambda (arr x y v)
    (if (array? arr)
	(array-set! arr v x y)
	(error "grid-xy! not an array"))))

(define tile-xy
  (lambda (tt x y)
    (if (tile? tt)
	(let ((td (tile-data tt)))
	  (grid-xy td x y))
	(error "tile-xy not a tile"))))

(define tile-xy!
  (lambda (tt x y v)
    (if (tile? tt)
	(let ((td (tile-data tt)))
	  (grid-xy! td x y v))
	(error "tile-xy! not a tile"))))


(define (make-tile-array wid hgt)
  (make-array #\. `(1 ,wid) `(1 ,hgt)))



;; -----------------------------------------------------------------
(define tt (make-tile 3 (make-tile-array 96 96)))

(tile-id tt)

(tile-data tt)

(tile-width tt)

(tile-height tt)

(tile? tt)

;; ===================================================================
(define make-tag
  (lambda (ch)
    (list 'tag ch #f)))

(define tag?
  (lambda (tt)
    (and (list? tt)
	 (= (length tt) 3)
	 (char? (second tt))
	 (boolean? (third tt)))))

(define tag-char
  (lambda (tt)
    (if (tag? tt)
	(second tt)
	(error "tag-char not a tag"))))

(define tag-bool
  (lambda (tt)
    (if (tag? tt)
	(third tt)
	(error "tag-bool not a tag"))))

(define tag!
  (lambda (tt)
    (if (tag? tt)
	(set-cdr! (cdr tt) (list #t))
	(error "tag-char not a tag"))))


(define show-tag
  (lambda (tt)
    (if (tag? tt)
	(format #t "~a" (second tt))
	(error "show-tag not a tag"))))

  

;; ===================================================================
(define t (make-tag #\p))

t

(tag? t)

(tag-char t)

(tag-bool t)

(tag! t)

t





;; -------------------------------------------------------------------
(define (show-tile tt)
  (let ((id (tile-id tt))
	(g (tile-data tt))
	(width (tile-width tt))
	(height (tile-height tt)))
    (format #t "~%Tile ~a:~%" id)
    (let loop-y ((y 1))
      (cond
       ((> y height)
	(format #t "~%"))
       (#t
	(let loop-x ((x 1))
	  (cond
	   ((> x width) #f)
	   (#t (let ((elem (tile-xy tt x y)))
		 (cond
		  ((tag? elem) (show-tag elem))
		  (#t (format #t "~a" elem))))
	       (loop-x (+ x 1))))) ;let loop-x
	(format #t "~%")
	(loop-y (+ y 1)))))))


;; -----------------------------------------------------------------------
(show-tile tt)


(define (test-tile)
  (let ((id 0)
	(g (make-tile-array 10 10))
	(width 10)(height 10))
  (let loop ((x 1)(y 1)(d 1))
      (cond
       ((> y height) (make-tile id g))
       ((> x width) (loop 1 (+ y 1) d))
       (#t
	(grid-xy! g x y d)
	(loop (+ x 1) y (+ d 1)))))))

(define tt2 (test-tile))

(show-tile tt2)

;; ------------------------------------------------------------------------------
(tile? (test-tile))
;; ------------------------------------------------------------------------------

;; clockwise 
(define (rotate-c tt)
  (let* ((id (tile-id tt))	 
	 (width (tile-width tt))
	 (height (tile-height tt))
	 (g (make-tile-array width height)))
    (let loop ((x 1)(y 1))
      (cond
       ((> y height) (make-tile id g))
       ((> x width) (loop 1 (+ y 1)))
       (#t
	(let ((d (tile-xy tt x y)))
	  (grid-xy! g (+ 1 (- height y)) x d)
	  (loop (+ x 1) y)))))))



;; ------------------------------------------------------------------------------
(tile? (rotate-c (test-tile)))

(define tt3 (rotate-c tt2))

(show-tile tt3)

;; ------------------------------------------------------------------------------


;; counter clockwise
(define (rotate-cc tt)
  (let* ((id (tile-id tt))
	 (width (tile-width tt))
	 (height (tile-height tt))
	 (g (make-tile-array width height)))
    (let loop ((x 1)(y 1))
      (cond
       ((> y height) (make-tile id g))
       ((> x width) (loop 1 (+ y 1)))
       (#t
	(let ((d (tile-xy tt x y)))
	  (grid-xy! g y (+ 1 (- width x)) d)
	  (loop (+ x 1) y)))))))


;; ------------------------------------------------------------------------------
(define tt4 (rotate-cc tt2))

(show-tile tt4)

;; ------------------------------------------------------------------------------


;; flip horizontal
(define (flip-horz tt)
  (let* ((id (tile-id tt))
	 (width (tile-width tt))
	 (height (tile-height tt))
	 (g (make-tile-array width height)))
    (let loop ((x 1)(y 1))
      (cond
       ((> y height) (make-tile id g))
       ((> x width) (loop 1 (+ y 1)))
       (#t
	(let ((d (tile-xy tt x y)))
	  (grid-xy! g (+ 1 (- width x)) y d)
	  (loop (+ x 1) y)))))))


;; ------------------------------------------------------------------------------
(define tt5 (flip-horz tt2))

(show-tile tt5)

(tile? tt5)
;; ------------------------------------------------------------------------------


;; flip vertical
(define (flip-vert tt)
  (let* ((id (tile-id tt))
	 (width (tile-width tt))
	 (height (tile-height tt))
	 (g (make-tile-array width height)))
    (let loop ((x 1)(y 1))
      (cond
       ((> y height) (make-tile id g))
       ((> x width) (loop 1 (+ y 1)))
       (#t
	(let ((d (tile-xy tt x y)))
	  (grid-xy! g x (+ 1 (- height y)) d)
	  (loop (+ x 1) y)))))))

;; ------------------------------------------------------------------------------
(define tt6 (flip-vert tt2))

(show-tile tt6)
;; ------------------------------------------------------------------------------


;; tile same if they have same array data
;; what does array-equal? mean
;; guile> ,describe array-equal?
(define (tile= tt tt2)
  (array-equal? (tile-data tt) (tile-data tt2)))

;; ;; ------------------------------------------------------------------------------
;; (tile= (flip-vert (flip-vert (test-tile))) (test-tile))
;; (tile= (flip-horz (flip-horz (test-tile))) (test-tile))
;; (tile= (rotate-c (rotate-c (rotate-c (rotate-c (test-tile))))) (test-tile))
;; ;; ------------------------------------------------------------------------------

(define (read-lines filename)
  (call-with-input-file filename
    (lambda (port) 
      (let loop ((lines '()))
	(let ((line (read-line port)))
	  (cond
	   ((eof-object? line) (reverse lines))
	   ((zero? (string-length line)) (loop lines))
	   (#t (loop (cons line lines)))))))))

(define loch-string (apply string-append (read-lines "lochness.txt")))

(define (loch-tile str)
  (let ((id (* 96 96))
	(g (make-tile-array 96 96))
	(width 96)
	(height 96))
  (let loop ((x 1)(y 1)(i 0))
      (cond
       ((> y height) (make-tile id g))
       ((> x width) (loop 1 (+ y 1) i))
       (#t
	(let ((ch (string-ref str i))) 
	(grid-xy! g x y (make-tag ch))
	(loop (+ x 1) y (+ i 1))))))))

(define lochtt (loch-tile loch-string))

;; ================================================================
(define example-loch-string (apply string-append (read-lines "../example-lochness.txt")))

(define (example-loch-tile str)
  (let ((id (* 24 24))
	(g (make-tile-array 24 24))
	(width 24)
	(height 24))
  (let loop ((x 1)(y 1)(i 0))
      (cond
       ((> y height) (make-tile id g))
       ((> x width) (loop 1 (+ y 1) i))
       (#t
	(let ((ch (string-ref str i))) 
	(grid-xy! g x y (make-tag ch))
	(loop (+ x 1) y (+ i 1))))))))

(define examplett (example-loch-tile example-loch-string))


#|

loch ness looks like this

                  # 
#    ##    ##    ###
 #  #  #  #  #  #   

                19
1 6 7 12 13 18 19 20
 2 5 8 11 14 17

find loch ness in a grid


given some start coordinate for the top left

|#

;; guard against out of bounds char matches , means free to look wherever
(define guard-xy
  (lambda (tt x y z)
    (let ((width (tile-width tt))
	  (height (tile-height tt)))
      (cond
       ((and (>= x 1)(<= x width)(>= y 1)(<= y height))
	(let ((elem (tile-xy tt x y)))
	  (cond
	   ((not (tag? elem)) (error "expected a tag char !"))
	   ((char=? (tag-char elem) z) #t)
	   (#t #f))))
       (#t #f)))))





(define lochness
  (let* ((row1 (map (lambda (x) (list x 1))(list 19)))
	 (row2 (map (lambda (x) (list x 2))(list 1 6 7 12 13 18 19 20)))
	 (row3 (map (lambda (x) (list x 3))(list 2 5 8 11 14 17)))	
	 (ness (append row1 row2 row3))
	 (match-len (length ness)))
    (lambda (tt x y)
      (let ((coords (map (lambda (p)
			   (let ((px (first p))
				 (py (second p)))
			     (list (+ x px)(+ y py))))
			 ness)))
	;; if there is a lochness
	(let ((match-coords (filter (lambda (p)
				      (let ((px (first p))
					    (py (second p)))
					(guard-xy tt px py #\#)))
				    coords)))
	  (cond
	   ((= (length match-coords) match-len)
	    (format #t "found ness at coordinates ~a ~a ~%" x y)
	    #t)
	   (#t
	    
	    ;; (format #t "match coords ~a / ~a of ~a ~%"
	    ;; 	    match-coords
	    ;; 	    (length match-coords)
	    ;; 	    match-len)
	    
	    #f)))))))



(define lochness3
  (let* ((row1 (map (lambda (x) (list x 1))(list 19)))
	 (row2 (map (lambda (x) (list x 2))(list 1 6 7 12 13 18 19 20)))
	 (row3 (map (lambda (x) (list x 3))(list 2 5 8 11 14 17)))	
	 (ness (append row1 row2 row3))
	 (match-len (length ness)))
    (lambda (tt x y)
      (let ((coords (map (lambda (p)
			   (let ((px (first p))
				 (py (second p)))
			     (list (+ x px)(+ y py))))
			 ness)))
	;; if there is a lochness
	(let ((match-coords (filter (lambda (p)
				      (let ((px (first p))
					    (py (second p)))
					(guard-xy tt px py #\#)))
				    coords)))

	  ;;(format #t "matched => ~a~%" match-coords)
	  
	  ;; (let ((gtt (make-tile 1 (make-tile-array (tile-width tt)
	  ;; 					 (tile-height tt)))))
	  ;;   (let ((count 0))
	  ;;   (let loop ((x 1)(y 1))
	  ;;     (cond
	  ;;      ((> x (tile-width tt))
	  ;; 	(loop 1 (+ y 1)))
	  ;;      ((> y (tile-height tt)) #f)
	  ;;      (#t
	  ;; 	(cond
	  ;; 	 ((member (list x y) match-coords)
	  ;; 	  (set! count (+ count 1))
	  ;; 	  (grid-xy! (tile-data gtt) x y #\O))
	  ;; 	 ((and (member (list x y) coords) ;; no match
	  ;; 	       (not (member (list x y) match-coords)))
	  ;; 	  (grid-xy! (tile-data gtt) x y (tile-xy tt x y)))
	  ;; 	 (#t
	  ;; 	  (grid-xy! (tile-data gtt) x y #\space)))
	  ;; 	(loop (+ x 1) y))))

	    (if (>= (length match-coords) 15)
		(format #t "found ness at ~a ~a ~%" x y)
		(begin
		  (let loop ((x 1)(y 1))
		    (cond
		     ((> x (tile-width tt))
		      (loop 1 (+ y 1)))
		     ((> y (tile-height tt)) #f)
		     (#t
		      (cond
		       ((member (list x y) match-coords)
			(tag! (tile-xy tt x y))))			
		    (loop (+ x 1) y))))))
	    
	    #f)))))




    



;;(lochness lochtt 1 1)

;; no lochness found
;; if we just randomly expand search area , nothing happens except
(define loch-search
  (lambda (tt)
    (let ((width (tile-width tt))
	  (height (tile-height tt)))
    (let loop ((x 0)(y 0))
      (cond
       ((> x width)
	(format #t "loch-search finished row ~a~%" y)
	(loop 1 (+ y 1)))
       ((> y height) #f)
       (#t
	;;(format #t "checking ~a ~a ~%" x y)
	(lochness3 tt x y)
	(loop (+ x 1) y)))))))


  


;; example
;; (loch-search examplett)
;; is it the same memory location for each 
(define loch-tiles
  (lambda ()
    (list lochtt
	  (rotate-c lochtt)
	  (rotate-c (rotate-c lochtt))
	  (rotate-c (rotate-c (rotate-c lochtt)))
	  (flip-horz lochtt)
	  (rotate-c (flip-horz lochtt))
	  (rotate-c (rotate-c (flip-horz lochtt)))
	  (rotate-c (rotate-c (rotate-c (flip-horz lochtt))))
	  ;; the rest are not required , for peace of mind ...
	  ;; (flip-vert lochtt)
	  ;; (rotate-c (flip-vert lochtt))
	  ;; (rotate-c (rotate-c (flip-vert lochtt)))
	  ;; (rotate-c (rotate-c (rotate-c (flip-vert lochtt))))
	  )))



(define (fmap f xs)
  (cond
   ((null? xs) xs)
   (#t (f (car xs))
       (fmap f (cdr xs)))))


(define lochness-search
  (lambda ()
    (fmap (lambda (x)
	    (format #t "searching next tile ~%")
	    (loch-search x)) (loch-tiles))))




(define lochness2
  (let* ((row1 (map (lambda (x) (list x 1))(list 19)))
	 (row2 (map (lambda (x) (list x 2))(list 1 6 7 12 13 18 19 20)))
	 (row3 (map (lambda (x) (list x 3))(list 2 5 8 11 14 17)))	
	 (ness (append row1 row2 row3))
	 (match-len (length ness)))
    (lambda (tt x y)
      (let ((coords (map (lambda (p)
			   (let ((px (first p))
				 (py (second p)))
			     (list (+ x px)(+ y py))))
			 ness)))
	;; if there is a lochness
	(let ((match-coords (filter (lambda (p)
				      (let ((px (first p))
					    (py (second p)))
					(guard-xy tt px py #\#)))
				    coords)))
	  (let ((gtt (make-tile 1 (make-tile-array (tile-width tt)
						 (tile-height tt)))))
	    ;;(format #t "matched => ~a~%" match-coords)
	    
	    (let loop ((x 1)(y 1))
	      (cond
	       ((> x (tile-width tt))
		(loop 1 (+ y 1)))
	       ((> y (tile-height tt)) #f)
	       (#t
		(cond
		 ((member (list x y) match-coords)
		  (grid-xy! (tile-data gtt) x y #\O))
		 ((and (member (list x y) coords) ;; no match
		       (not (member (list x y) match-coords)))
		  (grid-xy! (tile-data gtt) x y (tile-xy tt x y)))
		 (#t
		  (grid-xy! (tile-data gtt) x y #\space)))
		(loop (+ x 1) y))))

	    gtt))))))



(define example-soltt (flip-horz (rotate-c examplett)))

;;we can show a full lochness if we do these 
;;(show-tile (lochness2 example-soltt 2 2))
;;(show-tile (lochness2 example-soltt 1 16))

;; yet our "search" does not find anything?


  
	  
	  
	  


   
       


  



