
;; using guile 2d arrays rather than vector of vectors

;; in guile
(use-modules (ice-9 rdelim)) ;; read line ?

(use-modules (ice-9 pretty-print)) ;; show stuff nicely

(define pp pretty-print) 

;; what to import , what not to import 
(use-modules (srfi srfi-1)) ;; list

;; (use-modules (srfi srfi-9)) ;; record types - user defined records data structures

;; ==================================================================================
;; assert X
(define (warn m x)
  (let ((out x))
    (if (not out)
	(format #t "~a : ~a " m x))))


(defmacro assert (m x)
  (if (not x) (error (string-append "assert fail : " m) x)))


;; (assert "random test" #f)

;; ======= fix geiser/guile/emacs interaction 
;; M-x fix-keyboard () [] key swap 
;; (getcwd)
;; (chdir "day20/guile")

;; read non empty lines from a file , keep lines in same order appear in file 
(define (read-lines filename)
  (call-with-input-file filename
    (lambda (port) 
      (let loop ((lines '()))
	(let ((line (read-line port)))
	  (cond
	   ((eof-object? line) (reverse lines))
	   ((zero? (string-length line)) (loop lines))
	   (#t (loop (cons line lines)))))))))

;; ====================================================================

;;(substring "Tile 12323:" 0 4)

;; split-tiles : [String] -> [[String]]
;; (defun split-tiles (xs)
;;   (split-tiles-helper xs nil nil))

;; ;; split-tiles-helper: [String] -> [String] -> [[String]]
;; (defun split-tiles-helper (xs tmps rs)
(define (split-tiles xs)
  (let loop ((tiles '()))
    (cond
     ((null? xs) (reverse tiles))
     (#t (let ((s (car xs))) ;; s is a string	     
	   (cond
	    ((string= (substring s 0 4) "Tile")
	     (let ((id (with-input-from-string
			   (substring s 5 (- (string-length s) 1))
			 (lambda () (read)))))
	       (set! xs (cdr xs))
	       (loop (cons (list id (take xs 10)) (begin (set! xs (drop xs 10))
							 tiles)))))))))))

;; =================================================================================
;; tile data type 
;; (define-record-type <tile>
;;   (make-tile id array)
;;   tile?
;;   (id      tile-id)
;;   (array   tile-array))


;; ================================================================================
;; square grid implementation
;; drawback using scheme is either find module that does what want
;; write it again from scratch over and over
;; ===============================================================================
(define (make-tile-data wid hgt)
  (make-array #\. `(1 ,wid) `(1 ,hgt)))

;; optional args ??
(define (make-tile-array)
  (make-tile-data 10 10))

(define make-tile #f)

(define tile-data #f)

(define tile-id #f)

(define tile? #f)

;; secret sauce
(let ((sauce (gensym "sauce")))
  (letrec ((mk-t (lambda (id arr)
      (warn "make-tile : expected id integer" (integer? id))
      (warn "make-tile : expected arr array" (array? arr))
      (list (lambda (x)(eq? x sauce)) id arr)))
	   (t?  (lambda (p)
		  (and (list? p)(= (length p) 3)((car p) sauce))))
	   (t-d (lambda (p)
		  (if (t? p) (third p) (error "t-ds expected a tile" p))))
	   (t-id (lambda (p)
		   (if (t? p) (second p) (error "t-ids expected a tile")))))
    (set! make-tile mk-t)
    (set! tile? t?)
    (set! tile-data t-d)
    (set! tile-id t-id)))





;;------------------------------------------------------------------------------
  
(define (grid-xy arr x y)
  (let* ((dim (array-dimensions arr))
	 (max-x (second (first dim)))
	 (max-y (second (second dim))))
    (warn "grid-xy : x out of bounds " (and (>= x 1)(<= x max-x)))
    (warn "grid-xy : y out of bounds " (and (>= y 1)(<= y max-y)))
    (warn "grid-xy : expected an array" (array? arr))
    (array-ref arr x y)))

(let ((g (make-tile-data 10 10)))
  (grid-xy g 1 1))

;; (let ((g (make-tile-data 10 10)))
;;   (grid-xy g 0 0))


;;------------------------------------------------------------------------------


(define (grid-xy! arr x y v)
  (let* ((dim (array-dimensions arr))
	 (max-x (second (first dim)))
	 (max-y (second (second dim))))
    (warn "grid-xy! : x out of bounds " (and (>= x 1)(<= x max-x)))
    (warn "grid-xy! : y out of bounds " (and (>= y 1)(<= y max-y)))
    (warn "grid-xy! : expected an array" (array? arr))
    (array-set! arr v x y)))

;; ----------------------------------------------------------------------------
(let ((g (make-tile-data 10 10)))
  (grid-xy! g 1 1 3)
  (format #t "g => ~a~%" g))
;;------------------------------------------------------------------------------


;; join 10 strings together
;; ;; (apply string-append list-of-ten-strings) as input
(define (convert->grid s)
  (let* ((width 10)(height 10)(g (make-tile-data width height)))
    (let loop ((i 0)(x 1)(y 1))
      (cond
       ((> y height) g)
       ((> x width) (loop i 1 (+ y 1)))
       (#t
	(let ((ch (string-ref s i)))
	  (grid-xy! g x y ch)
	  (loop (+ i 1) (+ x 1) y)))))
    g))



;; ===========================================================================
;; the input itself
(define tiles (split-tiles (read-lines "../input.txt")))


(define xtiles (map (lambda (x)
		   (make-tile (car x)
			      (convert->grid
			       (apply string-append (car (cdr x))))))
		    tiles))

(set! tiles xtiles)


;; ============================================================================



;; ===========================================================================
;; (define-record-type <tile>
;;   (make-tile id array)
;;   tile?
;;   (id    tile-id    set-tile-id!) 
;;   (array tile-array set-tile-array!))
;; (make-record-type "tile" (list 3 (make-tile-data 10 10)))


;; ===========================================================================




;;     ;; 		  (let ((array-string ""))
;;     ;; 		    (loop for i from 1 to 10 do
;;     ;; 			  (setq array-string (concatenate 'string array-string (car xs)))
;;     ;; 			  (setq xs (cdr xs))) ;; drop			  
;;     ;; 		    (setq tiles (cons (list id array-string) tiles)))))))))
;;     ;; (reverse tiles)))

;; ;; ==============================================================================

;; (defun make-tile-array ()
;;   (make-array '(11 11) :initial-element #\.))

;; (define (tile-id tt)
;;   (car tt))

;; (define (tile-array tt)
;;   (car (cdr tt)))

;; ----------------------------------------------------------------------------
(define (tile-xy tt x y)
  (assert "tile-xy : expected tile tt" (tile? tt))
  (let ((d (tile-data tt)))
    (assert "tile-xy : expected tile data " (array? d))    
    (array-ref d x y)))

;; ------------------------------------------------------------------------------


  ;; ;; (assert (>= x 1))
  ;; ;; (assert (<= x 10))
  ;; ;; (assert (>= y 1))
  ;; ;; (assert (<= y 10))
  ;; (let ((arr (tile-array tt)))
  ;;   (array-ref arr x y)))


(define (show-tile tt)
  (let ((id (tile-id tt))
	(g (tile-data tt))
	(width 10)(height 10))
    (format #t "~%Tile ~a:~%" id)
    (let loop-y ((y 1))
      (cond
       ((> y height)
	(format #t "~%"))
       (#t
	(let loop-x ((x 1))
	  (cond
	   ((> x width) #f)
	   (#t (format #t "~a " (grid-xy g x y))
	       (loop-x (+ x 1)))))
	(format #t "~%")
	(loop-y (+ y 1)))))))



(define (test-tile)
  (let ((id 0)
	(g (make-tile-array))
	(width 10)(height 10))
  (let loop ((x 1)(y 1)(d 1))
      (cond
       ((> y height) (make-tile id g))
       ((> x width) (loop 1 (+ y 1) d))
       (#t
	(grid-xy! g x y d)
	(loop (+ x 1) y (+ d 1)))))))

;; ------------------------------------------------------------------------------
(tile? (test-tile))
;; ------------------------------------------------------------------------------

;; clockwise 
(define (rotate-c tt)
  (let ((id (tile-id tt))
	(g (make-tile-array))
	(width 10)(height 10))
    (let loop ((x 1)(y 1))
      (cond
       ((> y height) (make-tile id g))
       ((> x width) (loop 1 (+ y 1)))
       (#t
	(let ((d (tile-xy tt x y)))
	  (grid-xy! g (+ 1 (- 10 y)) x d)
	  (loop (+ x 1) y)))))))

;; ------------------------------------------------------------------------------
(tile? (rotate-c (test-tile)))
;; ------------------------------------------------------------------------------


;; counter clockwise
(define (rotate-cc tt)
  (let ((id (tile-id tt))
	(g (make-tile-array))
	(width 10)(height 10))
    (let loop ((x 1)(y 1))
      (cond
       ((> y height) (make-tile id g))
       ((> x width) (loop 1 (+ y 1)))
       (#t
	(let ((d (tile-xy tt x y)))
	  (grid-xy! g y (+ 1 (- 10 x)) d)
	  (loop (+ x 1) y)))))))

;; ------------------------------------------------------------------------------
(tile? (rotate-cc (test-tile)))
;; ------------------------------------------------------------------------------


;; flip horizontal
(define (flip-horz tt)
  (let ((id (tile-id tt))
	(g (make-tile-array))
	(width 10)(height 10))
    (let loop ((x 1)(y 1))
      (cond
       ((> y height) (make-tile id g))
       ((> x width) (loop 1 (+ y 1)))
       (#t
	(let ((d (tile-xy tt x y)))
	  (grid-xy! g (+ 1 (- 10 x)) y d)
	  (loop (+ x 1) y)))))))


;; ------------------------------------------------------------------------------
(tile? (flip-horz (test-tile)))
;; ------------------------------------------------------------------------------


;; flip vertical
(define (flip-vert tt)
  (let ((id (tile-id tt))
	(g (make-tile-array))
	(width 10)(height 10))
    (let loop ((x 1)(y 1))
      (cond
       ((> y height) (make-tile id g))
       ((> x width) (loop 1 (+ y 1)))
       (#t
	(let ((d (tile-xy tt x y)))
	  (grid-xy! g x (+ 1 (- 10 y)) d)
	  (loop (+ x 1) y)))))))

;; ------------------------------------------------------------------------------
(tile? (flip-vert (test-tile)))
;; ------------------------------------------------------------------------------


;; tile same if they have same array data
;; what does array-equal? mean
;; guile> ,describe array-equal?
(define (tile= tt tt2)
  (array-equal? (tile-data tt) (tile-data tt2)))

;; ------------------------------------------------------------------------------
(tile= (flip-vert (flip-vert (test-tile))) (test-tile))
(tile= (flip-horz (flip-horz (test-tile))) (test-tile))
(tile= (rotate-c (rotate-c (rotate-c (rotate-c (test-tile))))) (test-tile))
;; ------------------------------------------------------------------------------

;; =======================================================================
;; wasteland
;; =======================================================================
;; for some reason trying to filter out and identify unique tiles was too much
;; cognitive overload
;; -----------------------------------------------------------------------
;;
;; -----------------------------------------------------------------------
;; ------- BUG : cannot figure out why not filtering correctly , just infinite loops
;; --------
;; ======================= TO DO ===================================
;; ;; filter all unique tiles
;; (define (tile-member? x xs)
;;   ;; (format #t "tile-member? ~a ~a~%" x xs)
;;   (cond
;;    ((null? xs) #f)
;;    (#t (let ((tt (car xs)))
;; 	 (cond
;; 	  ((tile= x tt) #t)
;; 	  (#t (tile-member? x (cdr xs))))))))


;; (define (filter-tiles2 xs ys zs)
;;   ;; (format #t "filter-tiles ~a ~a~%" xs ys)
;;   (cond
;;    ((null? xs) zs)
;;    (#t (let ((tt (car xs)))
;; 	 (cond
;; 	  ((tile-member? tt ys) (filter-tiles2 (cdr xs) ys zs))
;; 	  ((tile-member? tt zs) (filter-tiles2 (cdr xs) ys zs))
;; 	  (#t (filter-tiles2 (cdr xs) ys (cons tt zs))))))))

;; (define (filter-tiles xs ys)
;;   (filter-tiles2 xs ys '()))

;; (define (filter-unique xs)
;;   (letrec ((filter-unique2 (lambda (x ys)
;; 			     (cond
;; 			      ((null? ys) (list x))
;; 			      (#t (let ((y (car ys)))
;; 				    (cond
;; 				     ((tile= x y) (filter-unique ys))
;; 				     (#t (cons x (filter-unique ys))))))))))
;;     (filter-unique2 (car xs) (cdr xs))))


;; remove duplicates
;; if xs null then empty list
;; if xs not null then x is hd , ys is tail (possibly empty list)
;;    map tile= over ys (possibly empty list)
;;    filter any true
;;    if any true - left with those that are true
;;      meaning there is a tile that is the same as tile at hd of list
;;      ie the hd tile is a duplicate !
;;    if none true - left with empty list
;;         then this tile is unique - so should keep this tile
;;
;;
;; what remove-dups does , is the exact reverse , yet leaves 8 tiles 
;; (filter (lambda (x) (if x #t #f)) '(#t #t #t #t))
;; (filter (lambda (x) (if x #t #f)) '(#f #f #f #f))

;; ;; * SUSPECT REMOVE-DUPS *
;; (define (remove-dups xs)
;;   (cond
;;    ((null? xs) '())
;;    (#t (let* ((x (car xs))
;; 	      (ys (cdr xs))
;; 	      (zs (map (lambda (p)(tile= p x)) ys))
;; 	      (zs2 (filter (lambda (p) (if p #t #f)) zs)))
;; 	 (cond
;; 	  ((null? zs2) (remove-dups (cdr xs)))
;; 	  (#t (cons x (remove-dups ys))))))))

;; ;; * SUSPECT REMOVE-DUPS-3 *
;; (define (remove-dups-3 f xs)
;;   (cond
;;    ((null? xs) '())
;;    (#t (let* ((x (car xs))
;; 	      (ys (cdr xs))
;; 	      (zs (map (lambda (p)(f p x)) ys))
;; 	      (zs2 (filter (lambda (p) (if p #t #f)) zs)))
;; 	 (cond
;; 	  ((null? zs2) (remove-dups-3 f (cdr xs)))
;; 	  (#t (cons x (remove-dups-3 f ys))))))))

;; ;;
;; (define (remove-dups-2 xs)
;;   (cond
;;    ((null? xs) '())
;;    (#t (let* ((x (car xs))
;; 	      (ys (cdr xs))
;; 	      (zs (map (lambda (p)(tile= p x)) ys))
;; 	      (zs2 (filter (lambda (p) (if p #t #f)) zs)))
;; 	 (cond
;; 	  ((null? zs2) (cons x (remove-dups-2 (cdr xs))))
;; 	  (#t (remove-dups-2 ys)))))))
;; ------------------------------------------------------------------------------

;; ==================================================================================
;; takes comparison function f and a list of items xs 
;; *SANCTIONED remove duplicates *
(define (remove-dups f xs)
  (cond
   ((null? xs) '())
   (#t (let* ((x (car xs))
	      (ys (cdr xs))
	      (zs (map (lambda (p)(f p x)) ys))
	      (zs2 (filter (lambda (p) (if p #t #f)) zs)))
	 (cond
	  ((null? zs2) (cons x (remove-dups f (cdr xs))))
	  (#t (remove-dups f ys)))))))
;;-----------------------------------------------------------------------------------
(remove-dups = (list 1 2 3 1 2 3 1 2 3 1 2 3))
;; ---------------------------------------------------------------------------------

;; ==================================================================================
;; * LOOKS GOOD *
;; remove all x from xs -- think purge 
(define (remove-all feq a xs)
  (cond
   ((null? xs) '())
   (#t (let* ((x (car xs))
	      (ys (cdr xs)))
	 (cond
	  ((feq a x) (remove-all feq a (cdr xs)))
	  (#t (cons x (remove-all feq a (cdr xs)))))))))

;; ---------------------------------------------------------------------------------
;; * TESTS LOOK GOOD *
(remove-all = 1 (list 1 2 3 1 2 3 1 2 3 1 2 3))

(remove-all = 2 (remove-all = 1 (list 1 2 3 1 2 3 1 2 3 1 2 3)))

(remove-all = 3 (remove-all = 2 (remove-all = 1 (list 1 2 3 1 2 3 1 2 3 1 2 3))))
;; ---------------------------------------------------------------------------------


;;===================================================================================
;; remove one x from xs
(define (remove-one feq a xs)
  (cond
   ((null? xs) '())
   (#t (let* ((x (car xs))
	      (ys (cdr xs)))
	 (cond
	  ((feq a x) (cdr xs))
	  (#t (cons x (remove-one feq a (cdr xs)))))))))

(remove-one = 1 (list 1 2 3 1 2 3 1 2 3 1 2 3))

(remove-one = 2 (remove-one = 1 (list 1 2 3 1 2 3 1 2 3 1 2 3)))

(remove-one = 3 (remove-one = 2 (remove-one = 1 (list 1 2 3 1 2 3 1 2 3 1 2 3))))
;;==================================================================================



;; --- we can actually test if no duplicates are created
;; --- we know there are 8 ways to fit the jigsaw ,
;; --- almost like a 2 sided jigsaw piece with image reflected on both sides
;; (equal? 
;;  (remove-dups tile= (create tt))
;;  (create tt)))

;; (define (all-tiles tt) 
;;     (define (create k)
;;       (let* ((r1 (rotate-c k))
;; 	     (r2 (rotate-c r1))
;; 	     (r3 (rotate-c r2))
;; 	     ;; (r4 (rotate-c r3))
;; 	     ;; (r5 (rotate-c r4))

;; 	     (f1 (flip-horz k))

;; 	     (g1 (rotate-c f1))
;; 	     (g2 (rotate-c g1))
;; 	     (g3 (rotate-c g2))
	     
;; 	     ;; (f2 (flip-horz r2))
;; 	     ;; (f3 (flip-horz r3))
;; 	     ;; (f4 (flip-horz r4))
;; 	     ;; (f5 (flip-horz r5))
	     
;; 	     ;; (g1 (flip-vert k))
;; 	     ;; (g2 (flip-vert r2))
;; 	     ;; (g3 (flip-vert r3))
;; 	     ;; (g4 (flip-vert r4))
;; 	     ;; (g5 (flip-vert r5))
;; 	     )

;; 	;;(list k r1 r2 r3 r4 r5  f1 f2 f3 f4 f5 g1 g2 g3 g4 g5)
;; 	(list k r1 r2 r3 f1 g1 g2 g3)))
;;     (create tt))
;; ----------------------------------------------------------------------

;; generate all possible unique tiles from single tile
;; flip-v rotate-c
(define (all-tiles tt) 
    (define (create k)
      (let* ((r1 (rotate-c k))
	     (r2 (rotate-c r1))
	     (r3 (rotate-c r2))
	     ;; rotate 3 times
	     (f1 (flip-horz k))
	     ;; flip it
	     ;; rotate another 3 times
	     (g1 (rotate-c f1))
	     (g2 (rotate-c g1))
	     (g3 (rotate-c g2))
	     )
	(list k r1 r2 r3 f1 g1 g2 g3)))
    (create tt))

;; --------------------------------------------------------------------


;; the filtering does not work?
(define g (test-tile))

g

(tile? g)

;; (define h (filter-unique (all-tiles g)))
;; (define i (remove-dups (all-tiles g)))
;; (define i2 (remove-dups-2 (all-tiles g)))
;; (define i3 (remove-dups-3 tile= (all-tiles g)))

;; FAULTY remove duplicate algorithm
(define j (remove-dups = (list 1 2 3 1 2 3 1 2 3 1 2 3)))

j

;; (4 1 2 3 )


;; (list 1 2 3 1 2 3 1 2 3)


;; question -
;; how can you be confident that the result of the expression give is what you expect

;; when using tables 2d arrays not clear at all if equivalence function array-equal?
;; is actually doing what it is supposed to, visually very difficult to acertain.
;; cannot 'see' it , too much data

;; ====================================================================








;; ;; for a given set of tiles
;; (defun the-width () (floor (sqrt (length tiles))))
;; (defun the-height () (the-width))

;; ;; for the given size of problem (the-width) X  (the-height)
;; ;; A B C
;; ;; D E F
;; ;; G H I
(define (factorial n)
  (define (factorial2 n m)
    (cond
     ((= n 1) m)
     (#t (factorial2 (- n 1) (* n m)))))
  (factorial2 n 1))

;; factorial calculations
(factorial 9)


;; 362880


(factorial 144)

;; 5550293832739304789551054660550388117999982337982762871343070903773209740507907044212761943998894132603029642967578724274573160149321818341878907651093495984407926316593053871805976798524658790357488383743402086236160000000000000000000000000000000000
	    
   
;; ;;(defun tile-east (tt) nil)

;; ===========================================================================
;; Question : is the east tile compatible with west tile ?
;; tile-east-west X Y
;;    EAST-TILE  WEST-TILE
;;     . . X    Y . . 
;;     . . X    Y . .
;;     . . X    Y . .
;;
;; we know tiles are 10 x 10 tiles

(define (tile-east-west? tt tt2)
  (let loop ((i 1))
    (cond
     ((> i 10) #t)
     (#t
      (let ((c (char=? (tile-xy tt 10 i) (tile-xy tt2 1 i))))
	(cond
	 (c (loop (+ i 1)))
	 (#t #f)))))))


;; tile-west-east X Y
;;     . . Y    X . . 
;;     . . Y    X . .
;;     . . Y    X . .
(define (tile-west-east? tt tt2)
  (tile-east-west? tt2 tt))

;; ---------------------------------------------------------------------------
(let ((g (car tiles)))
  (tile-east-west? g (flip-horz g)))

;; ============================================================================
;; tile-south-north X Y
;; . . .
;; . . . 
;; X X X 
;;
;; Y Y Y
;; . . .
;; . . .
(define (tile-south-north? tt tt2)
  (call/cc
   (lambda (exit)
     (let loop ((i 1))
       (cond
	((> i 10) #t)
	(#t
	 (let ((c (char=? (tile-xy tt i 10) (tile-xy tt2 i 1))))
	   (if (not c) (exit #f)
	       (loop (+ i 1)))))))
     #t)))


;; tile-north-south X Y
;; . . .
;; . . . 
;; X X X 
;;
;; Y Y Y
;; . . .
;; . . .
(define (tile-north-south? tt tt2)
  (tile-south-north? tt2 tt))

;; -------------------------------------------------------------------------------
(let ((g (car tiles)))
  (tile-east-west? g (flip-horz g)))

;; ================================================================================
;; * SIDE EFFECTS version of MAP *
;; apply f to each element of xs , but do not build a list at all
(define (fmap f xs)
  (cond
   ((null? xs) xs)
   (#t (f (car xs))
       (fmap f (cdr xs)))))

;; --------------------------------------------------------------------------------

;; --------------------------------------------------------------------------------


;; ================================================================================
;; (defmacro incf (x)
;;   (let ((s (gensym "s")))
;;     `(let ((,s ,x))
;;        (set! ,s (+ ,s 1)))))
(defmacro incf (x)
  `(set! ,x (+ ,x 1)))

(let ((x 1))
  (incf x)
  (list x x ))
;;----------------------------------------------------------------------------------


;;(assert "false" #f)

;; ----------------------------------------------------------------------------------

;; ================================================================================
;; try construct a 3 x 3 of tiles that satisfy example puzzle
;;
;; A B C
;; D E F
;; G H I
;;
;; guile >  (ftest)
;; fcount value after should be 9 ! --- factorial of 9  --- (factorial 9)   362880
;; nope ! 
;;
;; pick a tile from avaialble tiles
;; pick an orientation for that tile tiself
;;
;; ---------------------------------------------------------------------------------

(define f0count 0)

(define f1count 0)

(define f2count 0)

(define f3count 0)

(define f4count 0)

(define f5count 0)

(define f6count 0)

(define f7count 0)

(define f8count 0)

(define f9count 0)

(define solution-count 0)

(define (freset)
  (set! solution-count 0)
  (set! f0count 0)
  (set! f1count 0)
  (set! f2count 0)
  (set! f3count 0)
  (set! f4count 0)
  (set! f5count 0)
  (set! f6count 0)
  (set! f7count 0)
  (set! f8count 0)
  (set! f9count 0))

(define ftiles tiles)

(define (ftest)
  (freset)
  (f0 tiles)
  (format #t "fcounts = :0)~a :1)~a :2)~a :3)~a :4)~a :5)~a :6)~a :7)~a :8)~a :9)~a ~%"
	  f0count f1count f2count f3count f4count f5count f6count f7count f8count f9count))

(define (f0 tiles)
  (define (g x)
    (h (remove-one tile= x tiles) x))
  (define (h tiles x)
    (fmap (lambda (s) (f1 tiles s)) (all-tiles x)))
  (incf f0count)
  (begin (fmap g tiles) #f)
  #f)

;; nothing much to compare A to 
;; A 
(define (f1 tiles a)
  (define (g x)
    (h (remove-one tile= x tiles) a x))
  (define (h tiles a x)
    (fmap (lambda (s) (f2 tiles a s)) (all-tiles x)))
  (assert "f1 a is a tile" (tile? a))
  (assert "f1 tiles length is -1 original tiles" (= (length tiles) (- (length ftiles) 1)))
  (incf f1count)
  (begin (fmap g tiles) #f)
  #f)



;; A   *B*
(define (f2 tiles a b)
  (define (g x)
    (h (remove-one tile= x tiles) a b x))
  (define (h tiles a b x)
    (fmap (lambda (s) (f3 tiles a b s)) (all-tiles x)))
  (assert "f2 b is a tile" (tile? b))
  (assert "f2 tiles length is -2 original tiles" (= (length tiles) (- (length ftiles) 2)))
  (incf f2count)
  (if (tile-east-west? a b)
      (begin (fmap g tiles) #f)
      #f))

;; A   B   *C*
(define (f3 tiles a b c)
  (define (g x)
    (h (remove-one tile= x tiles) a b c x))
  (define (h tiles a b c x)
    (fmap (lambda (s) (f4 tiles a b c s)) (all-tiles x))) 
  (incf f3count)
  (assert "f3 c is a tile" (tile? c))
  (assert "f3 tiles length is -3 original tiles" (= (length tiles) (- (length ftiles) 3)))
  (if (tile-east-west? b c)
      (begin (fmap g tiles) #f)
      #f))



;; A   B   C
;; *D*     
(define (f4 tiles a b c d)
  (define (g x)
    (h (remove-one tile= x tiles) a b c d x))
  (define (h tiles a b c d x)
    (fmap (lambda (s) (f5 tiles a b c d s)) (all-tiles x))) 
  (assert "f4 tiles length is -4 original tiles" (= (length tiles) (- (length ftiles) 4)))
  (incf f4count)
  (if (tile-south-north? a d)
      (begin (fmap g tiles) #f)
      #f))



;; A   B   C
;; D   *E*   
(define (f5 tiles a b c d e)
  (define (g x)
    (h (remove-one tile= x tiles) a b c d e x))
  (define (h tiles a b c d e x)
    (fmap (lambda (s) (f6 tiles a b c d e s)) (all-tiles x))) ;; FCUT
  (incf f5count)
  (if (and (tile-east-west? d e)
	   (tile-south-north? b e))  
      (begin (fmap g tiles) #f)
      #f))

;; ============================== FCUT ====================================
(define (fcut6 tiles a b c d e f)
  (format #t "fcut6 : ~a ~%" (map tile-id (list a b c d e f))))



  
;; A   B   C
;; D   E   *F*
(define (f6 tiles a b c d e f)
  (define (g x)
    (h (remove-one tile= x tiles) a b c d e f x))
  (define (h tiles a b c d e f x)
    (fmap (lambda (s) (f7 tiles a b c d e f s)) (all-tiles x)))
  (incf f6count)
  (if (and (tile-east-west? e f)
	   (tile-south-north? c f))  
      (begin (fmap g tiles) #f)
      #f))


;; ============================== FCUT ====================================
(define (fcut7 tiles a b c d e f g)
  (format #t "fcut7 : ~a ~%" (map tile-id (list a b c d e f g))))

;; subtle shadow bug g is a tile 
;; A   B   C
;; D   E   F
;; *G* 
(define (f7 tiles a b c d e f g)
  (define (g! x)
    (h (remove-one tile= x tiles) a b c d e f g x))
  (define (h tiles a b c d e f g x)
    (fmap (lambda (s) (f8 tiles a b c d e f g s)) (all-tiles x)))
  (incf f7count)
  ;; (format #t "f7 : testing condition : d+g tiles ? ~a~%" (list (tile? d) (tile? g)))
  (let ((con (tile-south-north? d g)))
    ;;(format #t "f7 condition is ~a ~%" con)
  (if con
      (begin (fmap g! tiles) #f)
      #f)))


;; ============================== FCUT ====================================
(define (fcut8 tiles a b c d e f g h)
  (format #t "fcut8 : ~a ~%" (map tile-id (list a b c d e f g h))))


;; A   B   C
;; D   E   F
;; G  *H* 
(define (f8 tiles a b c d e f g h)
  (define (g! x)
    (h! (remove-one tile= x tiles) a b c d e f g h x))
  (define (h! tiles a b c d e f g h x)
    (fmap (lambda (s) (f9 tiles a b c d e f g h s)) (all-tiles x)))
  (incf f8count)
  (if (and (tile-east-west? g h)
	   (tile-south-north? e h))  
      (begin (fmap g! tiles) #f)
      #f))


;; ============================== FCUT ====================================
(define (fcut9 tiles a b c d e f g h i)
  (format #t "fcut9 : ~a ~%" (map tile-id (list a b c d e f g h i))))


;; this should be last one 
;; A   B   C
;; D   E   F
;; G   H   *I* 
(define (f9 tiles a b c d e f g h i)
;;  (if (null? tiles) 'ok (error "f9 should empty list tiles"))
  (incf f9count)
  (if (and (tile-east-west? h i)
	   (tile-south-north? f i))
      (f10 tiles a b c d e f g h i) ;; fcut
      #f))

;; ============================== FCUT ====================================
(define (fcut10 tiles a b c d e f g h i)
  (format #t "fcut10 : ~a ~%" (map tile-id (list a b c d e f g h i))))


;; A   B   C
;; D   E   F
;; G   H   I   ta * tc * tg * ti 
(define (f10 tiles a b c d e f g h i)
  (incf solution-count)
  (let ((product (apply * (map tile-id (list a c g i)))))
    (format #t "~%solution ~a : corner product is ~a ~%" solution-count product)
    (format #t "~a ~%" (map tile-id (list a b c)))
    (format #t "~a ~%" (map tile-id (list d e f)))
    (format #t "~a ~%" (map tile-id (list g h i)))
    (format #t "~%")))

;; -----------------------------------------------------------------------------



;; --------------------------------------------------------------------------------
;; this was the common lisp solution which worked for 3x3 but blew stack at 12x12
;; ;; for the given size of problem 3 x 3 
;; ;; A B C
;; ;; D E F
;; ;; G H I
;; (defun brute ()
;;   (let ((solution-count 0))
;;     (dolist (a tiles)
;;       (dolist (a2 (all-tiles a)) ;; A2
;; 	(let ((tiles (remove a tiles :test #'tile-eq)))
;; 	  (dolist (b tiles)
;; 	    (dolist (b2 (all-tiles b)) ;; B2 
;; 	      (when (tile-east-west a2 b2)  ;; A2 | B2 
;; 		(let ((tiles (remove b tiles :test #'tile-eq)))    	
;; 		  (dolist (c tiles)
;; 		    (dolist (c2 (all-tiles c)) ;; C2
;; 		      (when (tile-east-west b2 c2)  ;; A2 | B2 | C2
;; 			(let ((tiles (remove c tiles :test #'tile-eq)))
;; 			  (dolist (d tiles)
;; 			    (dolist (d2 (all-tiles d))
;; 			      ;; A2 | B2 | C2
;; 			      ;; D2
;; 			      (when (tile-south-north a2 d2)
;; 				(let ((tiles (remove d tiles :test #'tile-eq)))
;; 				  (dolist (e tiles)
;; 				    (dolist (e2 (all-tiles e)) ;; E2
;; 				      ;; A2 | B2 | C2
;; 				      ;; D2 | E2
;; 				      (when (and (tile-east-west d2 e2)
;; 						 (tile-south-north b2 e2))
;; 				    	(let ((tiles (remove e tiles :test #'tile-eq)))
;; 					  (dolist (f tiles)
;; 					    (dolist (f2 (all-tiles f)) ;; F2
;; 					      (let ((tiles (remove f tiles :test #'tile-eq)))
;; 						;; A2 | B2 | C2
;; 						;; D2 | E2 | F2
;; 						(when (and (tile-east-west e2 f2)
;; 							   (tile-south-north c2 f2))
;; 						  (dolist (g tiles)
;; 						    (dolist (g2 (all-tiles g)) ;; G2
;; 						      ;; A2 | B2 | C2
;; 						      ;; D2 | E2 | F2
;; 						      ;; G2 
;; 						      (when (tile-south-north d2 g2)						      
;; 							(let ((tiles (remove g tiles :test #'tile-eq)))
;; 							  (dolist (h tiles)
;; 							    (dolist (h2 (all-tiles h)) ;; H2
;; 							      ;; A2 | B2 | C2
;; 							      ;; D2 | E2 | F2
;; 							      ;; G2 | H2
;; 							      (when (and
;; 								     (tile-east-west g2 h2)
;; 								     (tile-south-north e2 h2))
;; 								(let ((tiles (remove h tiles :test #'tile-eq)))
;; 								  (dolist (i tiles)
;; 								    (dolist (i2 (all-tiles i)) ;; I2
;; 								      ;; A2 | B2 | C2
;; 								      ;; D2 | E2 | F2
;; 								      ;; G2 | H2 | I2
;; 								      (when (and
;; 									     (tile-east-west h2 i2)
;; 									     (tile-south-north f2 i2))								    
;; 									(let ((tiles (remove i tiles :test #'tile-eq)))
;; 									  (assert (null tiles))
;; 									  (format t "solution ~%")
;; 									  (format t "( ~a ~a ~a )~%" (tile-id a2) (tile-id b2) (tile-id c2))
;; 									  (format t "( ~a ~a ~a )~%" (tile-id d2) (tile-id e2) (tile-id f2))
;; 									  (format t "( ~a ~a ~a )~%" (tile-id g2) (tile-id h2) (tile-id i2))
;; 									  (format t " ==> ~a ~%" (* (tile-id a2) (tile-id c2) (tile-id g2) (tile-id i2)))									  
;; 									  (incf solution-count))))))))))))))))))))))))))))))))))))
;;     solution-count))


;; ;; 144 tiles
;; ;; grid 12 x 12
;; ;; x_1_1 is top left
;; ;; x_12_1 is top right
;; ;; x_1_12 is bottom left
;; ;; x_12_12 is bottom right
;; ;; x_1_1_a is actual orientation

;; (defmacro select (sym . body)
;;   (let ((sym2 (gensym (format nil "~a" sym))))
;;   `(dolist (,sym tiles)
;;      (dolist (,sym2 (all-tiles ,sym)) ;; pick an orientation
;;        (let ((tiles (remove ,sym tiles :test #'tile-eq)))
;; 	 (let ((,sym ,sym2))
;; 	   ;; do any specific
;; 	   ,@body))))))


;; ;;(macroexpand '(select a a2 (format t "a2 is ~a" a2)))

;; (defun brute2 ()
;;   (let ((solution-count 0))
;;     (select a 	    
;; 	    (select b 
;; 		    (when (tile-east-west a b)  ;; A | B
;; 		      (select c
;; 			      (when (tile-east-west b c)  ;; A | B | C
;; 				(select d 
;; 			      ;; A | B | C
;; 			      ;; D
;; 					(when (tile-south-north a d)
;; 					  (select e					
;; 				      ;; A2 | B2 | C2
;; 				      ;; D2 | E2
;; 				      (when (and (tile-east-west d e)
;; 						 (tile-south-north b e))
;; 					(select f 
;; 						;; A2 | B2 | C2
;; 						;; D2 | E2 | F2
;; 						(when (and (tile-east-west e f)
;; 							   (tile-south-north c f))
;; 						  (select g 
;; 						      ;; A2 | B2 | C2
;; 						      ;; D2 | E2 | F2
;; 						      ;; G2 
;; 							  (when (tile-south-north d g)
;; 							    (select h 
;; 							      ;; A2 | B2 | C2
;; 							      ;; D2 | E2 | F2
;; 							      ;; G2 | H2
;; 							      (when (and
;; 								     (tile-east-west g h)
;; 								     (tile-south-north e h))
;; 								(select i 
;; 								      ;; A2 | B2 | C2
;; 								      ;; D2 | E2 | F2
;; 								      ;; G2 | H2 | I2
;; 								      (when (and
;; 									     (tile-east-west h i)
;; 									     (tile-south-north f i))								    
;; 									  (assert (null tiles))
;; 									  (format t "solution ~%")
;; 									  (format t "( ~a ~a ~a )~%" (tile-id a) (tile-id b) (tile-id c))
;; 									  (format t "( ~a ~a ~a )~%" (tile-id d) (tile-id e) (tile-id f))
;; 									  (format t "( ~a ~a ~a )~%" (tile-id g) (tile-id h) (tile-id i))
;; 									  (format t " ==> ~a ~%" (* (tile-id a) (tile-id c) (tile-id g) (tile-id i)))									  
;; 									  (incf solution-count))))))))))))))))))
;;     solution-count))



;; ;; (defparameter *width* 3)
;; ;; (defparameter *height* 3)
;; ;; (defparameter lines (read-lines "../example.txt"))

;; (defparameter *width* 12)
;; (defparameter *height* 12)
;; (defparameter lines (read-lines "../input.txt"))

;; (defparameter tiles (mapcar (lambda (x) (list (car x) (conv (car (cdr x)))))
;; 			    (split-tiles lines)))

;; (defun internalize (str)
;;   (read-from-string str))


;; (defun check-fn (x y body)
;;   (let ((this (internalize (format nil "x_~a_~a" x y))))
;;     (cond
;;       ((and (< x 2) (< y 2)) ;; no left or up to do
;;        (funcall body))
;;       ((< y 2) ;; only left to check
;;        (let ((left (internalize (format nil "x_~a_~a" (- x 1) y))))
;; 	 `(when (tile-east-west ,left ,this)
;; 	    ,(funcall body))))      
;;       ((< x 2) ;; only up to check
;;        (let ((up (internalize (format nil "x_~a_~a" x (- y 1)))))	
;; 	 `(when (tile-south-north ,up ,this)
;; 	    ,(funcall body))))
;;       (t ;; check both up and left
;;        (let ((left (internalize (format nil "x_~a_~a" (- x 1) y)))
;; 	     (up (internalize (format nil "x_~a_~a" x (- y 1)))))
;; 	 `(when (and (tile-south-north ,up ,this)
;; 		     (tile-east-west ,left ,this))
;; 	    ,(funcall body)))))))



;; (defun select-fn (sym x y body)
;;   (let ((sym2 (gensym (format nil "~a" sym))))
;;     `(dolist (,sym tiles)
;;        (dolist (,sym2 (all-tiles ,sym)) ;; pick an orientation
;; 	 (let ((tiles (remove ,sym tiles :test #'tile-eq)))
;; 	   (let ((,sym ,sym2))
;; 	     ;; do any specific checking that needs to be done
;; 	     ,(check-fn x y body)))))))


;; (defun solution-row (y)
;;   (let ((exprs nil))
;;     (loop for x from 1 to *width* do
;;       (let ((sym (internalize (format nil "x_~a_~a" x y))))
;; 	;; just show id 
;; 	(setq exprs (cons `(format t "~a " (tile-id ,sym)) exprs))
;; 	;;(setq exprs (cons `(format t "~a " ,sym) exprs))
;; 	))
    
;;     (setq exprs (reverse exprs))
;;     (setq exprs (append exprs `((format t "~%"))))
;;     exprs))


	  

;; (defun solution-see ()
;;   (let ((exprs nil))
;;     (loop for y from 1 to *height* do
;;       (setq exprs (cons (solution-row y) exprs)))
;;     `(progn ,@(apply #'append (reverse exprs)))))






;; ;; generate a solver at runtime - macro in other words
;; (let ((width *width*)
;;       (height *height*))
;;   (defun create (x y)
;;     (cond
;;       ((> x width) (create 1 (+ y 1)))
;;       ((> y height) `(progn
;; 		       (assert (null tiles))
;; 		       (format t "solution ...~%")
;; 		       ,(solution-see)
;; 		       ))
;;       (t (let ((sym (internalize (format nil "x_~a_~a" x y))))
;; 	   (select-fn sym x y (lambda () (create (+ x 1) y))))))))


;; (defun generated ()
;;   (let ((gen (create 1 1)))
;;     (eval gen)))


#|

144 tiles to place

12 x 12 grid , need to fill that with different tile id's 

|#


;; take one of tiles fmap 
(define (ftrial tiles x y arr)
  (cond
   ((> x 12) (ftrial tiles 1 (+ y 1) arr))
   ((> y 12) (show-ftrial-solution tiles arr))
   (#t    
    (letrec ((foo (lambda (tt)
		    ;; for given tile in tiles , remove it from tiles 
		    (let ((new-tiles (remove-one tile= tt tiles)))
		      (assert "new-tiles vs tiles"
			      (= (length new-tiles) (- (length tiles) 1)))
		      ;; call bar with each of the tiles we can generate from tt
		      ;; choose orientation of tt 
		      (letrec ((bar (lambda (tt)
				      ;; put tt at x y
				      (array-set! arr tt x y)
				      ;; check consistency
				      (cond
				       ((and (> x 1)(> y 1))
					(let ((left (array-ref arr (- x 1) y))
					      (up (array-ref arr x (- y 1))))
					  (when (and (tile-east-west? left tt)
						   (tile-north-south? up tt))
					      (next))))
				       ((> y 1)
					(let ((up (array-ref arr x (- y 1))))
					  (when (tile-north-south? up tt)
					    (next))))
				       ((> x 1)
					(let ((left (array-ref arr (- x 1) y)))
					  (when (tile-east-west? left tt)
					    (next))))
				       ((and (= x 1)(= y 1))
					(next))
				       (#t (error "unhandled case FTRIAL " x y)))))
			       (next (lambda ()
				       ;; generic jump to next tile in array that needs to be filled in
      				       (ftrial new-tiles (+ x 1) y arr))))
			;; pass bar - each orientation of tt possible
			(fmap bar (all-tiles tt)))))))
      ;; apply foo to each available tile in tiles
      (fmap foo tiles)))))


(define (flarge)
  (let ((arr (make-tile-data 12 12)))
    (ftrial tiles 1 1 arr)))


(define show-ftrial-solution
  (let ((solution-count 0))
    (lambda (tiles arr)
      (format #t "~%")
      (incf solution-count)
      (format #t "solution ~a : " solution-count)
      (let* ((c1 (array-ref arr 1 1))
	     (c2 (array-ref arr 12 1))
	     (c3 (array-ref arr 1 12))
	     (c4 (array-ref arr 12 12))
	     (product (apply * (map tile-id (list c1 c2 c3 c4)))))
	(format #t " ~a ~%" (list c1 c2 c3 c4 'product-> product))
    
	(format #t "~%")
	(let loop ((x 1)(y 1))
	  (cond
	   ((> x 12)
	    (format #t "~%")
	    (loop 1 (+ y 1)))
	   ((> y 12) #f)
	   (#t
	    (let ((tt (array-ref arr x y)))
	      (format #t "~a " (tile-id tt))
	      (loop (+ x 1) y)))))))))


#|
guile > (flarge)

here are two solutions found ...

solution 1 :  ((#<procedure 7fb9d60020e0 at /home/terry/code/advent-of-code/advent-of-code-2020/day20/guile/large.scm:104:12 (x)> 3433 #2@1@1((. # . . . . . . . .) (. . . . . . . . . #) (# # . . . . . # # .) (# # . . . . . . . #) (. # . . . # # # . #) (# # . . # . . . . #) (# . # # . . . . # #) (. # . . . . . . . .) (. . . . . . # # # #) (# # . # . . . # # #))) (#<procedure 7fb9d6002c80 at /home/terry/code/advent-of-code/advent-of-code-2020/day20/guile/large.scm:104:12 (x)> 3833 #2@1@1((# . # . . # # # # #) (# . # . . # . . . #) (# # . . # . . . . .) (# . . . # # . . . #) (# . . # . # . . . #) (. . . . . . . . # .) (# . . . . . . . . .) (# . . . . . . . . .) (# . . # . . # # . .) (# . # # # . . . # .))) (#<procedure 7fb9c995f200 at /home/terry/code/advent-of-code/advent-of-code-2020/day20/guile/large.scm:104:12 (x)> 2011 #2@1@1((# # # # # . # . # .) (. . # . . . . # # .) (. . # . . . . . . #) (. . . . # . . . . #) (# . . # # # . . . #) (# . # . . . . . # .) (# # . . # . # . . .) (# . # . # . . # . #) (. # . # . . # . . #) (# . . # . . . . # .))) (#<procedure 7fb9d605f280 at /home/terry/code/advent-of-code/advent-of-code-2020/day20/guile/large.scm:104:12 (x)> 3001 #2@1@1((. . # . . . . . # .) (. . # . . . . . # .) (# . # # . # . . . #) (. # . # . . . . . #) (. . . . . # . . . #) (# . . . # # # . . #) (. . # . . . . # # #) (# . . . . . . # # #) (# . . . . . # . . #) (. . # # # . # . # .))) product-> 79412832860579) 

3433 2837 3613 2411 3643 1811 2671 3301 3881 1213 1013 3833 
1973 3307 1667 3853 3559 1559 3769 3169 2707 2281 1307 3943 
3319 1087 2273 1423 1889 3617 3257 3529 2447 1109 2393 2917 
2749 3821 2579 1949 1999 2081 2477 3217 2963 2521 1151 2791 
2801 2833 1873 1471 3517 2683 1097 2939 2531 2659 2719 1693 
3083 1091 3607 1009 3691 3359 3079 2351 2339 1103 3119 1051 
1451 1901 1289 2207 1709 2161 1979 3019 3583 2633 2063 1367 
2843 1447 2693 1381 2371 2137 1181 2053 2221 3947 3877 2851 
1867 1753 1847 1723 1223 1511 2909 2027 3547 1231 1163 2543 
3767 2731 1789 3701 2857 2239 1579 2089 1039 2143 2677 3413 
3491 1583 1567 2957 2557 1409 2269 1291 2417 2549 1063 1429 
2011 3793 2287 2423 1997 2003 2399 2099 3109 2111 1279 3001 

solution 2 :  ((#<procedure 7fb9d6002020 at /home/terry/code/advent-of-code/advent-of-code-2020/day20/guile/large.scm:104:12 (x)> 3433 #2@1@1((# # . # # # # . # .) (# # . # . . . # . .) (# # . . . # . # . .) (. # . . . # . . . .) (. . . . . # . . . .) (. . . . # . . . . .) (# . . # . . . . . .) (. . . # . . . . . .) (# . # . # # # # . #) (# . . # # . # # . .))) (#<procedure 7fb9d6059ca0 at /home/terry/code/advent-of-code/advent-of-code-2020/day20/guile/large.scm:104:12 (x)> 2011 #2@1@1((. # # . . # # # . .) (# . . . # . . . # #) (. . # . . . . . # .) (. # . # . . . . . #) (. . . . . # . . . .) (. . # # . # # . . #) (# # . . . # . . . #) (. . # . # . . # # #) (. # . # . . . . . #) (# . # # # # . . . #))) (#<procedure 7fb9d600b120 at /home/terry/code/advent-of-code/advent-of-code-2020/day20/guile/large.scm:104:12 (x)> 3833 #2@1@1((. . . . . # # . # #) (# . . . # . . . . #) (. # . . . . . . . #) (. # . . . . . . . #) (. . . . . # # . # #) (# . . . . . # # . .) (# # . . . # . . . .) (# . . . . . . . # #) (. . . . . . . # . .) (# # # # . # # # # #))) (#<procedure 7fb9d6077980 at /home/terry/code/advent-of-code/advent-of-code-2020/day20/guile/large.scm:104:12 (x)> 3001 #2@1@1((. # # # # # # # . .) (# . # # . . . . # #) (. . # # . . . . . .) (# # . . # . . . . .) (. . . . # # . # . .) (# . . . # . . . . .) (# . . . . . # # . .) (# . . # . . . # # #) (. . . . . . # . . .) (. # # . # . . # . .))) product-> 79412832860579) 

3433 1973 3319 2749 2801 3083 1451 2843 1867 3767 3491 2011 
2837 3307 1087 3821 2833 1091 1901 1447 1753 2731 1583 3793 
3613 1667 2273 2579 1873 3607 1289 2693 1847 1789 1567 2287 
2411 3853 1423 1949 1471 1009 2207 1381 1723 3701 2957 2423 
3643 3559 1889 1999 3517 3691 1709 2371 1223 2857 2557 1997 
1811 1559 3617 2081 2683 3359 2161 2137 1511 2239 1409 2003 
2671 3769 3257 2477 1097 3079 1979 1181 2909 1579 2269 2399 
3301 3169 3529 3217 2939 2351 3019 2053 2027 2089 1291 2099 
3881 2707 2447 2963 2531 2339 3583 2221 3547 1039 2417 3109 
1213 2281 1109 2521 2659 1103 2633 3947 1231 2143 2549 2111 
1013 1307 2393 1151 2719 3119 2063 3877 1163 2677 1063 1279 
3833 3943 2917 2791 1693 1051 1367 2851 2543 3413 1429 3001 


|#

  





















