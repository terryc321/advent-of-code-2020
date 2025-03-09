

;; in guile
(use-modules (ice-9 rdelim)) ;; read line ?
(use-modules (ice-9 pretty-print)) ;; show stuff nicely
(define pp pretty-print) 

;; what to import , what not to import 
(use-modules (srfi srfi-1)) ;; list 
(use-modules (srfi srfi-9)) ;; record types - user defined records data structures

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



;; ================================================================================
;; square grid implementation
;; drawback using scheme is either find module that does what want
;; write it again from scratch over and over
;; ===============================================================================
(define (make-grid wid hgt)
  (let ((array (make-array #\. `(1 ,wid) `(1 ,hgt))))
    array))

(define (grid-xy arr x y)
  (let* ((dim (array-dimensions arr))
	 (max-x (second (first dim)))
	 (max-y (second (second dim))))
    ;; (assert (and (>= x 1)(<= x max-x)))
    ;; (assert (and (>= y 1)(<= y max-y)))
    (array-ref arr x y)))


(define (grid-xy! arr x y v)
  (let* ((dim (array-dimensions arr))
	 (max-x (second (first dim)))
	 (max-y (second (second dim))))
    ;; (assert (and (>= x 1)(<= x max-x)))
    ;; (assert (and (>= y 1)(<= y max-y)))
    (array-set! arr v x y)))


;; join 10 strings together
;; ;; (apply string-append list-of-ten-strings) as input
(define (convert->grid s)
  (let* ((width 10)(height 10)(g (make-grid width height)))
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
(define tiles (split-tiles (read-lines "../example.txt")))


(set! tiles (map (lambda (x) (list (car x) (convert->grid (apply string-append (car (cdr x))))))
		 tiles))


;; ============================================================================



;; ===========================================================================
;; (define-record-type <tile>
;;   (make-tile id array)
;;   tile?
;;   (id    tile-id    set-tile-id!) 
;;   (array tile-array set-tile-array!))
;; (make-record-type "tile" (list 3 (make-grid 10 10)))


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



(define (tile-id tt)
  (car tt))

(define (tile-array tt)
  (car (cdr tt)))

(define (tile-xy tt x y)
  ;; (assert (>= x 1))
  ;; (assert (<= x 10))
  ;; (assert (>= y 1))
  ;; (assert (<= y 10))
  (let ((arr (tile-array tt)))
    (array-ref arr x y)))

(define (make-tile-array)
  (make-grid 10 10))

(define (show-tile tt)
  (let ((id (car tt))
	(g (car (cdr tt)))
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
       ((> y height) (list id g))
       ((> x width) (loop 1 (+ y 1) d))
       (#t
	(grid-xy! g x y d)
	(loop (+ x 1) y (+ d 1)))))))


;; clockwise 
(define (rotate-c tt)
  (let ((id (tile-id tt))
	(g (make-tile-array))
	(width 10)(height 10))
    (let loop ((x 1)(y 1))
      (cond
       ((> y height) (list id g))
       ((> x width) (loop 1 (+ y 1)))
       (#t
	(let ((d (tile-xy tt x y)))
	  (grid-xy! g (+ 1 (- 10 y)) x d)
	  (loop (+ x 1) y)))))))

;; counter clockwise
(define (rotate-cc tt)
  (let ((id (tile-id tt))
	(g (make-tile-array))
	(width 10)(height 10))
    (let loop ((x 1)(y 1))
      (cond
       ((> y height) (list id g))
       ((> x width) (loop 1 (+ y 1)))
       (#t
	(let ((d (tile-xy tt x y)))
	  (grid-xy! g y (+ 1 (- 10 x)) d)
	  (loop (+ x 1) y)))))))


;; flip horizontal
(define (flip-horz tt)
  (let ((id (tile-id tt))
	(g (make-tile-array))
	(width 10)(height 10))
    (let loop ((x 1)(y 1))
      (cond
       ((> y height) (list id g))
       ((> x width) (loop 1 (+ y 1)))
       (#t
	(let ((d (tile-xy tt x y)))
	  (grid-xy! g (+ 1 (- 10 x)) y d)
	  (loop (+ x 1) y)))))))

;; flip vertical
(define (flip-vert tt)
  (let ((id (tile-id tt))
	(g (make-tile-array))
	(width 10)(height 10))
    (let loop ((x 1)(y 1))
      (cond
       ((> y height) (list id g))
       ((> x width) (loop 1 (+ y 1)))
       (#t
	(let ((d (tile-xy tt x y)))
	  (grid-xy! g x (+ 1 (- 10 y)) d)
	  (loop (+ x 1) y)))))))


;; tile same if they have same array data
;; what does array-equal? mean
;; guile> ,describe array-equal?
(define (tile= tt tt2)
  (array-equal? (tile-array tt) (tile-array tt2)))

;; filter all unique tiles
(define (tile-member? x xs)
  ;; (format #t "tile-member? ~a ~a~%" x xs)
  (cond
   ((null? xs) #f)
   (#t (let ((tt (car xs)))
	 (cond
	  ((tile= x tt) #t)
	  (#t (tile-member? x (cdr xs))))))))


(define (filter-tiles2 xs ys zs)
  ;; (format #t "filter-tiles ~a ~a~%" xs ys)
  (cond
   ((null? xs) zs)
   (#t (let ((tt (car xs)))
	 (cond
	  ((tile-member? tt ys) (filter-tiles2 (cdr xs) ys zs))
	  ((tile-member? tt zs) (filter-tiles2 (cdr xs) ys zs))
	  (#t (filter-tiles2 (cdr xs) ys (cons tt zs))))))))

(define (filter-tiles xs ys)
  (filter-tiles2 xs ys '()))


;; -----------------------------------------------------------------------
;; ------- BUG : cannot figure out why not filtering correctly , just infinite loops
;; --------
;; ;; ======================= TO DO ===================================
;; ;; generate all possible unique tiles from single tile
;; ;; flip-v rotate-c
;; (define (all-tiles tt) 
;;     (define (create xs)
;;       (let* ((flipped (map flip-vert xs))
;; 	     (rotated (map rotate-c xs))
;; 	     (next (filter-tiles rotated (filter-tiles flipped xs))))
;; 	(cond
;; 	 ((= (length next) (length xs)) xs)
;; 	 (#t (create next)))))
;;     (create (list tt)))
;; ;; ====================================================================








;; ;; for a given set of tiles
;; (defun the-width () (floor (sqrt (length tiles))))
;; (defun the-height () (the-width))

;; ;; for the given size of problem (the-width) X  (the-height)
;; ;; A B C
;; ;; D E F
;; ;; G H I
;; (defun factorial (n)
;;   (let ((prod 1))
;;     (loop for i from 1 to n do
;;       (setq prod (* prod i)))
;;     prod))

;; #|

;; can see brute force search will not be great as size of inquiry becomes large

;; AOC> (factorial 9)
;; 362880
;; AOC> (factorial 144)
;; 5550293832739304789551054660550388117999982337982762871343070903773209740507907044212761943998894132603029642967578724274573160149321818341878907651093495984407926316593053871805976798524658790357488383743402086236160000000000000000000000000000000000
;; AOC> 
;; |#
	    

   
;; ;;(defun tile-east (tt) nil)

;; ;; tile-east-west X Y
;; ;;     . . X    Y . . 
;; ;;     . . X    Y . .
;; ;;     . . X    Y . .
;; (defun tile-east-west (tt tt2)
;;   (catch 'out
;;     (loop for i from 1 to 10 do
;;       (when (not (char= (tile-xy tt 10 i) (tile-xy tt2 1 i)))
;; 	(throw 'out nil)))	
;;     t))

;; ;; tile-west-east X Y
;; ;;     . . Y    X . . 
;; ;;     . . Y    X . .
;; ;;     . . Y    X . .
;; (defun tile-west-east (tt tt2)
;;   (tile-east-west tt2 tt))



;; ;; tile-south-north X Y
;; ;; . . .
;; ;; . . . 
;; ;; X X X 
;; ;;
;; ;; Y Y Y
;; ;; . . .
;; ;; . . .
;; (defun tile-south-north (tt tt2)
;;   (catch 'out
;;     (loop for i from 1 to 10 do
;;       (when (not (char= (tile-xy tt i 10) (tile-xy tt2 i 1)))
;; 	(throw 'out nil)))	
;;     t))

;; ;; tile-north-south X Y
;; ;; . . .
;; ;; . . . 
;; ;; X X X 
;; ;;
;; ;; Y Y Y
;; ;; . . .
;; ;; . . .
;; (defun tile-north-south (tt tt2)
;;   (tile-south-north tt2 tt))


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
























