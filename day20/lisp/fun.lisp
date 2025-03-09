
(defpackage :aoc
  (:use :cl))
(in-package :aoc)

(declaim (optimize (debug 0)(space 0)(speed 3)))


#|

blind strategy
pick A

A . .
. . .
. . .

pick B

A B .
. . .
. . .

check conflicts
......... is there redundant checking going on ?
... memoize ?
..................


tile has id , 2d grid 
rotate a tile 

thinking clearly.

getting the problem information correct - solve the problem posed .
if make an error reading problem spec does it   really matrter

read tiles from file
figure out if can match any edges together
tile can be rotated
tile can be flipped
how many orientations of tile yield a different tile
what size are tiles ?
how many tiles are in the input file
enumerate them
show a tile
show match between two tiles
suppose have

  a b c d e
a   ? ? ? ?
b     ? ? ?
c       ? ?
d         ?
e  

if have corner piece , only compatible on two sides

A - B
|
C

|#


;; read-lines
(defun read-lines (filename)
  (with-open-file (stream filename :direction :input)
    (let ((lines nil))
      (catch 'done
      (loop while t do
	(let ((line (read-line stream nil 'eof)))
	  (cond
	    ((eq line 'eof) (throw 'done (reverse lines)))
	    ((zerop (length line)) nil)
	    (t (setq lines (cons line lines))))))))))


;; split-tiles : [String] -> [[String]]
;; (defun split-tiles (xs)
;;   (split-tiles-helper xs nil nil))

;; ;; split-tiles-helper: [String] -> [String] -> [[String]]
;; (defun split-tiles-helper (xs tmps rs)
(defun split-tiles (xs)
  (let ((tiles nil))
    (catch 'done
      (loop while t do 
      (cond
	((null xs) (throw 'done t))
	(t (let ((s (car xs))) ;; s is a string	     
	     (cond
	       ((string= (subseq s 0 4) "Tile")
		(let ((id (read-from-string (subseq s 5 (- (length s) 1)))))
		  ;;(format t "id = ~a ~%" id)
		  (setq xs (cdr xs)) ;; drop Tile ID: 
		  (let ((array-string ""))
		    (loop for i from 1 to 10 do
		      (setq array-string (concatenate 'string array-string (car xs)))
		      (setq xs (cdr xs)) ;; drop
			  )
		    (setq tiles (cons (list id array-string) tiles)))))))))))
    (reverse tiles)))

(defun make-tile-array ()
  (make-array '(11 11) :initial-element #\.))

(defun conv (s)
  (assert (= (length s) 100))
  (let ((array (make-tile-array))
	(x 1)(y 1)(wid 10))
    (loop for i from 1 to 100 do
      (let ((ch (char s (- i 1))))
	(setf (aref array x y) ch)
	(incf x)
	(when (> x wid) (setq x 1) (incf y))))
    array))


(defun tile-id (tt)
  (car tt))

(defun tile-array (tt)
  (car (cdr tt)))

(defun tile-xy (tt x y)
  (assert (>= x 1))
  (assert (<= x 10))
  (assert (>= y 1))
  (assert (<= y 10))
  (let ((arr (car (cdr tt))))
    (aref arr x y)))

;; counter-clockwise 
(defun rotate-cc (tt)
  (let ((id (tile-id tt))
	(arr (make-tile-array)))
    (loop for x from 1 to 10 do
      (loop for y from 1 to 10 do
	(let ((d (tile-xy tt x y)))
	  (setf (aref arr (+ 1 (- 10 y)) x) d))))
    (list id arr)))

;; clockwise rotate
(defun rotate-c (tt)
  (let ((id (tile-id tt))
	(arr (make-tile-array)))
    (loop for x from 1 to 10 do
      (loop for y from 1 to 10 do
	(let ((d (tile-xy tt x y)))
	  (setf (aref arr y (+ 1 (- 10 x))) d))))
    (list id arr)))

;; flip horizontal
(defun flip-h (tt)
  (let ((id (tile-id tt))
	(arr (make-tile-array)))
    (loop for x from 1 to 10 do
      (loop for y from 1 to 10 do
	(let ((d (tile-xy tt x y)))
	  (setf (aref arr x (+ 1 (- 10 y))) d))))
    (list id arr)))

;; flip vertical 
(defun flip-v (tt)
  (let ((id (tile-id tt))
	(arr (make-tile-array)))
    (loop for x from 1 to 10 do
      (loop for y from 1 to 10 do
	(let ((d (tile-xy tt x y)))
	  (setf (aref arr (+ 1 (- 10 x)) y) d))))
    (list id arr)))


;; tile same if they have same array data
(defun tile-eq (tt tt2)
  (equalp (tile-array tt) (tile-array tt2)))

;; generate all tiles from a single tile
;; flip-v rotate-c
(defun all-tiles (tt)
  (let ((all (list tt))
	(created 1))
    (loop while (> created 0) do
      (setq created 0)
      (let ((flipped (mapcar #'flip-v all))
	    (rotated (mapcar #'rotate-c all)))
	(dolist (f flipped)
	  (cond
	    ((member f all :test #'tile-eq) nil)
	    (t
	     (incf created)
	     (setq all (cons f all)))))
	(dolist (f rotated)
	  (cond
	    ((member f all :test #'tile-eq) nil)
	    (t
	     (incf created)
	     (setq all (cons f all)))))))
    all))


(defun test-tile ()
  (let ((id 0)
	(arr (make-tile-array))
	(n 1))
    (loop for x from 1 to 10 do
      (loop for y from 1 to 10 do
	(setf (aref arr x y) n)
	(incf n)))
    (list id arr)))


;; for a given set of tiles
(defun the-width () (floor (sqrt (length tiles))))
(defun the-height () (the-width))

;; for the given size of problem (the-width) X  (the-height)
;; A B C
;; D E F
;; G H I
(defun factorial (n)
  (let ((prod 1))
    (loop for i from 1 to n do
      (setq prod (* prod i)))
    prod))

#|

can see brute force search will not be great as size of inquiry becomes large

AOC> (factorial 9)
362880
AOC> (factorial 144)
5550293832739304789551054660550388117999982337982762871343070903773209740507907044212761943998894132603029642967578724274573160149321818341878907651093495984407926316593053871805976798524658790357488383743402086236160000000000000000000000000000000000
AOC> 
|#
	    

   
;;(defun tile-east (tt) nil)

;; tile-east-west X Y
;;     . . X    Y . . 
;;     . . X    Y . .
;;     . . X    Y . .
(defun tile-east-west (tt tt2)
  (catch 'out
    (loop for i from 1 to 10 do
      (when (not (char= (tile-xy tt 10 i) (tile-xy tt2 1 i)))
	(throw 'out nil)))	
    t))

;; tile-west-east X Y
;;     . . Y    X . . 
;;     . . Y    X . .
;;     . . Y    X . .
(defun tile-west-east (tt tt2)
  (tile-east-west tt2 tt))



;; tile-south-north X Y
;; . . .
;; . . . 
;; X X X 
;;
;; Y Y Y
;; . . .
;; . . .
(defun tile-south-north (tt tt2)
  (catch 'out
    (loop for i from 1 to 10 do
      (when (not (char= (tile-xy tt i 10) (tile-xy tt2 i 1)))
	(throw 'out nil)))	
    t))

;; tile-north-south X Y
;; . . .
;; . . . 
;; X X X 
;;
;; Y Y Y
;; . . .
;; . . .
(defun tile-north-south (tt tt2)
  (tile-south-north tt2 tt))


;; for the given size of problem 3 x 3 
;; A B C
;; D E F
;; G H I
(defun brute ()
  (let ((solution-count 0))
    (dolist (a tiles)
      (dolist (a2 (all-tiles a)) ;; A2
	(let ((tiles (remove a tiles :test #'tile-eq)))
	  (dolist (b tiles)
	    (dolist (b2 (all-tiles b)) ;; B2 
	      (when (tile-east-west a2 b2)  ;; A2 | B2 
		(let ((tiles (remove b tiles :test #'tile-eq)))    	
		  (dolist (c tiles)
		    (dolist (c2 (all-tiles c)) ;; C2
		      (when (tile-east-west b2 c2)  ;; A2 | B2 | C2
			(let ((tiles (remove c tiles :test #'tile-eq)))
			  (dolist (d tiles)
			    (dolist (d2 (all-tiles d))
			      ;; A2 | B2 | C2
			      ;; D2
			      (when (tile-south-north a2 d2)
				(let ((tiles (remove d tiles :test #'tile-eq)))
				  (dolist (e tiles)
				    (dolist (e2 (all-tiles e)) ;; E2
				      ;; A2 | B2 | C2
				      ;; D2 | E2
				      (when (and (tile-east-west d2 e2)
						 (tile-south-north b2 e2))
				    	(let ((tiles (remove e tiles :test #'tile-eq)))
					  (dolist (f tiles)
					    (dolist (f2 (all-tiles f)) ;; F2
					      (let ((tiles (remove f tiles :test #'tile-eq)))
						;; A2 | B2 | C2
						;; D2 | E2 | F2
						(when (and (tile-east-west e2 f2)
							   (tile-south-north c2 f2))
						  (dolist (g tiles)
						    (dolist (g2 (all-tiles g)) ;; G2
						      ;; A2 | B2 | C2
						      ;; D2 | E2 | F2
						      ;; G2 
						      (when (tile-south-north d2 g2)						      
							(let ((tiles (remove g tiles :test #'tile-eq)))
							  (dolist (h tiles)
							    (dolist (h2 (all-tiles h)) ;; H2
							      ;; A2 | B2 | C2
							      ;; D2 | E2 | F2
							      ;; G2 | H2
							      (when (and
								     (tile-east-west g2 h2)
								     (tile-south-north e2 h2))
								(let ((tiles (remove h tiles :test #'tile-eq)))
								  (dolist (i tiles)
								    (dolist (i2 (all-tiles i)) ;; I2
								      ;; A2 | B2 | C2
								      ;; D2 | E2 | F2
								      ;; G2 | H2 | I2
								      (when (and
									     (tile-east-west h2 i2)
									     (tile-south-north f2 i2))								    
									(let ((tiles (remove i tiles :test #'tile-eq)))
									  (assert (null tiles))
									  (format t "solution ~%")
									  (format t "( ~a ~a ~a )~%" (tile-id a2) (tile-id b2) (tile-id c2))
									  (format t "( ~a ~a ~a )~%" (tile-id d2) (tile-id e2) (tile-id f2))
									  (format t "( ~a ~a ~a )~%" (tile-id g2) (tile-id h2) (tile-id i2))
									  (format t " ==> ~a ~%" (* (tile-id a2) (tile-id c2) (tile-id g2) (tile-id i2)))									  
									  (incf solution-count))))))))))))))))))))))))))))))))))))
    solution-count))


;; 144 tiles
;; grid 12 x 12
;; x_1_1 is top left
;; x_12_1 is top right
;; x_1_12 is bottom left
;; x_12_12 is bottom right
;; x_1_1_a is actual orientation

(defmacro select (sym . body)
  (let ((sym2 (gensym (format nil "~a" sym))))
  `(dolist (,sym tiles)
     (dolist (,sym2 (all-tiles ,sym)) ;; pick an orientation
       (let ((tiles (remove ,sym tiles :test #'tile-eq)))
	 (let ((,sym ,sym2))
	   ;; do any specific
	   ,@body))))))


;;(macroexpand '(select a a2 (format t "a2 is ~a" a2)))

(defun brute2 ()
  (let ((solution-count 0))
    (select a 	    
	    (select b 
		    (when (tile-east-west a b)  ;; A | B
		      (select c
			      (when (tile-east-west b c)  ;; A | B | C
				(select d 
			      ;; A | B | C
			      ;; D
					(when (tile-south-north a d)
					  (select e					
				      ;; A2 | B2 | C2
				      ;; D2 | E2
				      (when (and (tile-east-west d e)
						 (tile-south-north b e))
					(select f 
						;; A2 | B2 | C2
						;; D2 | E2 | F2
						(when (and (tile-east-west e f)
							   (tile-south-north c f))
						  (select g 
						      ;; A2 | B2 | C2
						      ;; D2 | E2 | F2
						      ;; G2 
							  (when (tile-south-north d g)
							    (select h 
							      ;; A2 | B2 | C2
							      ;; D2 | E2 | F2
							      ;; G2 | H2
							      (when (and
								     (tile-east-west g h)
								     (tile-south-north e h))
								(select i 
								      ;; A2 | B2 | C2
								      ;; D2 | E2 | F2
								      ;; G2 | H2 | I2
								      (when (and
									     (tile-east-west h i)
									     (tile-south-north f i))								    
									  (assert (null tiles))
									  (format t "solution ~%")
									  (format t "( ~a ~a ~a )~%" (tile-id a) (tile-id b) (tile-id c))
									  (format t "( ~a ~a ~a )~%" (tile-id d) (tile-id e) (tile-id f))
									  (format t "( ~a ~a ~a )~%" (tile-id g) (tile-id h) (tile-id i))
									  (format t " ==> ~a ~%" (* (tile-id a) (tile-id c) (tile-id g) (tile-id i)))									  
									  (incf solution-count))))))))))))))))))
    solution-count))



;; (defparameter *width* 3)
;; (defparameter *height* 3)
;; (defparameter lines (read-lines "../example.txt"))

(defparameter *width* 12)
(defparameter *height* 12)
(defparameter lines (read-lines "../input.txt"))

(defparameter tiles (mapcar (lambda (x) (list (car x) (conv (car (cdr x)))))
			    (split-tiles lines)))

(defun internalize (str)
  (read-from-string str))


(defun check-fn (x y body)
  (let ((this (internalize (format nil "x_~a_~a" x y))))
    (cond
      ((and (< x 2) (< y 2)) ;; no left or up to do
       (funcall body))
      ((< y 2) ;; only left to check
       (let ((left (internalize (format nil "x_~a_~a" (- x 1) y))))
	 `(when (tile-east-west ,left ,this)
	    ,(funcall body))))      
      ((< x 2) ;; only up to check
       (let ((up (internalize (format nil "x_~a_~a" x (- y 1)))))	
	 `(when (tile-south-north ,up ,this)
	    ,(funcall body))))
      (t ;; check both up and left
       (let ((left (internalize (format nil "x_~a_~a" (- x 1) y)))
	     (up (internalize (format nil "x_~a_~a" x (- y 1)))))
	 `(when (and (tile-south-north ,up ,this)
		     (tile-east-west ,left ,this))
	    ,(funcall body)))))))



(defun select-fn (sym x y body)
  (let ((sym2 (gensym (format nil "~a" sym))))
    `(dolist (,sym tiles)
       (dolist (,sym2 (all-tiles ,sym)) ;; pick an orientation
	 (let ((tiles (remove ,sym tiles :test #'tile-eq)))
	   (let ((,sym ,sym2))
	     ;; do any specific checking that needs to be done
	     ,(check-fn x y body)))))))


(defun solution-row (y)
  (let ((exprs nil))
    (loop for x from 1 to *width* do
      (let ((sym (internalize (format nil "x_~a_~a" x y))))
	;; just show id 
	(setq exprs (cons `(format t "~a " (tile-id ,sym)) exprs))
	;;(setq exprs (cons `(format t "~a " ,sym) exprs))
	))
    
    (setq exprs (reverse exprs))
    (setq exprs (append exprs `((format t "~%"))))
    exprs))


	  

(defun solution-see ()
  (let ((exprs nil))
    (loop for y from 1 to *height* do
      (setq exprs (cons (solution-row y) exprs)))
    `(progn ,@(apply #'append (reverse exprs)))))






;; generate a solver at runtime - macro in other words
(let ((width *width*)
      (height *height*))
  (defun create (x y)
    (cond
      ((> x width) (create 1 (+ y 1)))
      ((> y height) `(progn
		       (assert (null tiles))
		       (format t "solution ...~%")
		       ,(solution-see)
		       ))
      (t (let ((sym (internalize (format nil "x_~a_~a" x y))))
	   (select-fn sym x y (lambda () (create (+ x 1) y))))))))


(defun generated ()
  (let ((gen (create 1 1)))
    (eval gen)))
























