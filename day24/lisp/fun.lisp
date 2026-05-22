
(defpackage :fun
  (:use :cl))
(in-package :fun)

;; example.txt
;; input.txt

(defun pparse (filename)
  (catch 'done
    (with-open-file (stream filename :direction :input)
      (let ((lines nil))
	(loop while t do
	  (let ((line (read-line stream nil nil)))
	    (cond
	      (line (setq lines (cons line lines)))
	      (t (throw 'done (reverse lines))))))))))

(defparameter *input-lines* (pparse "../input.txt"))
(defparameter *example-lines* (pparse "../example.txt"))

;; break string into directions to move
;; e, se, sw, w, nw, and ne.

(defun as-directions (str)
  (catch 'done
    (let ((directions nil))
      (labels ((add-direction (sym)
		 (setq directions (cons sym directions))))
	(with-input-from-string (stream str)
	  (loop while t do
	    (let ((char1 (read-char stream nil :eof)))
	      (cond
		((eq char1 :eof) (throw 'done (reverse directions)))
		((char= char1 #\e) (add-direction :e))
		((char= char1 #\w) (add-direction :w))
		(t (let ((char2 (read-char stream nil :eof)))
		     (cond
		       ((eq char2 :eof) (error "expected a direction"))
		       ((and (char= char1 #\n) (char= char2 #\w)) (add-direction :nw))
		       ((and (char= char1 #\n) (char= char2 #\e)) (add-direction :ne))
		       ((and (char= char1 #\s) (char= char2 #\e)) (add-direction :se))
		       ((and (char= char1 #\s) (char= char2 #\w)) (add-direction :sw))
		       (t (error "expected a direction")))))))))))))


(defparameter *input-directions* (mapcar #'as-directions input-lines))
(defparameter *example-directions* (mapcar #'as-directions example-lines))

;; hexagon
;; from some arbitrary start position 0 0
;; take Y axis as being upwards (almost opposite from most drawing applications)
;; north 0 degrees
;;
;; hexagon 360 divided by 6 gives internal angle of 60 degrees 
;;
;; ne bearing 30 degrees     ing say 30 degrees from vertical - clockwise rotation .
;; e bearing  90 degrees 
;; se bearing 150 degrees
;; sw bearing 210 degrees
;; w  bearing 270 degrees 
;; nw bearing 330 degrees
;;   true north 360 degrees - gives another 30 degrees
;;

(defparameter *radius* 10)
;; radius of 10 units per each move 


(defun degrees-to-radians (d) (* d (/ (* 2 pi) 360)))

;; a position - x y
(defstruct pos
  x
  y)

(defun north-east(p)
  (let* ((radius *radius*)
	 (x (pos-x p))
	 (y (pos-y p))
	 (dx (* radius (cos (degrees-to-radians 60))))
	 (dy (* radius (sin (degrees-to-radians 60)))))	 
    (make-pos :x (+ x dx) :y (+ y dy))))

(defun south-east(p)
  (let* ((radius *radius*)
	 (x (pos-x p))
	 (y (pos-y p))
	 (dx (* radius (cos (degrees-to-radians 60))))
	 (dy (* radius (sin (degrees-to-radians 60)))))	 
    (make-pos :x (+ x dx) :y (- y dy))))

(defun north-west(p)
  (let* ((radius *radius*)
	 (x (pos-x p))
	 (y (pos-y p))
	 (dx (* radius (cos (degrees-to-radians 60))))
	 (dy (* radius (sin (degrees-to-radians 60)))))	 
    (make-pos :x (- x dx) :y (+ y dy))))

(defun south-west(p)
  (let* ((radius *radius*)
	 (x (pos-x p))
	 (y (pos-y p))
	 (dx (* radius (cos (degrees-to-radians 60))))
	 (dy (* radius (sin (degrees-to-radians 60)))))	 
    (make-pos :x (- x dx) :y (- y dy))))


(defun east(p)
  (let* ((radius *radius*)
	 (x (pos-x p))
	 (y (pos-y p))
	 (dx radius)
	 (dy 0))
    (make-pos :x (+ x dx) :y (+ y dy))))

(defun west(p)
  (let* ((radius *radius*)
	 (x (pos-x p))
	 (y (pos-y p))
	 (dx radius)
	 (dy 0))
    (make-pos :x (- x dx) :y (+ y dy))))

(defun same-pos(p p2)
  (let ((delta 1e-5))
    (and (< (abs (- (pos-x p)(pos-x p2))) delta)
	 (< (abs (- (pos-y p)(pos-y p2))) delta))))

(same-pos (make-pos :x 0 :y 0) (west (south-east (north-east (make-pos :x 0 :y 0)))))

;; The tiles are all white on one side and black on the other.
;; They start with the white side facing up.
;; The lobby is large enough to fit whatever pattern might need to appear there.

;; if we only keep track of which tiles are black - we can exclude infinite number of tiles
;; if a tile becomes white - we forget it - automatically assumed white if not in the table
;; we want a hash table or graph capable of using position coordinate
;; floating point treating it as same graph vertex if below a delta difference (x@y) 

;; a tile has a side - either black or white and a position pos
(defstruct tile
  side
  pos)

(defparameter *tiles* (make-array 100 :fill-pointer 0 :adjustable t))

;; vector-push-extend
;; ;; Create an adjustable vector with a fill pointer
;; (defparameter *v* (make-array 2 :fill-pointer 0 :adjustable t))

;; ;; Push elements; the vector extends automatically when full
;; (vector-push-extend 1 *v*) => 0
;; (vector-push-extend 2 *v*) => 1
;; (vector-push-extend 3 *v*) => 2 ;; Vector expands here   

(defun run (directions)
  (let ((black-positions nil))
    (dolist (direction-list directions)
      ;; lets start from a central location - origin 0,0
      (let ((pos (make-pos :x 0 :y 0)))
	(dolist (direction direction-list)
	  (cond
	    ((eq direction :ne) (setq pos (north-east pos)))
	    ((eq direction :e) (setq pos (east pos)))
	    ((eq direction :se) (setq pos (south-east pos)))
	    ((eq direction :sw) (setq pos (south-west pos)))
	    ((eq direction :w) (setq pos (west pos)))
	    ((eq direction :nw) (setq pos (north-west pos)))
	    (t (error "bad direction"))))
	;; end up somewhere - flip this (remove it from known black positions)
	;; or if not in list - add it to list 
	(catch 'flipped 
	  (dolist (black black-positions)
	    (when (same-pos black pos)
	      (setq black-positions (remove black black-positions :test #'same-pos))
	      (throw 'flipped t)))
	  (setq black-positions (cons pos black-positions)))))
    black-positions))

(defun example ()
  (run *example-directions*))

(defun part1 ()
  (run *input-directions*))

;; part 1 - accepted answer
;; (length (example))
;; 10
;; (length (part1))
;; 277

;; part 2 
;;Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
;;Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.

;; positions only held for black tiles 
(defun one-day (black-positions)
  (let ((removal-list nil)
	(insertion-list nil)
	(white-positions nil)
	(fn-directions (list #'north-east #'east #'south-east
			     #'south-west #'west #'north-west)))
    (labels ((add-position (p)  (setq white-positions (cons p white-positions))))
      ;; construct removal list of positions of black positions to be eliminated
      (dolist (p black-positions)
	(let ((neighbour 0))
	  (when (member (north-east p) black-positions :test #'same-pos) (incf neighbour))
	  (when (member (east p) black-positions :test #'same-pos) (incf neighbour))
	  (when (member (south-east p) black-positions :test #'same-pos) (incf neighbour))
	  (when (member (south-west p) black-positions :test #'same-pos) (incf neighbour))
	  (when (member (west p) black-positions :test #'same-pos) (incf neighbour))
	  (when (member (north-west p) black-positions :test #'same-pos) (incf neighbour))
	  (when (or (= neighbour 0)
		    (> neighbour 2))
	    (setq removal-list (cons p removal-list)))))

      ;; find adjacent white tiles - any reachable from black positions
      ;; - not black - ie not in black positions
      (dolist (p black-positions)
	(loop for fn in fn-directions do
	  (let ((adjacent (funcall fn p)))
	    (when (not (member adjacent black-positions :test #'same-pos))
	      (add-position adjacent)))))

      ;; for each white neighbour tile - if it has 2 black neighbours -
      ;; include it for insertion list 
      (dolist (p white-positions)
	(let ((neighbour 0))
	  (when (member (north-east p) black-positions :test #'same-pos) (incf neighbour))
	  (when (member (east p) black-positions :test #'same-pos) (incf neighbour))
	  (when (member (south-east p) black-positions :test #'same-pos) (incf neighbour))
	  (when (member (south-west p) black-positions :test #'same-pos) (incf neighbour))
	  (when (member (west p) black-positions :test #'same-pos) (incf neighbour))
	  (when (member (north-west p) black-positions :test #'same-pos) (incf neighbour))
	  (when (= neighbour 2)
	    (setq insertion-list (cons p insertion-list)))))
      ;;
      (let ((result black-positions))
	(loop for p in removal-list do
	  (setq result (remove p result :test #'same-pos)))
	(loop for p in insertion-list do
	  (when (not (member p result :test #'same-pos))
	    (setq result (cons p result))))
	result))))

(defun example2 ()
  (let ((black-positions (example)))
    (loop for i from 1 to 100 do
      (setq black-positions (one-day black-positions))
      (format t "day ~a => ~a~%" i (length black-positions)))))

(defun part2 ()
  (let ((black-positions (part1)))
    (loop for i from 1 to 100 do
      (setq black-positions (one-day black-positions))
      (format t "day ~a => ~a~%" i (length black-positions)))))


;; slow down on iterating over lists 
;; part2 - accepted answer
;; day 100 => 3531



      
      
	
    
      
	
    


    
    







	       
	       
	       

