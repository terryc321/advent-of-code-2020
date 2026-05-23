;;
;; aoc 2020 day 23 - crab cups
(defpackage :fun
  (:use :cl))
(in-package :fun)

;; do we have cyclic datatype
;; roll own but as a library

;; play around with setf + cdr to make a circular list
;; but whatever do - do not let circular list out as printable or else it will cause crash
(eval-when (:load-toplevel :execute)
  (setq *print-circle* t))

;; do it whenever 
(setq *print-circle* t)

(format t "an infinite list ~A~%" (let ((p (list 1))) (setf (cdr p) p)))

;; five elements in a circular list
(defparameter example (let ((p (list 3 8 9 1 2 5 4 6 7)))
			(setf (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr p))))))))) p)))

;; we can cycle through list indefinitely
;; (setq example (cdr example))
;; #1=(8 9 1 2 5 4 6 7 3 . #1#)
;; FUN> (setq example (cdr example))
;; #1=(9 1 2 5 4 6 7 3 8 . #1#)
;; FUN> (setq example (cdr example))
;; #1=(1 2 5 4 6 7 3 8 9 . #1#)
;; FUN> (setq example (cdr example))
;; #1=(2 5 4 6 7 3 8 9 1 . #1#)
;; FUN> (setq example (cdr example))
;; #1=(5 4 6 7 3 8 9 1 2 . #1#)
;; FUN> (setq example (cdr example))
;; #1=(4 6 7 3 8 9 1 2 5 . #1#)

;; copy list -- does it preserve original list in face of setf + cdr - it should.

;; get contents of circular list - ie just those elements before any wrapping occurs

;; or we can ignore circular lists altogether and just use a flat vector 

;; remove next 3 elements from a circular list

;; puzzle input = 394618527 or 3 9 4 6 1 8 5 2 7

;; example input = 389125467 or 3 8 9 1 2 5 4 6 7
;;                            3 8 9 1 2 5 4 6 7
;;       current cup is       ||  the 3
;;  next three cups are removed ||||||  the 8 9 1
;;                             3 2 5 4 6 7      remain
;; find destination cup
;;  cup with label equal to cup minus 1 or the 2
;;                             3 2 5 4 6 7      remain
;;                              |||   destination cup is 2
;; insert cups picked up immediately after destination cup
;;                             3 2 8 9 1 5 4 6 7     
;;                                 |||||  8 9 1 inserted
;;  new current cup immediately clockwise of current cup
;;                             3 2 8 9 1 5 4 6 7     
;;                              |||  2 is immediately to right of current cup - the 3 

;; for the example
(defparameter c0 
  (let* ((p (list 3 8 9 1 2 5 4 6 7)))
    (setf (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr p))))))))) p)
    p))


(defparameter p0 
  (let* ((p (list 3 9 4 6 1 8 5 2 7)))
    (setf (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr p))))))))) p)
    p))
      

(defun move (circ)
  (dotimes (n 100)
    (let ((current-cup (car circ)))
      ;; remove next 3 items preserving order
      (format t "current cup is ~a~%" current-cup)
      (let* ((next (cdr circ))
	     (n1 (car next))
	     (n2 (car (cdr next)))
	     (n3 (car (cdr (cdr next))))
	     (after (cdr (cdr (cdr next)))))
	(format t "next 3 are ~a ~a ~a~%" n1 n2 n3)
	;; removing them circ -> n1 -> n2 -> n3 -> after
	;;               circ -> after 
	(setf (cdr circ) after)
	;; look for current-cup -1 down to 1 and then wrap around 9 8 7 6 5 4 3 2 1 ...
	(catch 'found
	  ;; find destination cup
	  (let ((d (- current-cup 1)))
	    (when (zerop d) (setq d 9))
	    (dotimes (i 3)
	      (when (or (= d n1)(= d n2) (= d n3))
		(setq d (- d 1))
		(when (zerop d) (setq d 9))))
	    ;; go find d the destination-cup 
	    (dotimes (i 10)
	      (setq circ (cdr circ))
	      (when (= (car circ) d)
		(throw 'found t))))
      	  ;; complain 
	  (error "how do i get here ?"))	
	;; circ -> after
	;; circ -> n1 -> n2 -> n3 -> after
	(let ((after (cdr circ))
	      (temp (list n1 n2 n3)))
	  (setf (cdr circ) temp)
	  (setf (cdr (cdr (cdr temp))) after)
	  ;; should be all to do
	  ;; cycle round so number after current-cup
	  (catch 'found 
	    (dotimes (i 10)
	      (setq circ (cdr circ))	      
	      (when (= (car circ) current-cup)
		(throw 'found t))))
	  ;; one more step - ready for next call to move 
	  (setq circ (cdr circ))
	  (format t "circ => ~A ~%" circ)))))
  circ)
	
;; circ => #1=(3 4 1 7 8 5 6 9 2 . #1#) 
;; #1=(3 4 1 7 8 5 6 9 2 . #1#)
;; FUN> 78569234       
;; answer -accepted !

(defconstant ten-thousand (expt 10 4))
(defconstant one-million (expt 10 6))
(defconstant ten-million (expt 10 7))

;; how long is 10 to million inclusive ?
;; how long is 10 to 100 inclusive ? 100 - 9 perhaps  => 91
(defun from-to(a b)
  (let ((xs nil))
    (loop for n from b downto a do
      (setq xs (cons n xs)))
    xs))

(defun count-from-to(a b)
  (let ((xs nil))
    (loop for n from b downto a do
      (setq xs (cons n xs)))
    (length xs)))


(let* ((p (list 1))) (setf (cdr p) p) p)
(let* ((p (list 1 2 3 4 5 6))(tail p)) (dotimes (n (+ -1 (length p)))
				     (setq tail (cdr tail)))
  (setf (cdr tail) p))


;;one-million))) -- generated easily -- now its circular !
(defun the-mill-puzzle ()
  ;;(declare (optimize (debug 3)))
  (let* ((p (list 3 9 4 6 1 8 5 2 7))
	 (rest-million (from-to 10 one-million))
	 (combined rest-million)) 
    (dolist (v (reverse p))
      (setq combined (cons v combined)))
    (let ((tail combined))
      (dotimes (n (+ -1 (length combined)))
	(setq tail (cdr tail)))
      (setf (cdr tail) combined)
      combined)))

(defun the-mill-example ()
  ;;(declare (optimize (debug 3)))
  (let* ((p (list 3 8 9 1 2 5 4 6 7))
	 (rest-million (from-to 10 one-million))
	 (combined rest-million)) 
    (dolist (v (reverse p))
      (setq combined (cons v combined)))
    ;; before circularisation  -- one million in length 
    ;; (format t "combined length is ~a ~%" (length combined))    
    (let ((tail combined))
      (dotimes (n (+ -1 (length combined)))
	(setq tail (cdr tail)))
      (setf (cdr tail) combined)
      combined)))


;; full-cycle is one million and ten , so we cover all possible values in there 
(defun move2 (circ)
  (let ((full-cycle (+ one-million 10)))
    (dotimes (n ten-million)
      (let ((current-cup (car circ)))
	;; remove next 3 items preserving order
	(when (zerop (rem n 10000)) (format t "current cup is ~a~%" current-cup))
	(let* ((next (cdr circ))
	       (n1 (car next))
	       (n2 (car (cdr next)))
	       (n3 (car (cdr (cdr next))))
	       (after (cdr (cdr (cdr next)))))
	  ;; (format t "next 3 are ~a ~a ~a~%" n1 n2 n3)
	  ;; removing them circ -> n1 -> n2 -> n3 -> after
	  ;;               circ -> after 
	  (setf (cdr circ) after)
	  ;; look for current-cup -1 down to 1 and then wrap around 9 8 7 6 5 4 3 2 1 ...
	  (catch 'found
	    ;; find destination cup
	    (let ((d (- current-cup 1)))
	      (when (zerop d) (setq d one-million))
	      (dotimes (i 3)
		(when (or (= d n1)(= d n2) (= d n3))
		  (setq d (- d 1))
		  (when (zerop d) (setq d one-million))))
	      ;; go find d the destination-cup 
	      (dotimes (i full-cycle)
		(setq circ (cdr circ))
		(when (= (car circ) d)
		  (throw 'found t))))
      	    ;; complain 
	    (error "how do i get here ?"))	
	  ;; circ -> after
	  ;; circ -> n1 -> n2 -> n3 -> after
	  (let ((after (cdr circ))
		(temp (list n1 n2 n3)))
	    (setf (cdr circ) temp)
	    (setf (cdr (cdr (cdr temp))) after)
	    ;; should be all to do
	    ;; cycle round so number after current-cup
	    (catch 'found 
	      (dotimes (i full-cycle)
		(setq circ (cdr circ))	      
		(when (= (car circ) current-cup)
		  (throw 'found t))))
	    ;; one more step - ready for next call to move 
	    (setq circ (cdr circ))
	    ;;(format t "circ => ~A ~%" circ)
	    ))))
    ;; cycle throu till find number 1 
    (catch 'found 
      (dotimes (i full-cycle)
	(setq circ (cdr circ))	      
	(when (= (car circ) 1)
	  (throw 'found t))))
    (setq circ (cdr circ))
    ;; find two cups after the number 1 
    (let ((cup1 (car circ))
	  (cup2 (car (cdr circ))))
      (list cup1 cup2 (* cup1 cup2)))))


(defun example2 ()
  (move2 (the-mill-example)))

(defun part2 ()
  (move2 (the-mill-puzzle)))

#|

for part 2 - ten thousand iterations took about 16 seconds 
theres a thousnad of them to do
so about 5 hrs to complete 

Evaluation took:
  15.946 seconds of real time
  14.811894 seconds of total run time (14.806021 user, 0.005873 system)
  92.89% CPU
  1 form interpreted
  58,785,007,675 processor cycles
  22,406,592 bytes consed
  
  before it was aborted by a non-local transfer of control.


15,000 seconds is equal to 4 hours and 10 minutes, which can also be expressed as 250 minutes or approximately 4.17 hours.  In terms of days, this duration is roughly 0.17 days. 

The conversion is derived by dividing the total seconds by 3,600 (the number of seconds in an hour): 15,000÷3,600=4.166... hours

Hours: 4 hours (with a remainder of 600 seconds)
Minutes: 10 minutes (600 seconds ÷ 60)
Days: ~0.174 days (15,000 seconds ÷ 86,400 seconds per day)

|#


	

      
  

