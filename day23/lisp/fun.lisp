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
;;

