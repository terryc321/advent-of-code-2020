

(ql:quickload :fiveam)

(defpackage :aoc
  (:use :cl :fiveam))

(in-package :aoc)




(test test-demo
  "This demonstrates the basic use of test and check."
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3)) "This should pass.")  ; &rest reason-args
  (is (= 4 4.1) "~D and ~D are not = to each other." 4 4.1))
;;(run! 'test-demo)


;; read lines from input.txt and parse them , then execute them ...
;;(defun lines


;; tokeniser : string -> [tokens]
;; parser : [tokens] -> expr-tree

;; make a parser

;; ( )
;; nums
;; + add 
;; * product


;; convert string to a list of characters 
(defun tokeniser (s)
  (let ((xs nil))
    (loop for i from 0 to (- (length s) 1) do
      (setq xs (cons (char s i) xs)))
    (setq xs (reverse xs))
    (tokenise xs)))

(let ((digit-chars '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
  (defun tokenise (xs)
    (cond
      ((null xs) xs)
      (t (let ((c (car xs)))
	   (cond
	     ((char= c #\() (cons 'open (tokenise (cdr xs))))
	     ((char= c #\)) (cons 'close (tokenise (cdr xs))))
	     ((char= c #\+) (cons 'add (tokenise (cdr xs))))
	     ((char= c #\*) (cons 'mul (tokenise (cdr xs))))
	     ((member c digit-chars)
	      (let ((peek (car (cdr xs))))
		(when (member peek digit-chars)
		  (error "multiple char nums not supported"))
		(cons (list 'num (read-from-string (format nil "~a" c)))
		      (tokenise (cdr xs)))))
	     (t (tokenise (cdr xs)))))))))

(defparameter *tokens* nil)
(defparameter *all-tokens* nil)
(defparameter *ps* nil)
(defparameter *parse-error* nil)
(defparameter *debug* nil)

(defun *status* (message)
  (when *debug*
    (format t "~%~%")
    (format t "message : ~a ~%" message)
    (format t "tokens => ~a ~%" *tokens*)
    (format t "p.stack => ~a ~%" *ps*)
    (break)))

(defun true-listp (x)
  (and (not (null x))
       (listp x)))


(defun advance ()
  (when (not (null *tokens*))
    (setq *tokens* (cdr *tokens*))))
    

(defun parse-num ()
  (let ((tok (car *tokens*)))
    (cond
      ((true-listp tok) ;; know (num N)
       (setq *ps* (cons (car (cdr tok)) *ps*))
       (advance))
      (t
       (format t "stuck on token -> ~a : ~a~%" tok *tokens*)
       (error "expected number in parse-num")))))
    

(defun parse-brak ()
  (let ((tok (car *tokens*)))
    (cond
      ((true-listp tok) ;; know (num N)
       (parse-num))
      ((eq tok 'open)
       (advance)
       (catch 'brak-end
       (loop while (not (null *tokens*)) do
	 (parse-loop)
	 (let ((tok2 (car *tokens*)))
	   (cond
	     ((eq tok2 'close)
	      (advance)
	      (throw 'brak-end t)))))))
      (t (error "expected either number or open parens")))))



;; 
(defun parse-add-mul ()  
  (parse-brak)
  (catch 'brak-end
    (loop while (not (null *tokens*)) do
      (let ((tok (car *tokens*)))
	(cond
	  ((eq tok 'add)
	   (*status* "parse-add-mul ADD")
	   (advance)
	   (parse-brak)
	   ;; pull two things from stack and do something
	   (assert (>= (length *ps*) 2))
	   (when (< (length *ps*) 2) (error "parse stack ADD missing operand"))
	   (let ((left (car *ps*))
		 (right (car (cdr *ps*))))
	     (setq *ps* (cdr (cdr *ps*)))
	     (setq *ps* (cons `(ADD ,right ,left) *ps*))))
	  ((eq tok 'mul)
	   (*status* "parse-add-mul MUL")
	   (advance)
	   (parse-brak)
	   ;; pull two things from stack and do something
	   (assert (>= (length *ps*) 2))
	   (when (< (length *ps*) 2) (error "parse stack MUL missing operand"))
	   (let ((left (car *ps*))
		 (right (car (cdr *ps*))))
	     (setq *ps* (cdr (cdr *ps*)))
	     (setq *ps* (cons `(MUL ,right ,left) *ps*))))
	  ((eq tok 'close)
	   (*status* "parse-add-mul CLOSE?")
	   ;; let parse-brak advance close
	   (throw 'brak-end t))
	  (t
	   (*status* "parse-add-mul stuck")
	   (format t "stuck on token -> ~a ~%" tok)
	   nil))))))



            
  

;; expect a number or open bracket
(defun parse-loop ()
  (*status* "PARSE-LOOP")
  (parse-add-mul))



(defun parse-entry (toks)
  (setq *parse-error* nil) ;; parse errors go here
  (setq *ps* nil) ;; parse stack
  (setq *all-tokens* toks) ;; all tokens initially
  (setq *tokens* toks) ;; 
  (parse-loop)
  ;; should only be one thing on the stack if all okay
  (assert (= (length *ps*) 1))
  (car *ps*))


(defun parse (s)
  (let ((toks (tokeniser s)))
    (parse-entry toks)))

    

;; line columns

    
  
