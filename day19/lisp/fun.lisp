

(ql:quickload :fiveam)

(defpackage :aoc
  (:use :cl :fiveam))

(in-package :aoc)

#|

NUM colon:  NUM* ( | NUM* )?*

|#

;; implicit in haskell 
;; string -> [char] 
(defun tokeniser (s)
  (let ((xs nil))
    (loop for i from 0 to (- (length s) 1) do
      (setq xs (cons (char s i) xs)))
    (setq xs (reverse xs))
    (tokenise xs)))

;; tokenise a list of characters
(let ((digit-chars '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
  (defun tokenise (xs)
    (cond
      ((null xs) xs)
      (t (let ((c (car xs)))
	   (cond
	     ((char= c #\:) (cons 'colon (tokenise (cdr xs))))
	     ((char= c #\|) (cons 'or (tokenise (cdr xs))))
	     ((char= c #\") (cons `(LETTER ,(second xs)) (tokenise (cdr (cdr (cdr xs))))))     
	     ((member c digit-chars)
	      (let ((nums (list c)))
		(setq xs (cdr xs))
		(let ((val (cond
			     ((null xs) (read-from-string (format nil "~a" c)))
			     (t
			      (catch 'out
				(loop while (not (null xs)) do
				  (let ((at (car xs)))
				    (cond
				      ((member at digit-chars)
				       (setq nums (cons at nums))
				       (setq xs (cdr xs))
				       (if (null xs) ;; edge case last num also coerce
					   (throw 'out (read-from-string
							(coerce (reverse nums) 'string)))))
				   (t
				    (throw 'out (read-from-string
						 (coerce (reverse nums) 'string))))))))))))
			   (cons (list 'num val) (tokenise xs)))))
	     (t ;; ignore blanks
	      (tokenise (cdr xs)))))))))








(defparameter *tokens* nil)
(defparameter *all-tokens* nil)

(defparameter *nums* nil)
(defparameter *ors* nil)

(defparameter *parse-error* nil)
(defparameter *debug* nil)

(defun true-listp (x)
  (and (not (null x))
       (listp x)))


(defun advance ()
  (when (not (null *tokens*))
    (setq *tokens* (cdr *tokens*))))


;; numbers get added to *nums* stack
(defun parse-num ()
  (catch 'out
    (loop while (not (null *tokens*)) do
      (let ((tok (car *tokens*)))
	(cond
	  ((true-listp tok) ;; know (num N)
	   (setq *nums* (cons (second tok) *nums*))
	   (advance))
	  (t
	   (throw 'out nil)))))))


;; slurp up nums until meet OR 
(defun parse-or ()
  (loop while (not (null *tokens*)) do
    (let ((tok (car *tokens*)))
      (cond
	((true-listp tok)
	 (parse-num)
	 (if (and (null *tokens*) *nums*) ;;  edge case end tokens , but also nums
	     (setq *ors* (cons (reverse *nums*) *ors*))))
	((eq tok 'OR)
	 (advance)
	 (setq *ors* (cons (reverse *nums*) *ors*))
	 (setq *nums* nil))))))

(defun parse-head ()
  (let ((tok (car *tokens*)))
    (cond
      ((true-listp tok)
       (let ((val (second tok)))
	 (advance)
	 (advance)
	 (parse-or)
	 (setq *ors* `(,val ,(reverse *ors*)))))
      (t (error "expected number tok at start")))))


(defun parse-entry (toks)
  (setq *ors* nil)
  (setq *nums* nil)  
  (setq *tokens* toks)
  (parse-head)
  *ors*)


(defun parse (s)
  (let ((toks (tokeniser s)))
    (parse-entry toks)))


;; ;; define the operators , could left MUL as * , ADD as +
;; (defun mul (x y)
;;   (* x y))

;; (defun add (x y)
;;   (+ x y))


;; ;; execute code at runtime
;; (defun interpret (s)
;;   (eval (parse s)))

;; open file and read in lines
(defun get-lines (filename)
  (with-open-file (stream filename :direction :input)
    (let ((lines nil))
      (catch 'done
	(loop while t do
	  (let ((line (read-line stream nil 'eof)))
	    (cond
	      ((eq line 'eof) (throw 'done (reverse lines)))
	      (t (setq lines (cons line lines))))))))))


(let ((code-lines nil)
      (pattern-lines nil))
  (labels ((all-ab (str)
	     (let ((len (length str)))
	       (catch 'out
		 (loop for i from 0 to (- len 1) do
		   (cond
		     ((char= (char str i) #\a) t)
		     ((char= (char str i) #\b) t)
		     (t (throw 'out nil))))
		 t)))
	   (keep (x)
	     (cond
	       ((zerop (length x)) nil)
	       ((all-ab x)
		;;(format t "all-abs => ~a~%" x)
		(setq pattern-lines (cons x pattern-lines)))
	       (t
		;;(format t "code => ~a~%" x)
		(setq code-lines (cons x code-lines))))))
  (defun input-lines ()
    (cond
      ((or code-lines pattern-lines)
       (values code-lines pattern-lines))
      (t
       (let ((all (get-lines "../input.txt")))
	 ;;(format t "all => ~a ~%" all)
	 (mapcar #'keep all)
	 (setq code-lines (reverse code-lines))
	 (setq pattern-lines (reverse pattern-lines))
	 (values code-lines pattern-lines)))))))


(defparameter tarray nil)
(defparameter table nil)
(defparameter patterns nil)

(defun assign (x)
  (destructuring-bind (id . rest) x
    (setq rest (car rest))
    (format t "id = ~a : rest = ~a ~%" id rest)
    (setf (aref tarray id) rest)))

(multiple-value-bind (c p) (input-lines)
  (setq patterns p)
  (setq table (mapcar #'parse c))
  (setq table (sort table (lambda (x y) (< (first x)(first y)))))
  (setq tarray (make-array (length table) :initial-element nil))
  (mapcar #'assign table))

;; tarray is a lookup array
;; start at 0 -> list of ((8 11))
;;  means sequence has to be success on path 8 , followed by success on path 11
;; may require backtracking if more than one list is present in lookup
;; 1 -> ((86 9) (95 69))
;;      need 86 followed by 9 to succeed
;;   OR need 95 followed by 69 to succeed
;; how determine if it succeeds - success against a given string
;; on entry to trying out need position index into string
;; potentially stack heavy recursively
;;





;;(defparameter input-lines (get-lines "../input.txt"))

;; mini test suite
(test test-demo
  "This demonstrates the basic use of test and check."
  (is (= 1 1))
  (is (= 2 3))
  )

(run! 'test-demo)
;;(run 'test-demo)

