

(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...
(use-modules (srfi srfi-2)) ;; first second third ...
(use-modules (rnrs)) ;; assert

#|
always check what current directory working in 
scheme@(guile-user) [1]> (getcwd)
$5 = "/home/terry/advent-of-code/2020/day1"
scheme@(guile-user) [1]> (chdir "../day1/")
|#



;; regular expression
(use-modules (ice-9 regex)) 

;; pattern matcher ?
(use-modules (ice-9 match))

;; binary io -- dont know if this helped any as read-u8 is for reading ints no??
(use-modules (ice-9 binary-ports))

;; r7rs 
(use-modules (scheme base))


;; --------------------- macros --------------------------
(define-macro (dolist varls . body)
  (let* ((fn (gensym "fn"))
	 (xs (gensym "xs"))
	 (var (car varls))
	 (ls  (car (cdr varls))))
    `(letrec ((,fn (lambda (,xs)
		     (cond
		      ((null? ,xs) #f)
		      (#t (let ((,var (car ,xs)))
			    ,@body
			    (,fn (cdr ,xs))))))))
       (,fn ,ls))))

;; --------------------- while ----------------------------

(defmacro while (condition . body)
  (let ((lup (gensym "loop")))
    `(letrec ((,lup (lambda ()
		      (when ,condition
			,@body
			(,lup)))))
       (,lup))))

;; (let ((i 0))
;;   (while (< i 10)
;;     (format #t " i = ~a ~%" i )
;;     (set!  i (+ i 1))))

;; --------------------- macros --------------------------

(define *debug* #f)

(define input #f)
(define input2 #f)
(define example #f)
(define example2 #f)

#|
(read-line ... doc says a procedure in (scheme base)
lookup documentation on module (scheme base)
just says same thing just a procedure
|#
(define (get-lines filename)
  (let ((lines '()))
    (call-with-port (open-input-file filename)
      (lambda (port)
	(letrec ((rec (lambda ()
			(let ((val (read-line port)))
			  (cond
			   ((eof-object? val)  (set! lines (reverse lines)))
			   (#t (set! lines (cons val lines))
			       (rec)))))))
	  (rec))))
    lines))


(define (get-input filename)  
  (call-with-port (open-input-file filename)
    (lambda (port)
      (read port))))

(set! input (get-lines "input"))

(define (string-split-by-empty-string lines)
  (let ((ys '()))
    (letrec ((rec (lambda (xs rs)
		    (cond
		     ((null? xs) (set! ys (cons (reverse rs) ys))
		      (set! ys (reverse ys)))		      
		     ((string= (car xs) "") (set! ys (cons (reverse rs) ys))
		      (rec (cdr xs) '()))
		     (#t (rec (cdr xs) (cons (car xs) rs)))))))
      (rec lines '())
      ys)))

(define (remove-dups xs)
  (cond
   ((null? xs) xs)
   ((null? (cdr xs)) xs)
   ((member (car xs) (cdr xs)) (remove-dups (cdr xs)))
   (#t (cons (car xs) (remove-dups (cdr xs))))))


(define lines (get-lines "input"))
(define groups (string-split-by-empty-string lines))
(define groups2 (map (lambda (x) (apply string-append x)) groups))
(define groups3 (map string->list groups2))
(define groups4 (map remove-dups groups3))
(define yes (map length groups4))

(define sol (apply + yes))

#|
(define groups2 (map (lambda (x)
		       (map (lambda (s) (string-split s #\:))
			    (apply append
				   (map (lambda (s) (string-split s #\space))
					x))))
		     groups))
(define groups3 (map (lambda (x)
		       ;;(format #t "x = ~a ~%~%" x)
		       (map (lambda (y)
			      (list (string->symbol (car y))
				    (cadr y)))
			    x))
		     groups2))
|#

(format #t "day 6 loaded~%")


;; ----------- puzzle -----------------------------------
#|
bit of a confused puzzle to be honest

part 2
groups is a list each element is a list-2 of strings
each entry in list-2 is one persons
|#
(define set2 (map (lambda (x) (map string->list x)) groups))
#|
set2 is a list containing a list of characters
everyone in set of characters has to have that character for a count

(assoc y counts)

|#
(define (foo xs)
  (let ((counts '()))
    (dolist (x xs)
	    (dolist (y x)
		    (cond
		     ((assoc y counts) #f) 
		     ((null? (filter not 
				     (map (lambda (p) (member y p)) xs)))
		      (set! counts (cons (list y 1) counts)))
		     (#t #f))))
    (length counts)))

(define set3 (map foo set2))
(define sol2 (apply + set3))

;; ------------ results ------------
#|
scheme@(guile-user) [20]> (pp sol)
6565

scheme@(guile-user) [20]> sol2
$128 = 3137

|#





