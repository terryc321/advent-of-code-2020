

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

(define (get-input filename)  
  (call-with-port (open-input-file filename)
    (lambda (port)
      (read port))))


(set! input (get-input "input"))

#|
(set! example (convert-to-2d (get-input "day24/example")))
(set! example2 (convert-to-2d (get-input "day24/example2")))

(define (reset)
   (set! input (get-input "day17/input")))
 (reset)
|#

(format #t "day 2 loaded~%")


;; ----------- puzzle -----------------------------------
#|
after adjusted input to scheme readable form
|#
(define (foo)
  (let ((good 0))
    (dolist (p input)
	    (match p
	      ((from to ch str)
	       (format #t "prob ~a - ~a  ~a : ~a ~%" from to ch str)
	       (let ((occ (occur ch str)))
		 (cond
		  ((and (>= occ from) (<= occ to))
		   (set! good (1+ good)))
		  (#t #f))))
	      ( _ (error (list "foo.match")))))
    (format #t "there are ~a valid passwords ~%" good)
    good))


(define (occur ch s)
  (let ((tot 0))
    (dolist (n (iota (string-length s)))
	    (cond
	     ((char=? ch (string-ref s n))
	      (set! tot (1+ tot)))
	     (#t #f)))
    tot))

#|
test driven development ? meh.
|#
(define (test-1)
  (assert (= 3 (occur #\a "abcabcabc"))))


;; uses a different policy to verify passwords
(define (bar)
  (let ((good 0))
    (dolist (p input)
	    (match p
	      ((from to ch str)
	       (format #t "prob ~a - ~a  ~a : ~a  --> " from to ch str)
	       (let ((occ (occur2 ch str from to)))
		 (cond
		  (occ
		   (format #t " good ~%")
		   (set! good (1+ good)))
		  (#t
		   (format #t " bad ~%")
		   #f))))
	      ( _ (error (list "foo.match")))))
    (format #t "there are ~a valid passwords ~%" good)
    good))


#|
indexing starts at 1 not zero
(string-ref 0 s) means 1st character

exclusive or , only one or other but not both -- bug 001
|#
(define (occur2 ch s a b)
  (let ((p (and (>= a 1) (<= a (string-length s)) (char=? ch (string-ref s (- a 1)))))
	(q (and (>= b 1) (<= b (string-length s)) (char=? ch (string-ref s (- b 1))))))
    (cond
     ((and p (not q)) #t)
     ((and q (not p)) #t)
     (#t #f))))



(define (test-2)
  (assert (occur2 #\a "abcabcabc" 1 1))
  (assert (occur2 #\a "abcabcabc" 4 4))
  (assert (occur2 #\a "abcabcabc" 7 7)))



#|
------------ results ------------
(foo)
there are 542 valid passwords 
$10 = 542

(bar)
exclusive one or other match char at a position - but not both
there are 360 valid passwords 
$15 = 360

|#










