

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

(set! input (get-input "input"))


(format #t "day 5 loaded~%")


;; ----------- puzzle -----------------------------------
#|
binary partition
first 7 characters F B 0 - 127 : 128
0-63 64-127

binary partition

128
64
32
16
8
4
2

indexes zero based ??

|#
(define (bpart a b)
  (let* ((n (- b a))
	 (half (/ n 2)))
    (list (list a (+ a half))
	  (list (+ a half) (+ a (+ half half))))))

(define seats (iota 128))

(define (row xs n s i)
  (cond
   ((> i 6) (car xs))
   ((char=? (string-ref s i) #\F)
    (let ((val (take xs (/ n 2))))
      ;;(format #t "F : taking front ~a values ~%" (/ n 2))
      (row val (/ n 2) s (+ i 1))))
   ((char=? (string-ref s i) #\B)
    (let ((val (take (drop xs (/ n 2)) (/ n 2))))
      ;;(format #t "B : taking back ~a values ~%" (/ n 2))
      (row val (/ n 2) s (+ i 1))))
   (#t
    (format #t "error ??")
    (format #t "xs = ~a ~%" xs)
    (format #t "n = ~a ~%" n)
    (format #t "s = ~a ~%" s)
    (format #t "i = ~a ~%" i)    
    (error (list "partition.row" 'unknown)))))

(define (col xs n s i)
  (cond
   ((> i 9) (car xs))
   ((char=? (string-ref s i) #\L)
    (let ((val (take xs (/ n 2))))
      ;;(format #t "F : taking left ~a values ~%" (/ n 2))
      (col val (/ n 2) s (+ i 1))))
   ((char=? (string-ref s i) #\R)
    (let ((val (take (drop xs (/ n 2)) (/ n 2))))
      ;;(format #t "B : taking right ~a values ~%" (/ n 2))
      (col val (/ n 2) s (+ i 1))))
   (#t
    (format #t "error ??")
    (format #t "xs = ~a ~%" xs)
    (format #t "n = ~a ~%" n)
    (format #t "s = ~a ~%" s)
    (format #t "i = ~a ~%" i)    
    (error (list "partition.col" 'unknown)))))


(define (partition-row s)
  (row (iota 128) 128 s 0))

(define (partition-col s)
  (col (iota 8) 8 s 7))

 
(define (test-1)
  (partition-row "FBFBBFFRLR"))

(define (test-2)
  (partition-col "FBFBBFFRLR"))

(define (seat-id s)
  (+ (partition-col s) (* 8 (partition-row s))))

(define (test-3)
  (seat-id "FBFBBFFRLR"))

(define (highest-seat-id)
  (apply max (map seat-id input)))

(define (foo)
  (let ((ids (sort (map seat-id input) <)))
    (letrec ((rec (lambda (xs)
		    (rec2 (cdr xs) (car xs))))
	     (rec2 (lambda (xs prev)
		     (cond
		      ((null? xs) "FALSE no empty seat found in list")
		      ((> (- (car xs) prev) 1)
		       (format #t "empty seat at ~a ~%" (+ prev 1))
		       (rec xs))
		      (#t (rec2 (cdr xs) (car xs)))))))
      (rec ids))))





;; ------------ results ------------
#|
scheme@(guile-user) [19]> (highest-seat-id)
$115 = 913

913 is highest seat id


$121 = (32 33 34  .......  713 714 715 ***716 718*** 719 720 ..... 913)
scheme@(guile-user) [19]> (foo)
empty seat at 717 

|#







