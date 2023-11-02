

(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...
(use-modules (srfi srfi-2)) ;; first second third ...
(use-modules (rnrs)) ;; assert

;; (getcwd)
;; (chdir "..somewhere..")

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


(format #t "day one loaded~%")


#| ----------- puzzle -----------------------------------
find two numbers that sum to 2020
if you multiply them get the answer
|#

(define (/= x y)
  (not (= x y)))


(define (test-1)
  (call/cc (lambda (escape)
	     (dolist (x input)
		     (dolist (y input)
			     (cond
			      ((and (/= x y) (= 2020 (+ x y)))
			       (format #t "found ~a and ~a ~%" x y)
			       (escape (* x y)))
			      (#t #f)))))))

(define (test-2)
  (call/cc (lambda (escape)
	     (dolist (x input)
		     (dolist (y input)
			     (dolist (z input)
				     (cond
				      ((and (/= x y) (/= y z) (/= x z) (= 2020 (+ x y z)))
				       (format #t "found ~a ,  ~a and ~a  ~%" x y z)
				       (escape (* x y z)))
				      (#t #f))))))))




#|
------------ results ------------
found 1704 and 316 
$8 = 538464

found 615 ,  903 and 502  
$9 = 278783190

|#










