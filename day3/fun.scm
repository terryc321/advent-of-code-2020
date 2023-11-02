

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

;; sanity check all strings same length
;; (map string-length input)


#|
(set! example (convert-to-2d (get-input "day24/example")))
(set! example2 (convert-to-2d (get-input "day24/example2")))

(define (reset)
   (set! input (get-input "day17/input")))
 (reset)
|#

(format #t "day 3 loaded~%")


;; ----------- puzzle -----------------------------------
#|
pattern repeats
A A A A
move right 3 and down 1
how many trees encounter?

|#
(define (get-xy x y)
  (string-ref (vector-ref input y) x))

(define (tree? ch)
  (char=? ch #\# ))


(define (foo)  
  (let ((height (vector-length input))
	(y 0)
	(x 0)
	(n-trees 0))
    (letrec ((rec (lambda ()
		    (set! y (+ y 1))
		    (set! x (mod (+ x 3) 31))
		    (cond
		     ((>= y height) n-trees)
		     ((tree? (get-xy x y))
		      (set! n-trees (1+ n-trees))
		      (rec))
		     (#t (rec))))))
      (rec))))

;; throw a macro in to see how the weather is 
(define-macro (slope dx dy)
  `(let ((height (vector-length input))
	 (y 0)
	 (x 0)
	 (n-trees 0))
     (letrec ((rec (lambda ()
		     (set! y (+ y ,dy))
		     (set! x (mod (+ x ,dx) 31))
		     (cond
		      ((>= y height) n-trees)
		      ((tree? (get-xy x y))
		       (set! n-trees (1+ n-trees))
		       (rec))
		      (#t (rec))))))
       (rec))))

(define (f1)  (slope 1 1))
(define (f2)  (slope 3 1))
(define (f3)  (slope 5 1))
(define (f4)  (slope 7 1))
(define (f5)  (slope 1 2))

(define (opt)
  (* (f1) (f2) (f3) (f4) (f5)))

#|
------------ results ------------

based on (mod x 31) idea , guile says should hit 151 trees

(opt) meaning optimistic guess that macro actually does its job
says there should be a lot of trees
7540141059
of them



|#










