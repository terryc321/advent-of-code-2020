

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

(set! input (lambda () (get-lines "input")))



#|
input either 
acc +/-XXX
nop +/-XXX
jmp +/-XXX
|#

(define (convert s)
  (let ((m (string-match "(acc|nop|jmp) ([+-][0-9]+)" s)))
    (cond
     (m (let ((v (string->number (match:substring m 2)))
	      (op (string->symbol (match:substring m 1))))
	  `(,op ,v)))
     (#t (error (list "convert"))))))

(define (code) (list->vector (map convert (input))))


(define (test-1-code) (list->vector '((nop +0)
				      (acc +1)
				      (jmp +4)
				      (acc +3)
				      (jmp -3)
				      (acc -99)
				      (acc +1)
				      (jmp -4)
				      (acc +6))))


(format #t "day 8 loaded~%")


;; ----------- puzzle -----------------------------------
(define (exec c)
  (call/cc (lambda (escape)
	     (let ((acc 0)
		   (lup (make-vector (vector-length c) 0)))
	       (letrec ((foo (lambda (i)
			       (when (= i (vector-length c))
				 ;;(format #t "terminated correctly by attempting execute instruction after end code~%")
				 (escape (values #t acc)))
			       (let ((ins (vector-ref c i)))
				 ;;(format #t "ip = ~a : ins ~a : acc ~a ~%" i ins acc)
				 (vector-set! lup i (1+ (vector-ref lup i)))
				 ;; escape with accumulator value 
				 ;; if instruction has run more than once before
				 (when (> (vector-ref lup i) 1)
				   ;;(format #t "hit infinite loop ~%")
				   (escape (values #f acc)))
				 ;; match instruction based on 
				  (match ins
				    (('acc n)  (set! acc (+ acc n)) (foo (+ i 1)))
				    (('jmp n)  (foo (+ i n)))
				    (('nop n)  (foo (+ i 1)))
				    (#t (error (list "exec.match"))))))))
			(foo 0))))))

;;; REDEFINE over-write previous version above
(define (exec c)
  (let ((acc 0)
	(lup (make-vector (vector-length c) 0)))
    (letrec ((foo (lambda (i)
		    (cond
		     ((= i (vector-length c))
		      ;;(format #t "terminated correctly by attempting execute instruction after end code~%")
		      (values #t acc))
		     (#t
		      (let ((ins (vector-ref c i)))
			;;(format #t "ip = ~a : ins ~a : acc ~a ~%" i ins acc)
			(vector-set! lup i (1+ (vector-ref lup i)))
			;; escape with accumulator value 
			;; if instruction has run more than once before
			(cond
			 ((> (vector-ref lup i) 1)
			  ;;(format #t "hit infinite loop ~%")
			  (values #f acc))
			 (#t
			  ;; match instruction based on 
			  (match ins
			    (('acc n)  (set! acc (+ acc n)) (foo (+ i 1)))
			    (('jmp n)  (foo (+ i n)))
			    (('nop n)  (foo (+ i 1)))
			    (#t (error (list "exec.match"))))))))))))
      (foo 0))))




;; iterate through vector until changing instruction
;; nop to jmp
;; jmp to nop
;; seeing if find termination correct 
(define (seek c)
  (letrec ((foo (lambda (i)
		  (format #t "seek i = ~a ~%" i)
		  (cond
		   ((>= i (vector-length c)) #f)
		   (#t
		    (let ((ins (vector-ref c i)))
		      (match ins
			(('acc n)  #f)
			(('jmp n)  (vector-set! c i `(nop ,n))
			           (call-with-values (lambda () (exec c))
				     (lambda (worked acc)
				       (cond
					(worked (format #t "solution : change jmp to nop at IP ~a : acc ~a~%" i acc)))))
				   (vector-set! c i `(jmp ,n)))
			(('nop n)  (vector-set! c i `(jmp ,n))
			           (call-with-values (lambda () (exec c))
				     (lambda (worked acc)
				       (cond
					(worked (format #t "solution : change nop to jmp at IP ~a : acc ~a~%" i acc)))))
				   (vector-set! c i `(nop ,n)))
			(#t (error (list "seek.match")))))
		    (foo (+ i 1)))))))
    (foo 0)))





;; ---------- tests ------------------
;; can pass multiple values back through call/cc
(define (test-1)
  (define (my-call/cc)
    (call/cc (lambda (esc)
	       (esc (values 1 #t "three")))))
  (call-with-values (lambda () (my-call/cc))
    (lambda (a b c)
      (format #t "values returned ~a ~a ~a ~%" a b c))))

(define (test-2)
  (define (my-call/cc)
    (call/cc (lambda (esc)
	       (esc (values 1 #t "three")))))
  (call-with-values (lambda () (values 2 #f "four"))
    (lambda (a b c)
      (format #t "values returned ~a ~a ~a ~%" a b c))))




;; ------------ results ------------
#|
part-1
(exec (code))
1744

accepted

part - 2 
change a nop to a jmp , or a jmp to a nop for what reason ?
EDIT ok so changing OPCODE only not the number after opcode

solution : change jmp to nop at IP 441 : acc 1174

|#
