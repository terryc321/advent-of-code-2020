
#|
;; chicken
;; aoc2020/day13

notes2 :: (Int , [Int])
notes2 = (1000391 , filter (\x -> x /= 0) (let x = 0 in [19,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,383,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29,x,457,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17]))

notes :: (Int , [Int])
notes = (939,filter (\x -> x /= 0) (let x = 0 in [7,13,x,x,59,x,31,19]))


The earliest timestamp that matches the list
17,x,13,19 is 3417.
67,7,59,61 first occurs at timestamp 754018.
67,x,7,59,61 first occurs at timestamp 779210.
67,7,x,59,61 first occurs at timestamp 1261476.
1789,37,47,1889 first occurs at timestamp 1202161486.

|#


(import (srfi-1))
(import (chicken pretty-print))
(import (chicken format))


(define (next t n)
  (let ((before (* n (floor (/ t n)))))
    (cond
     ((< before t) (+ before n))
     (#t before))))

(define (example t xs)
  (filter
   (lambda (o) (not (eq? o #f)))
   (map (lambda (q) ;; q pair (n,r)
	  (let ((n (car q))
		(r (car (cdr q))))
	    (cond
	     ((< n 0) #f)
	     (#t 
	      (list n (next t n))))))
	xs)))


;; buses 17 x 13 19
;; times 0 1  2  3 
(example 3417 '((17 0) (13 2) (19 3)))

;; 3417 = 17 * ?
;; 3418
;; 3419 = 13 * ?
;; 3420 = 19 * ? 
(modulo 3417 17)
(modulo 3419 13)
(modulo 3420 19)


;; 67,7,59,61
(define (qfind n exit)
  (qfind-2 n exit)
  (qfind (+ n 67) exit))

(define (qfind-2 n exit)
  (when (and (zero? (modulo (+ n 1) 7))
	     (zero? (modulo (+ n 2) 59))
	     (zero? (modulo (+ n 3) 61)))
    (exit n)))

(define (run)
  (call/cc (lambda (k)
	     (qfind 67 k))))


;; more dynamic solution such that
;; 17,x,13,19 is 3417.
;; 67,7,59,61 first occurs at timestamp 754018.
;; 67,x,7,59,61 first occurs at timestamp 779210.
;; 67,7,x,59,61 first occurs at timestamp 1261476.
;; 1789,37,47,1889 first occurs at timestamp 1202161486.
;; generated dynamically

(define (make-expr2 xs step)
  (cond
   ((null? xs) '())
   (#t (let ((value (car xs)))
	 (cond
	  ((integer? value) (append
			     `((zero? (modulo (+ n ,step) ,value)))
			     (make-expr2 (cdr xs) (+ step 1))))
	  (#t (make-expr2 (cdr xs) (+ step 1))))))))


(define (make-expr xs)
  `(letrec ((qfind (lambda (n exit)
		     (qfind-2 n exit)
		     (qfind (+ n ,(car xs)) exit)))
	    (qfind-2 (lambda (n exit)
		       (when (and ,@(make-expr2 (cdr xs) 1))
			 (exit n))))
	    (run (lambda ()
		   (call/cc (lambda (k)
			      (qfind ,(car xs) k))))))
     (run)))

(define (run-expr xs)
  (eval (make-expr xs)))


(pp (make-expr '(17 x 13 19)))
(run-expr '(17 x 13 19))
			     
(letrec ((qfind (lambda (n exit) (qfind-2 n exit) (qfind (+ n 17) exit)))
         (qfind-2
           (lambda (n exit)
             (when (and (zero? (modulo (+ n 2) 13))
                        (zero? (modulo (+ n 3) 19)))
                   (exit n))))
         (run (lambda () (call/cc (lambda (k) (qfind 17 k))))))
  (run))
				     
(run-expr '(67 7 59 61))
(run-expr '(67 x 7 59 61))
(run-expr '(67 7 x 59 61))
(run-expr '(1789 37 47 1889))
(run-expr '(7 13 x x 59 x 31 19))

;; brute force didnt work 
;; (time
;;  (format #t "~%run-expr ( ~a )~%~%"
;; 	 (run-expr '(19 x x x x x x x x x x x x 37 x x x x x 383 x x x x x x x 23 x x x x 13 x x x x x x x x x x x x x x x 29 x 457 x x x x x x x x x 41 x x x x x x 17))))

;; 209
;; (- 912 209)   703    (+ 209 (* 703 1))  (+ 209 (* 703 2))   (+ 209 (* 703 3))

(define (make-expr-repeat xs)
  `(letrec ((qfind (lambda (n exit)
		     (qfind-2 n exit)
		     (qfind (+ n ,(car xs)) exit)))
	    (qfind-2 (lambda (n exit)
		       (when (and ,@(make-expr2 (cdr xs) 1))
			 (exit n))))
	    (run (lambda ()
		   (qfind ,(car xs) (lambda (k) (format #t "k = ~a ~%" k))))))
     (run)))


(pp (make-expr-repeat '(19 x x x x x x x x x x x x 37)))

(define (f2)
  (letrec ((qfind (lambda (n exit) (qfind-2 n exit) (qfind (+ n 19) exit)))
           (qfind-2
            (lambda (n exit)
              (when (and (zero? (modulo (+ n 13) 37))) (exit n))))
           (run (lambda () (qfind 19 (lambda (k) (format #t "k = ~a ~%" k))))))
    (run)))

(define f3 (eval `(lambda () ,(make-expr-repeat '(19 x x x x x x x x x x x x 37 x x x x x 383)))))

(define f4 (eval `(lambda () ,(make-expr-repeat '(19 x x x x x x x x x x x x 37 x x x x x 383 x x x x x x x 23)))))

(define f5 (eval `(lambda () ,(make-expr-repeat '(19 x x x x x x x x x x x x 37 x x x x x 383 x x x x x x x 23 x x x x 13)))))

;; f6 fails to make answer as steps too large
(define (make-expr-repeat-big k1 k2 xs)
  `(letrec ((qfind (lambda (k1 k2 exit)
		     (qfind-2 k1 exit)
		     (qfind (+ k1 k2) k2 exit)))
	    (qfind-2 (lambda (n exit)
		       (when (and ,@(make-expr2 (cdr xs) 1))
			 (exit n))))
	    (run (lambda ()
		   (qfind ,k1 ,(- k2 k1) (lambda (k) (format #t "k = ~a ~%" k))))))
     (run)))




(define f6 (eval `(lambda () ,(make-expr-repeat-big 23555630 104061081 '(19 x x x x x x x x x x x x 37 x x x x x 383 x x x x x x x 23 x x x x 13 x x x x x x x x x x x x x x x 29)))))

(define f7 (eval `(lambda () ,(make-expr-repeat-big 587093787 2921751866  '(19 x x x x x x x x x x x x 37 x x x x x 383 x x x x x x x 23 x x x x 13 x x x x x x x x x x x x x x x 29 x 457)))))

(define f8 (eval `(lambda () ,(make-expr-repeat-big 946123615782 2013062357885 
						    '(19 x x x x x x x x x x x x 37 x x x x x 383 x x x x x x x 23 x x x x 13 x x x x x x x x x x x x x x x 29 x 457 x x x x x x x x x 41)))))

(define f9 (eval `(lambda () ,(make-expr-repeat-big 31887347136769 75631835562992 
						    '(19 x x x x x x x x x x x x 37 x x x x x 383 x x x x x x x 23 x x x x 13 x x x x x x x x x x x x x x x 29 x 457 x x x x x x x x x 41 x x x x x x 17)))))

#|

#;531> (f9)
k = 294354277694107  <<< ACCEPTED ANSWER 
k = 1038010580939898 
k = 1781666884185689 
k = 2525323187431480 
k = 3268979490677271 
k = 4012635793923062 
k = 4756292097168853 
k = 5499948400414644 
k = 6243604703660435 

using results of running f5 to make f6 constants were first two values output on k= lines
f6 makes f7
f7 makes f8
f8 makes f9 possible

(f9) runs and generates solution
using eval


|#




















