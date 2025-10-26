
(use-modules (srfi srfi-1))
(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))
(define pp pretty-print)

;; load values from input file
(define (readlines filename)
  (call-with-input-file filename
    (lambda (p)
      (let loop ((line (read p))
                 (result '()))
        (if (eof-object? line)
            (reverse result)
            (loop (read p) (cons line result)))))))

;;(getcwd)
(define input (readlines "../input"))

;; (length input)
;; (pp input)

(define example '(35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576))

(define preamble 5)

;; (take example 5)
;; (drop example 5)

;; n preamble number
(define iter
  (lambda (xs n)
    (let ((set (take xs n))
	  (todo (drop xs n)))
      (iter2 set todo))))

(define iter2
  (lambda (xs ys)
    (cond
     ((null? ys) 'no-more)
     (#t (let ((y (car ys)))
	   ;; is y the sum of values in xs
	   (cond
	    ((y-sum-of-two-values-entry xs y)
	     (iter2 (append (cdr xs) (list y)) (cdr ys)))
	    (#t y)))))))

;; m4uhd i swear movie

(define y-sum-of-two-values-entry  
  (lambda (xs y)
    ;;(format #t "trying to make ~a from set (~a)~%" y xs)
    (y-sum-of-two-values xs y)))


(define y-sum-of-two-values
  (lambda (xs y)
    (cond
     ((null? xs) #f)
     (#t 
      (let ((x (car xs))
	    (xs2 (cdr xs)))
	(let ((maybe (y-sum-helper x xs2 y)))
	  (if maybe
	      maybe
	      (y-sum-of-two-values (cdr xs) y))))))))

(define y-sum-helper
  (lambda (x xs y)
    (cond
     ((null? xs) #f)
     (#t (let ((x2 (car xs)))
	   (cond
	    ((= (+ x x2) y) #t)
	    (#t
	     ;;(format #t "failed ~a + ~a is ~a NOT ~a ~%" x x2 (+ x x2) y)
	     (y-sum-helper x (cdr xs) y))))))))


(define result-1
  (lambda ()
    (iter example 5)))


(define part-1
  (lambda ()
    (iter input 25)))


(define target (part-1))
;; 14144619
;; answer accepted !!!

;; (y-sum-of-two-values-entry '(35 20 15 25 47) 40)
;;(take '(1 2 3 ) 5)
;; find contiguous sequences of values that add up to the target number
(define input-length (length input))


(define (sel-entry xs n)
  (let ((len (length xs)))
    (sel xs len n)))

(define (sel xs len n)
  (cond
   ((< len n) (sel-entry input (+ n 1)))
   (#t 
    (let ((seq (take xs n)))
      (cond
       ((= (apply + seq) target)
	(format #t "sequence found of length ~a : ~a " n seq)
	(let* ((smallest (apply min seq))
	       (largest (apply max seq))
	       (sum (+ smallest largest)))
	  (format #t "largest ~a : smallest ~a : sum ~a ~%" smallest largest sum)
	  sum))
       (#t (sel (cdr xs) (- len 1) n)))))))


(define (part-2)
  (sel-entry input 2))

(define solution-2 (part-2))
;; 1766397
;; answer accepted !!!
