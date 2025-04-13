
#|
;; chicken
;; aoc2020/day13

notes2 :: (Int , [Int])
notes2 = (1000391 , filter (\x -> x /= 0) (let x = 0 in [19,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,383,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29,x,457,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17]))

notes :: (Int , [Int])
notes = (939,filter (\x -> x /= 0) (let x = 0 in [7,13,x,x,59,x,31,19]))


The earliest timestamp that matches the list 17,x,13,19 is 3417.
67,7,59,61 first occurs at timestamp 754018.
67,x,7,59,61 first occurs at timestamp 779210.
67,7,x,59,61 first occurs at timestamp 1261476.
1789,37,47,1889 first occurs at timestamp 1202161486.

|#
(import (srfi-1))

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


;; buses 7 x 13 19
;; times 0 1  2  3 
(example 939 '((7 0) (13 2) (19 3)))








(modulo 3417 17)
(modulo 3417 13)
(modulo 3417 19)
