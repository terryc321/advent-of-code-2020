;; guile 

(import (ice-9 pretty-print))
(define pp pretty-print) ;; pretty-print alias pp
(import (ice-9 format)) ;; format
(import (srfi srfi-1)) ;; take , drop 

;; company-mode
;; company-box-mode
;; electric-indent-mode
;; paredit mode

;; read input into 2d grid
(define in
  (lambda (filename)
    (with-input-from-file filename
      (lambda () 
        (let ((seq '()))
          (letrec ((foo (lambda ()
                          (let ((ch (read-char)))
                            (cond
                             ((eof-object? ch) #f)
                             ((not (or (char=? ch #\L)
                                       (char=? ch #\.)))
                              (foo))
                             (#t (set! seq (cons ch seq))
                                 (foo)))))))
            (foo))
          (reverse seq))))))

;; split into blocks of ? 
(define blocks
  (lambda (n xs)
    (define blocks-helper 
      (lambda (n xs ys)
        (cond
         ((null? xs) (reverse ys))
         (#t (blocks-helper n (drop xs n) (cons (take xs n) ys))))))
    (blocks-helper n xs '())))

(define input 
  (lambda ()
    (let ((lists (blocks 92 (in "../input"))))
      (list->vector (map list->vector lists)))))

(define input2
  (lambda ()
    (let ((lists (blocks 10 (in "../input2"))))
      (list->vector (map list->vector lists)))))


;; 0 based array indexing
(define unsafe-at
  (lambda (grid x y)
    (let* ((row (vector-ref grid y))
           (col (vector-ref row x)))
      col)))

;; recreate input - if it matches file - 
;;  x ...... 0 to 91 inclusive   : 92 column width
;;  y ...... 0 to 90 inclusive   : 91 rows
(define show-grid
  (lambda (grid)
    (let ((width (vector-length (vector-ref grid 0)))
          (height (vector-length grid)))
      (let lp ((x 0)(y 0))
        (cond
         ((>= x width) (lp 0 (+ y 1)))
         ((>= y height) #f)
         (#t 
          (format #t "~a" (vector-ref (vector-ref grid y) x))
          (when (= x (- width 1)) (format #t "~%"))
          (lp (+ x 1) y)))))))

(define copy-vector
  (lambda (v)
    (let* ((len (vector-length v))
           (vec2 (make-vector len 0)))
      (let lp ((x 0))
        (cond
         ((= x len) vec2)
         (#t 
          (vector-set! vec2 x (vector-ref v x))          
          (lp (+ x 1))))))))

;; test cases
(define test-copy-vector
  (lambda ()
    (let* ((tmp (list->vector '(1 2 3 4 5 )))
           (tmp2 (copy-vector tmp)))
      (vector-set! tmp 3 33)
      (format #t "tmp = ~a ~%" tmp)
      (format #t "tmp2 = ~a ~%" tmp2))))
       

(define copy-grid
  (lambda (grid)
    (let* ((width (vector-length (vector-ref grid 0)))
           (height (vector-length grid))
           (grid2 (make-vector height)))          
      (let lp ((y 0))
        (cond
         ((>= y height) grid2)
         (#t 
          (vector-set! grid2 y (copy-vector (vector-ref grid y)))
          (lp (+ y 1))))))))


;; test cases
(define test-copy-grid
  (lambda ()
    (let* ((grid (input))
           (grid2 (copy-grid grid)))
      (vector-set! (vector-ref grid 0) 0 #\W)
      (with-output-to-file "output3" 
        (lambda ()
          (show-grid grid)))
      (with-output-to-file "output4"
        (lambda ()
          (show-grid grid2)))
      )))

(define check-output
  (lambda ()
    (with-output-to-file "output"
      (lambda ()
        (show-grid (input))))))

(define check-output2
  (lambda ()
    (with-output-to-file "output2"
      (lambda ()
        (show-grid (input2))))))

#|
If a seat is empty (L) and there are no occupied seats adjacent to it, 
the seat becomes occupied.

If a seat is occupied (#) and four or more seats adjacent to it are also occupied, 
the seat becomes empty.

Otherwise, the seat's state does not change.

Floor (.) never changes; 
seats don't move, and 
nobody sits on the floor.
|# 

;; if on border ... assumed other seats around are empty seats ? or floor ?

;; #\L is empty seat
(define empty-seat? 
  (lambda (grid x y)
    (let* ((width (vector-length (vector-ref grid 0)))
           (height (vector-length grid)))
      (cond
       ((< x 0) #f)
       ((< y 0) #f)
       ((>= x width) #f)
       ((>= y height) #f)
       (#t (let ((ch (vector-ref (vector-ref grid y) x)))
             (char=? ch #\L)))))))

;; #\. is floor
(define floor?
  (lambda (grid x y)
    (let* ((width (vector-length (vector-ref grid 0)))
           (height (vector-length grid)))
      (cond
       ((< x 0) #f)
       ((< y 0) #f)
       ((>= x width) #f)
       ((>= y height) #f)
       (#t (let ((ch (vector-ref (vector-ref grid y) x)))
             (char=? ch #\.)))))))

;; #\# is occupied
(define recur-occupied?
  (lambda (grid x y dx dy)
    (let* ((width (vector-length (vector-ref grid 0)))
           (height (vector-length grid)))
      (cond
       ((< x 0) #f)
       ((< y 0) #f)
       ((>= x width) #f)
       ((>= y height) #f)
       (#t (let ((ch (vector-ref (vector-ref grid y) x)))
             (cond
              ((char=? ch #\#) #t)              
              ((char=? ch #\.) (recur-occupied? grid (+ x dx) (+ y dy) dx dy))
              ((char=? ch #\L) #f))))))))

(define occupied?
  (lambda (grid x y)
    (let* ((width (vector-length (vector-ref grid 0)))
           (height (vector-length grid)))
      (cond
       ((< x 0) #f)
       ((< y 0) #f)
       ((>= x width) #f)
       ((>= y height) #f)
       (#t (let ((ch (vector-ref (vector-ref grid y) x)))
             (char=? ch #\#)))))))
       
(define adjacent-count    
  (lambda (grid x y)
    (+ (if (recur-occupied? grid (- x 1) (- y 1) -1 -1) 1 0)
       (if (recur-occupied? grid (- x 1) y -1 0) 1 0)
       (if (recur-occupied? grid (- x 1) (+ y 1) -1 1) 1 0)
       (if (recur-occupied? grid x (- y 1) 0 -1) 1 0)
       ;;(if (recur-occupied? grid x y) 1 0)
       (if (recur-occupied? grid x (+ y 1) 0 1) 1 0)
       (if (recur-occupied? grid (+ x 1) (- y 1) 1 -1) 1 0)
       (if (recur-occupied? grid (+ x 1) y 1 0) 1 0)
       (if (recur-occupied? grid (+ x 1) (+ y 1) 1 1) 1 0))))


(define grid-get
  (lambda (grid x y)
    (let* ((width (vector-length (vector-ref grid 0)))
           (height (vector-length grid)))
      (cond
       ((or (< x 0) 
            (< y 0)
            (>= x width)
            (>= y height))
        (error "grid-get out bounds"))
       (#t
        (vector-ref (vector-ref grid y) x))))))

(define grid-width
  (lambda (grid)
    (let* ((width (vector-length (vector-ref grid 0))))
      width)))

(define grid-height
  (lambda (grid)
    (let* ((width (vector-length (vector-ref grid 0)))
           (height (vector-length grid)))
      height)))


(define grid-set!
  (lambda (grid x y v)
    (let* ((width (vector-length (vector-ref grid 0)))
           (height (vector-length grid)))
      (cond
       ((or (< x 0) 
            (< y 0)
            (>= x width)
            (>= y height))
        (error "grid-set! out bounds"))
       (#t
        (vector-set! (vector-ref grid y) x v))))))



(define process
  (lambda (grid)
    (let* ((width (vector-length (vector-ref grid 0)))
           (height (vector-length grid))
           (grid2 (copy-grid grid)))      
      (let lp ((x 0)(y 0))
        (cond
         ((>= x width) (lp 0 (+ y 1)))
         ((>= y height) #f)
         (#t 
          (cond
           ;; seat becomes occupied
           ((and (empty-seat? grid x y)
                 (zero? (adjacent-count grid x y)))
            (grid-set! grid2 x y #\#))
           ;; seat occupied and 5 or more seats occupied .. seat becomes empty
           ((and (occupied? grid x y)
                 (>= (adjacent-count grid x y) 5))
            (grid-set! grid2 x y #\L)))
          (lp (+ x 1) y))))
      grid2)))


(define occupied-seat-count
  (lambda (grid)
    (let* ((width (vector-length (vector-ref grid 0)))
           (height (vector-length grid))
           (tot 0))
      (let lp ((x 0)(y 0))
        (cond
         ((>= x width) (lp 0 (+ y 1)))
         ((>= y height) #f)
         (#t 
          (cond    
           ((occupied? grid x y)
            (set! tot (+ tot 1))))
          (lp (+ x 1) y))))
      tot)))


(define hack
  (lambda (grid)
    (letrec ((foo (lambda (g)
                    (let ((g1 (copy-grid g))
                          (g2 #f))
                      (set! g2 (process g1))
                      (cond
                       ((equal? g1 g2) 
                        (show-grid g2)
                        (let ((os (occupied-seat-count g2)))
                          (format #t "occupied seat count ~a ~%" os)
                          os))
                       (#t (foo g2)))))))
      (foo grid))))


(define solution
  (lambda ()
    (hack (input))))

#|
> (solution)
...
occupied seat count 1974  ..... ACCEPTED
|#











    
    
    
    
    



