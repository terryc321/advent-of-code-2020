

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

#|
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

(format #t "day 7 loaded~%")

;; ----------- puzzle -----------------------------------
#|
hand bags ... containing other hand bags ...
regex type puzzle

a contains b c d
b contains e f g
c contains j k l
d contains e f g

told start with f , what things could hold f eventually bags inside bags ...
task 1 - parse input into alist
task 2 - use alist to breadth first search of possible cases

task 1 
"mirrored brown bags contain 1 pale teal bag, 3 muted gray bags, 3 dark bronze bags."

|#

(define (name-bag s)
  (let ((m (string-match "^([a-z]+) ([a-z]+) bag. contain" s)))
    (cond
     (m (let ((style (string->symbol (match:substring m 1)))
	      (color (string->symbol (match:substring m 2))))
	  (string->symbol (format #f "~a-~a" style color))))
     (#t #f))))

#|

|#
(define (name-bag2 s)
  (let ((m (string-match "[0-9]+ ([a-z]+) ([a-z]+) bag" s)))
    (cond
     (m (let ((style (string->symbol (match:substring m 1)))
              (color (string->symbol (match:substring m 2))))
          (string->symbol (format #f "~a-~a" style color))))
     (#t #f))))

#|
(let ((m (map match:substring (list-matches "[0-9]+ ([a-z]+ [a-z]+) bag" s))))
      (format #t "contains : ~a ~%" m)))

(cond
     (m (let ((style (string->symbol (match:substring m 1)))
	      (color (string->symbol (match:substring m 2))))
	  (string->symbol (format #f "~a-~a" style color))))
     (#t #f))))

|#
(define (contains-bag s)
  (let ((m (list-matches "([0-9]+) ([a-z]+) ([a-z]+) bag" s)))
    ;;(format #t "contains : ~a ~%" m)
    (let ((rs '()))
      (dolist (p m)
	      (let ((sym (string->symbol
			  (format #f "~a-~a"
				  (match:substring p 2)
				  (match:substring p 3))))
		    (ct (string->number
			 (match:substring p 1))))
		(set! rs (cons (list sym ct) rs))))
      (reverse rs))))


(define (name-bag3 s)
  (let ((m (string-match "^.*contain(.*$)" s)))
    (cond
     (m (let ((back (match:substring m 1)))
	  ;;(format #t "back : ~a ~%" back)
	  (contains-bag back)))
     (#t #f))))

;; combination bag at start followed by a list of bags it contains
(define (name-bag4 s)
  (cons (name-bag s) (name-bag3 s)))



#|  
|#


(define (sample) (map name-bag input))
(define (sample2) (map name-bag2 input))
(define (sample3) (map name-bag3 input))
(define (sample4) (map name-bag4 input))


;; is shiny-gold in the list of symbols
(define (check-1)
  (if (member 'shiny-gold (sample)) #t #f))

(define (dataset) (sample4))

#|
breadth first search
find bags that contain shiny-gold
for each of those bags - find bags that can contain those bags
for each set of new bags not already in known list of bags - investigate them


foo look for all bags that contain target , the target bag
initially this is 'shiny-gold

part - 2

search forwards from shiny - bag to find other bags , keeping track of how many bags they contain

give foo a list of bags where want to jump off and look for other bags  , rather than just one bag

shiny-bag ->  
shiny gold bags contain 3 striped gold bags, 2 faded violet bags, 3 shiny tan bags, 3 dark turquoise bags.

|#


;; foo association list
(define (shiny-bag bag dataset)
  (let ((all '()))
    (letrec ((foo (lambda (as)
		    ;;(format #t "as = ~A ~%" as)
		    (cond
		     ((null? as) 0)
		     (#t  (apply +				
				 (map (lambda (x)
					;;(format #t "x = ~a ~%" x)
					(let ((n (cadr x))
					      (sym (car x)))
					  ;;(format #t " n = ~a : sym = ~a ~%" n sym)
					  (+ n (* n (foo (cdr (assoc sym dataset)))))))
				     as)))))))
      (foo (cdr (assoc bag dataset)))
      )))




;; ---------- tests ------------------
(define (data-1) '((faded-blue)
		   (dotted-black)
  		   (vibrant-plum (faded-blue 5) (dotted-black 6))
		   (dark-olive (faded-blue 3) (dotted-black 4))
		   (shiny-gold (dark-olive 1) (vibrant-plum 2))))

(define (test-1)  ;; should be 32
  (shiny-bag 'shiny-gold (data-1)))

#|
           sg : do + 2 vp
           do : 3 fb  + 4 db = 7 
           vp : 5 fb  + 6 db = 11

           sg : 1 * do + ( 1 * 7 do bags ) + 2 vp ( + 2 * 11 vp bags )
                 1 + 7 + 2 + 22
                 1 + 7 + 24
                 8 + 24
                 32 altogether
|#

#|
shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.

|#
(define (data-2) '((shiny-gold (dark-red 2))
		   (dark-red (dark-orange 2))
		   (dark-orange (dark-yellow 2))
		   (dark-yellow (dark-green 2))
		   (dark-green (dark-blue 2))
		   (dark-blue (dark-violet 2))
		   (dark-violet)))

(define (test-2)  ;; should be 126
  (shiny-bag 'shiny-gold (data-2)))

(define (part-2)
  (shiny-bag 'shiny-gold (dataset)))





;; ------------ results ------------
#|
scheme@(guile-user) [5]> (length (find-shiny-bag))
$25 = 337

337 bags can contain a shiny-gold bag 

part - 2

find how many bags a bag has to contain
scheme@(guile-user) [5]> (shiny-bag)
$50 = 44172

44,172 bags in bags ? WRONG ! , lets see how tests 

(part-2)
scheme@(guile-user) [16]> (part-2)
$83 = 50100

attempt 2 : 50100  




|#


