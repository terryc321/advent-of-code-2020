

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

#|
input needs more fudgin to get into correct format
it be nice if we could write a scheme parser to read the data

get all lines
split by empty string - splits into groups
split each group by space #\space
split each group by #\: colon
then collect

|#

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

(define lines (get-lines "input"))
(define groups (string-split-by-empty-string lines))
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

;; (set! input (get-input "input"))

;; sanity check all strings same length
;; (map string-length input)


#|
(set! example (convert-to-2d (get-input "day24/example")))
(set! example2 (convert-to-2d (get-input "day24/example2")))

(define (reset)
   (set! input (get-input "day17/input")))
 (reset)
|#

(format #t "day 4 loaded~%")


;; ----------- puzzle -----------------------------------
#|

    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)
    cid (Country ID)

8 keys cid is optional , others must all be present
|#

(define passport-valid?
  (lambda (x)
    (and
     (assoc 'byr x) 
     (assoc 'iyr x) 
     (assoc 'eyr x) 
     (assoc 'hgt x) 
     (assoc 'hcl x) 
     (assoc 'ecl x) 
     (assoc 'pid x))))

(define groups4 (filter passport-valid? groups3))

(define (part-1)
  (format #t "number of valid passports = ~a ~%" (length groups4)))

#|
next part use regular expressions
(ice-9 regex) module

;; string-match reg-ex string  ->  match object
;; match:substring match index

(define (check-byr s)
  (match:substring (string-match "^([0-9][0-9][0-9][0-9])[c][m]$" s)
		   1))


|#

;; false means fails the check

;; birth year
(define (check-byr s)
  (let ((m (string-match "^([0-9][0-9][0-9][0-9])$" s)))
    (cond
     (m (let ((year (string->number (match:substring m 1))))
	  (and (>= year 1920) (<= year 2002))))
     (#t #f))))

;; issue year
(define (check-iyr s)
  (let ((m (string-match "^([0-9][0-9][0-9][0-9])$" s)))
    (cond
     (m (let ((year (string->number (match:substring m 1))))
	  (and (>= year 2010) (<= year 2020))))
     (#t #f))))

;; expiration year
(define (check-eyr s)
  (let ((m (string-match "^([0-9][0-9][0-9][0-9])$" s)))
    (cond
     (m (let ((year (string->number (match:substring m 1))))
	  (and (>= year 2020) (<= year 2030))))
     (#t #f))))

;; height
(define (check-hgt s)
  (or (let ((m (string-match "^([0-9]+)cm$" s)))
	(cond
	 (m (let ((dist (string->number (match:substring m 1))))
	      (and (>= dist 150) (<= dist 193))))
	 (#t #f)))
      (let ((m (string-match "^([0-9]+)in$" s)))
	(cond
	 (m (let ((dist (string->number (match:substring m 1))))
	      (and (>= dist 59) (<= dist 76))))
	 (#t #f)))))

;; hair colour
(define (check-hcl s)
  (let ((m (string-match "^([#][0-9a-f]{6})$" s)))
    (cond
     (m #t)
     (#t #f))))

(define (check-ecl s)
  (member s '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")))

(define (check-pid s)
  (let ((m (string-match "^([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9])$" s)))
    (cond
     (m #t)
     (#t #f))))

;; ignore cid

(define passport-valid2?
  (lambda (x)
    (and
     (and (assoc 'byr x) (let ((val (cadr (assoc 'byr x))))
			   (check-byr val)))
     (and (assoc 'iyr x) (let ((val (cadr (assoc 'iyr x))))
			   (check-iyr val)))
     (and (assoc 'eyr x) (let ((val (cadr (assoc 'eyr x))))
			   (check-eyr val)))
     (and (assoc 'hgt x) (let ((val (cadr (assoc 'hgt x))))
			   (check-hgt val)))
     (and (assoc 'hcl x) (let ((val (cadr (assoc 'hcl x))))
			   (check-hcl val)))
     (and (assoc 'ecl x) (let ((val (cadr (assoc 'ecl x))))
			   (check-ecl val)))
     (and (assoc 'pid x) (let ((val (cadr (assoc 'pid x))))
			   (check-pid val)))
     )))


(define groups5 (filter passport-valid2? groups4))

(define (part-2)
  (format #t "number of valid passports = ~a ~%" (length groups5)))

#|

scheme@(guile-user) [9]> (part-1)
number of valid passports = 190 
$65 = #t
scheme@(guile-user) [9]> (part-2)
number of valid passports = 121 
$66 = #t
scheme@(guile-user) [9]> 


#|
------------ results ------------

|#










