
;; chicken scheme

#|
any language need a front end parser  - generic requirement for all languages 
load file into string
parse ID: ID ID ALT ID ID 
parse abab strings
parse "a"
parse "b"

id
sequence
alternation


have to parse this line 

66: 69 116 | 9 115

|#

;; first second third
(import srfi-1)

;; sort 
(import (chicken sort))

(import (chicken pretty-print))

;; for current directory
(import (chicken process-context))
;;(current-directory)
;;(change-directory)


;; for readlines 
(import (chicken io))

;; format #t .,...
(import (chicken format))

(import regex) ;; string-split-fields?/


#|
;; word count a file 
(with-input-from-file "../input.txt" (lambda ()
				       (let loop ((count 0) (c (read-char)))
					 (if (eof-object? c) count
					     (loop
					      (if (char=? c #\newline) (+ count 1) count)
                                              (read-char))))))
|#



;; parse this into (define (s66 ?) (s-alt (s-seq s69 s116)(s-seq 9 115)))
;; ("66:" "69" "116" "|" "9" "115")
;;
;; if line is null - ignore
;; if line starts letter a or b then its region 2
;; otherwise region1 parse


;; how parse a sequence of expressions already split

(define (de-colon s)
  (string->number 
   (list->string (reverse (cdr (reverse (string->list s)))))))

(define (colon? s)
  (char=? (car (reverse (string->list s))) #\:))

;; word count??
(define sslines
  (let ((input-lines (with-input-from-file "../input.txt" (lambda ()
							    (read-lines)))))
    (letrec ((split-input-lines (lambda ()
				 (map (lambda (s)
					(cond
					 ((= 0 (string-length s)) '())
					 ((or (char=? (string-ref s 0) #\a)
					      (char=? (string-ref s 0) #\b))
					  s)
					 (#t (string-split-fields "[^ ]+" s))))
				      input-lines))))
      (split-input-lines))))

#|
region-a
 ("66:" "69" "116" "|" "9" "115")
 ("91:" "95" "9" "|" "109" "69")
 ("14:" "110" "69" "|" "15" "9")
 ...
region-b
 "bbabbbaaaaaabbabbaabaaabbaababbbabbbabbbababbbbbbbbabbbbbbabaaaa"
 "aabbbabbabaaaababbbaaabb"
 "baababbbaababaaaabbaababbabbbaaabaaaabbbaaaabbaaabbaabba"
 "abbbbaaabbaaaababbbabaaabbbabbbbbabaabab"
...
...
|#
(define region-a '())
(define region-b '())

(define (ssregions xs)
  (let ((before '())
	(after '()))
    (let loop ((lines xs))
      (cond
       ((null? lines)
	(set! region-a (reverse before))
	(set! region-b after))
       (#t
	(let ((line (car lines)))
	  (cond ;; () means region marker
	   ((null? line)
	    (set! after (cdr lines))
	    (loop '())) 
	   ((and (list? line) (colon? (car line)))
	    (set! before (cons line before))
	    (loop (cdr lines))))))))))

(ssregions sslines)

    


#|
so if given
"66:" .... take CDR get lower line 
"n" "n" "n" "n" "|" "n" "n" "n" "n" ... | ....

|#

;;
(define (fug xs)
  (define (fug2 xs ys zs)
    (cond
     ((null? xs) (map reverse (cons ys zs)))
     ((string=? "|" (car xs)) (fug2 (cdr xs) '() (cons ys zs)))
     (#t (fug2 (cdr xs) (cons (car xs) ys) zs))))
  (reverse (fug2 xs '() '())))

(let ((xs '("2" "3" "4" "|" "5" "6" "7")))
  (fug xs))

(define (fab xs)
  (let ((root `(alt ,@(map (lambda (xs)
			    `(seq ,@(map
					  (lambda (i) (de-stringify i))
					  xs)))
			   (fug (cdr xs))))))
    `(br ,(de-colon (car xs)) ,(length root) ,root)))

    ;;(cond
    ;; ((not (= (length root) 1))  `(br ,(de-colon (car xs)) ,root))
    ;; (#t `(br ,(de-colon (car xs)) ,(car (cdr root)))))))




(define (de-stringify s)
  (cond
   ((and (char>=? (string-ref s 0) #\0) (char<=? (string-ref s 0) #\9))
    `(br ,(string->number s)))
   (#t `(char ,(string-ref s 1)))))


(define fregion-a (map fab region-a))

(define highest-id
  (let ((largest 0))
    (map 
     (lambda (xs) (let ((n (second xs)))
		    (when (> n largest)
		      (set! largest n))))
     fregion-a)
    largest))

;; inclusive says 135
;; want vector atleast as large as highest-id

(define svector (make-vector (+ 1 highest-id) '()))

(define (fill-vector)
  (let loop ((xs fregion-a))
    (cond
     ((null? xs) #f)
     (#t (let* ((branch (car xs))
		(id (second branch))
		(alt (fourth branch)))
	   (vector-set! svector id alt)
	   (loop (cdr xs)))))))

(fill-vector)

#|
id 0 : (alt (seq (br 8) (br 11)))
id 1 : (alt (seq (br 86) (br 9)) (seq (br 95) (br 69)))
id 2 : (alt (seq (br 69) (br 88)) (seq (br 9) (br 23)))
...

given a string , here are the 10 of the problem - how do we match them against
alt = alternatives
seq = sequence
br  = branch off to X

("bbabbbaaaaaabbabbaabaaabbaababbbabbbabbbababbbbbbbbabbbbbbabaaaa"
 "aabbbabbabaaaababbbaaabb"
 "baababbbaababaaaabbaababbabbbaaabaaaabbbaaaabbaaabbaabba"
 "abbbbaaabbaaaababbbabaaabbbabbbbbabaabab"
 "aaabbbbbabbbbbabbabbaabbaaaaababababbbbaaaaabbbabbbbabbababbaaabaaaababa"
 "bbbabaaaabbaaabbbababababababaaaabaabbbabaaaabbbabbabbabbbaaabbaaabaaaaa"
 "bbaaaabbaabbbaabbabaaaba"
 "abaabbabbaaaaabbbbbabaabbabbbababaaababb"
 "baaaaabbbabbbbbbabababba"
 "baaabbbaabbaabababbbaaab")

if sequence seq fails , then it just fails
if alternate alt fails , then it can fail to next alternative (ie try that instead) ,
  if no more alternatives possible then alternate fails

match made when all characters of string are consumed and we return to empty stack

keep let it generate

|#


#|
(alt X Y Z)
(seq X Y Z)
(br X)
(char C)

hist = history ie characters produced so far

|#
(define (tgen-recur expr hist restarts h-r)
  (format #t "looking at ~a ~%" expr)
  (cond
   ((eq? (car expr) 'alt) (tgen-alt expr hist restarts h-r))
   ((eq? (car expr) 'seq) (tgen-seq expr hist restarts h-r))
   ((eq? (car expr) 'br) (tgen-br expr hist restarts h-r))
   ((eq? (car expr) 'char) (tgen-char expr hist restarts h-r))
   (#t (format #t "dont know how to handle ~a~%" expr))))

#|
next ...
prev 

(call/cc (lambda (cont)

write in CPS style conitnuation passing 

wheere do lambdas and restarts come in ? 

|#

;; try all alternatives one after another 
(define (tgen-alt expr hist restarts h-r)
  (let loop ((xs (cdr expr)))
    (cond
     ((null? xs) #f)
     (#t (let ((attempt (car xs)))
	   (tgen-recur attempt hist)
	   (loop (cdr xs)))))))

;; try each in sequence - if any fail , all fail
(define (tgen-seq expr hist restarts h-r)
  (let loop ((xs (cdr expr)))
    (cond
     ((null? xs) #f)
     (#t (let ((attempt (car xs)))
	   (tgen-recur attempt)
	   (loop (cdr xs)))))))

;; feed this branch into recursive machine
(define (tgen-br expr hist restarts h-r)
  (let ((i (second expr)))
    (tgen-recur (vector-ref svector i))))

;; output that character
(define (tgen-char expr hist cont)
  (let ((ch (second expr)))
    (h-r (cons ch hist) restarts)
    ;; here we might insert something clever
    
    ))

;; only one of the alternative alt legs can be used , exclusively
(define (dummy-run)
  (let ((expr0 (vector-ref svector 0))
	(hist '())
	(restarts '()))
    (tgen-recur expr0
		hist
		restarts
		(lambda (fhist restarts)
		  (format #t "~a" (reverse fhist))
		  (restarts)))))









     
     
     
  





