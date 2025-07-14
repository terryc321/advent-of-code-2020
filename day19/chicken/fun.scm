
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

(import (chicken pretty-print))

;; for current directory
(import (chicken process-context))
;;(current-directory)
;;(change-directory)


;; for readlines 
(import (chicken io))

;; word count??
(define input-lines 
  (with-input-from-file "../input.txt" (lambda ()
					 (read-lines))))

;; split each line based on space character
(define split-input-lines (map (lambda (s)
				 (cond
				  ((= 0 (string-length s)) '())
				  ((or (char=? (string-ref s 0) #\a)
				       (char=? (string-ref s 0) #\b))
				   s)
				  (#t (string-split-fields "[^ ]+" s))))
			       input-lines))









#|
;; word count a file 
(with-input-from-file "../input.txt" (lambda ()
				       (let loop ((count 0) (c (read-char)))
					 (if (eof-object? c) count
					     (loop
					      (if (char=? c #\newline) (+ count 1) count)
                                              (read-char))))))
|#


;; using regex module
(import regex)


(define (foo) 22)
