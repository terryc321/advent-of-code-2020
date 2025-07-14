
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

;; for current directory
(import (chicken process-context))
;;(current-directory)
;;(change-directory)


;; for readlines 
(import (chicken io))


(with-input-from-file "../input.txt" (lambda ()
				       (let loop ((count 0) (c (read-char)))
					 (if (eof-object? c) count
					     (loop
					      (if (char=? c #\newline) (+ count 1) count)
					      (read-char))))))


;; using regex module
(import regex)

(let ((s "66: 69 116 | 9 115")) (string-split-fields "[^ ]+" s))

(define (foo) 22)
