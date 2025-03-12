
;; look for nessy in text file

(defun at (x2 y)
  (let ((x (- x2 1)))
    (cond
     ((and (>= x 1) (>= y 1))
      (goto-line y)
      (move-to-column (- x 1))
      (string (char-after (point))))
     (t "?"))))


(defun ness (x y)
  (and
   ;; row 1 ness
   (string= "#" (at (+ x 25) (+ y 1)))   
   ;; row 2 ness
   (string= "#" (at (+ x 0) (+ y 2)))
   (string= "#" (at (+ x 5) (+ y 2)))
   (string= "#" (at (+ x 6) (+ y 2)))
   (string= "#" (at (+ x 11) (+ y 2)))
   (string= "#" (at (+ x 12) (+ y 2)))
   (string= "#" (at (+ x 17) (+ y 2)))
   (string= "#" (at (+ x 18) (+ y 2)))
   (string= "#" (at (+ x 19) (+ y 2)))
   ;; row 3 ness
   (string= "#" (at (+ x 1) (+ y 3)))
   (string= "#" (at (+ x 4) (+ y 3)))
   (string= "#" (at (+ x 7) (+ y 3)))
   (string= "#" (at (+ x 10) (+ y 3)))
   (string= "#" (at (+ x 13) (+ y 3)))
   (string= "#" (at (+ x 16) (+ y 3)))
   ))





;; 01234567890123456789012345
;;                          #   1
;; #    ##    ##    ###         2
;;  #  #  #  #  #  #            3
;;                          25
;; 0    56    11,12 17-18-19  
;;  1  4  7  10 13 16


(defun tenk ()
  (interactive)
  (let ((x 0)(y 0)(maxx 4)(maxy 4))
    (while (<= y maxy)
      (setq x 0)
      (while (<= x maxx)
	;; do something
	(goto-line y)
	(move-to-column x)
	(when (ness x y)
	  (message "found ness !"))
	(setq x (+ x 1)))
      (setq y (+ y 1)))))



	
  ;; (goto-char -1) ;; top left
  ;; ;;(move-to-column 5) ;; move to column 5  0 1 2 3 4 5
  ;; (goto-line 1)
  ;; (move-to-column 1)
  ;; (char-after (point))
  ;; (message "message {%s}" (string (char-after (point))))  
  ;; (when (string= "." (string (char-after (point))))
  ;;   (message "DOT !{%s}" (string (char-after (point))))))



;; what character is under point ?
;;(move-to-column 4)


