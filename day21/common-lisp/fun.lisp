
#|
load input.txt into input as a single read ?

we need to be in package aoc for *input* to work or be explicit aoc::*input*

+bug copy paste using mouse and yank on keyboard two different clipboards ?!?

take each ingredient list - need to find for

(SKLT FFVP ZRBX KFFF BKVS TSXF QRZZXR ZMLFV BTJT JXVR KQHDGL
                NPCCPR NMB ZSRXJR ZXHQZ CJBNKL QHVZ GDTFN BLLQ HJSFFQ JGNR GPTV
                GBRZ MFMS PGLLP RFM HPLQJV DMDQ DNXZFJ LVZZX FZSL DMFTNG SLC
                GHKPJC RZK ZKLJRVC PQXMX CVXK HMNKLC BMJ CDGM VRVSNF MKSMF CZSX
                FBLV JTDN FZVXQ SJTMNS MJZRJ MBGR GGMNZ RMTJLX VGLD TMK HZV
                NMRNMGCR XJTGF GJPCZ VQMSH QBXHT KPB BLFBJJH KPLZMN KTC LHMFQHC
                SJMJF QLPGL RVBR KGKRHG ZCLQ ZTFPGG BJDT ZSHFB CHNGQ GNS VLQPDM
                BDFS FZSDSN SRKT MFKL VTXM XLCTNVTX KSCL FRGGD XZTLR GGQC KQQ
                (PEANUTS SOY SHELLFISH))
(KTZZP LNQN LFRCRD SRKT KGKRHG PGLLP QHVZ VMXB CDGM XZTLR DMFTNG
                SKLT DMDQ ZTFPGG TPTVD MKSMF BDFS CNNH LLRNPQP PNB VZL GNKXPMX
                QFBNB GFLR NMB VPQLJ GHKPJC MMCM CHNGQ NBGB NGFBTM BMJ VGKKP
                GDTFN HQXN ZSRXJR KHGKDR RVBR SCX PGXGV PPVHXT ZMLFV HFXPZ KPB
                THRDBHMV DNXZFJ KQHDGL BBSM GNS HPFB KBCPN FFPMJ SJTMNS FXNXP
                KRBR TQSTJFDC CBKN LVZZX HMNKLC THFVRJ MJZRJ HPLQJV FZVXQ
                BLFBJJH RBGQHB FZSL DBKJJHG RGQC VJSVX SJMJF (SHELLFISH))
               
shellfish is common allergen in first and second list , what is the common symbol ?

find all SHELLFISH ... and a list of symbols with SHELLFISH ... find symbol that is in
all sets of SHELLFISH
has to be unique solution or else problem is undecidable, true ? or more than one solution

+bug eq is the symbol eq
    #' shorthand for the function for
    #'eq  is the function for eq 

|#

(defpackage :aoc
  (:use :cl))

(in-package :aoc)

(defparameter *input*
  (let ((result nil))
    (with-open-file (stream "../input.txt")
      (setq result (read stream)))
    (format t "the result is ~a~%" result)
    result))


(defun foo ()
  (let ((*input* '(this is my replacement list)))
    (format t "input is ~a~%" *input*)
    (bar)))

(defun bar ()
  (format t "~a~%" *input*))

;; last butlast must be lisp macros then ..

;; (defun last (xs)
;;   (let ((ptr xs))
;;     (loop while (not (null (cdr ptr))) do
;;       (setq ptr (cdr ptr)))
;;     (cond
;;       ((null ptr) ptr)
;;       (t (car ptr)))))
    
    


;; find all allergens
(defparameter *allergens* nil)

(defun find-all-allergens ()
  (let ((all nil)
	(count 0))
    (loop for line in *input* do
      (let ((symbols (butlast line))
	    (allergens (first (last line))))
	(incf count)      
	(format t "~%count => ~a ~%" count)
	(format t "symbols => ~a ~%" symbols)
	(format t "allergens => ~a ~%" allergens)

	(loop for allergen in allergens do
	  (cond
	    ((member allergen all) t)
	    (t (pushnew allergen all :test #'eq))))))
    (setq *allergens* all)
    all))


#|
symbol sym and set of keys 
|#
(defun run-allergens ()
  (loop for allergen in (find-all-allergens) do 
    (let ((keys nil)(init t))
      (loop for line in *input* do
	(let ((symbols (butlast line))
	      (allergens (first (last line))))
	  (cond
	    (init (setq init nil)
		  (setq keys symbols))
	    (t
	     (let ((keys2 nil))
	       (loop for k in symbols do
		 (when (not (member k 
	      
	      
	  (when (member sym symbols)
	    (loop for k in allergens do
		  ...))))))
  

      
      



