(ns day5.core
  (:gen-class))

(require 'clojure.math)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(-> 
    slurp
    (fn [] "./../../input"))

(def input (slurp "./../../input.txt"))


(def lines
  (let [text (slurp "./../../input.txt")]
    (clojure.string/split-lines text)))

;; lines ==>
;; ["FFBBBFFLRL"
;;  "BFFBFBFRLR"
;;  "FFFBBFBRRR"
;; ....

;; 1 2 3 4
;; C0 : 1 2 3 ... 128
;; C1 : 1 .. 64  ; 65 .. 128
;; C2 : 1 .. 32  ; 33 .. 64 ; 65 ..
;; C3 : 1 .. 16
;; C4 : 1 .. 8
;; C5 : 1 .. 4
;; C6 : 1 .. 2
;; C7 : 1 .. 1

;; 2 raised to 7 gives 128.0 floating point number ? 
(clojure.math/pow 2 7) ;; ==> 128.0

;; loop - recur
;; begin 0 to 127 inclusive , split into half
(defn upperRange [from to]
  true)

(defn lowerRange [from to]
  true)

(- (/ (+ 127 1) 2) 1)

"FBFBBFFRLR"


(defn toCol
  [theRow s i]
  (loop [delta 8
         from 0
         to 8
         index i]
    (let [ch (get s index)]
      (cond
        (= delta 1) (let [theCol from]
                      (list theRow theCol))
        (= ch \L) (let [step (/ delta 2)]
                    (recur step from (+ from step) (+ index 1)))
        (= ch \R) (let [step (/ delta 2)]
                    (recur step (- to step) to (+ index 1)))))))
  

(defn toRow
  "determine row from F or B"
  [s]
  (loop [delta 128
         from 0
         to 128
         index 0]
    (let [ch (get s index)]
      (cond
        (= delta 1) (toCol from s index)
        (= ch \F) (let [step (/ delta 2)]
                    (recur step from (+ from step) (+ index 1)))
        (= ch \B) (let [step (/ delta 2)]
                    (recur step (- to step) to (+ index 1)))))))


(defn seatID
  "multiply row by 8 then add column"
  [rc]
  (let [row (nth rc 0)
        col (nth rc 1)]
    (list row col (+ (* row 8) col))))


(seatID (toRow "FBFBBFFRLR")) ;;
(seatID (toRow "BFFFBBFRRR")) ;;: row 70, column 7, seat ID 567.
(seatID (toRow "FFFBBBFRRR")) ;;: row 14, column 7, seat ID 119.
(seatID (toRow "BBFFBBFRLL")) ;;: row 102, column 4, seat ID 820.

;; find highest seatID on the boarding passes

(let [f (fn [x] (nth (seatID (toRow x)) 2))]
  (apply max (map f lines))) ;; ==> 913


(let [f (fn [x] (seatID (toRow x)))]
  (filter (fn [x] (= (nth x 2) 913)) (map f lines)))
;;==> ((114 1 913))



