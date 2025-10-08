
(comment
  reading input-pristine.txt file in slurp split-lines
  then ?
  for some width
  right 3 down 1

  pattern repeats horizontally but not vertically

  a string repeated
  alpha alpha alpha alpha
  ........ n steps forward .... what

  \# represents a tree ?
  . represents open square
  
  )



(ns day3.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


;; slurp file ?
(def input (slurp "../../input-pristine.txt"))
(def string-lines (clojure.string/split-lines input))


;; routine forward and a given string
;; computes what would be the character if such a string was concatenated with itself until its length was
;; atleast as great as forward index required

;; assuming str is non zero length string !?
(defn expand-string
  "stretch a string by repeating itself until greater than some index"
  [s i]
  (loop [snake s
         len (count s)]
    (cond
      (<= len i) (recur (str snake s) (+ len (count s)))
      true (nth snake i))))

(expand-string "alpha" 0)
(expand-string "alpha" 1)
(expand-string "alpha" 2)
(expand-string "alpha" 3)
(expand-string "alpha" 4)
(expand-string "alpha" 5)

;; start at x = 0 , y = 0
;; model moving forward by 3 , simply increase coordinate x by 3
;; moving downward by 1 , increase coordinate y by 1
;; do we ignore initial position ? yes i think we do , ignore x=0 y=0
    
(defn movement
  "move 3 to right , move 1 down"
  []
  (let [ylim (count string-lines)]
    (loop [x 0
           y 0
           trees 0]
      (let* [x2 (+ x 3)
             y2 (+ y 1)]
        (cond
          (>= y2 ylim) trees
          true (let* [s (get string-lines y2)
                      ch (expand-string s x2)]
                 (cond
                   (= ch \#) (recur x2 y2 (+ trees 1))
                   true (recur x2 y2 trees))))))))

(def part1 (movement))

;;part1 ==> 151

(defn specialise-movement
  "move DX to right , move DY down"
  [dx dy]
  (let [ylim (count string-lines)]
    (loop [x 0
           y 0
           trees 0]
      (let* [x2 (+ x dx)
             y2 (+ y dy)]
        (cond
          (>= y2 ylim) trees
          true (let* [s (get string-lines y2)
                      ch (expand-string s x2)]
                 (cond
                   (= ch \#) (recur x2 y2 (+ trees 1))
                   true (recur x2 y2 trees))))))))

(def part2
  (apply *
         (map (fn [x] (specialise-movement (nth x 0) (nth x 1)))
              '((1 1)(3 1)(5 1)(7 1)(1 2)))))

;; part2 ==> 7540141059 



