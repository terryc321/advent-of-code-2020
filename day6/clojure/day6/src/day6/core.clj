(ns day6.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def input (slurp "./../../input.txt"))

(def lines (clojure.string/split-lines input))

;; form groups seperated by empty lines ""
;; this is boring - almost 2nd time had to write this routine 
(defn groups [lines]
  (let [lim (count lines)
        i 0]
    (loop [i 0
           curgroup []
           accumulate []]
      (cond
        (>= i lim) (if (= curgroup []) accumulate (conj accumulate curgroup))
        true (let [line (get lines i)]
               (cond
                 (= line "") (recur (+ i 1) [] (conj accumulate curgroup))
                 true (recur (+ i 1) (conj curgroup line) accumulate)))))))


(def group-lines (groups lines))
(def xs-group-lines (map (fn [x] (apply str x)) (groups lines)))

;; how do i use a map - add stuff into it ?
;; (hash-map :g 1 ,:b 2 ,:c 3 ,:g 2)
;; (apply hash-map '(g 1 b 2 c 3 g 2))
;; (apply hash-map (.split "a b c a b c" " "))

;;join a hash-map together  
;;(merge (hash-map :a 2 :b 1)(hash-map :a 1 :b 2))

(def first-group (apply str (nth group-lines 0)))

(defn fix [str]
  (let [ilim (count str)]    
    (loop [i 0
           m {}]
      (cond
        (>= i ilim) m
        true (recur (+ i 1) (merge m (hash-map (get str i) i)))))))

(def counts (map (fn [x] (count (fix x))) xs-group-lines))

(def part-one (apply + counts))

;; part-one 6565

