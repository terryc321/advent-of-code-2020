

(comment
  vecinput is a vector in clojure parlance of values
  find two different? values that sum to 2020 , v1 v2
  if we can find two such numbers , their product  v1*v2 is the answer we require

  solution
  slurp in each line
  convert strings to integers
  now have a vector of ints
  how do we iterate over the vector without creating a mess of values ?
  for loop with
  i1 index 0 to n-1
  i2 index 1 to n-2
  where i1 < i2
  such v1 = vecinput[i1]
       v2 = vecinput[i2]
  )


(ns day1.core
  (:gen-class))

;; can run this in lein repl (-main)
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(defn fact [n]
  (if (< n 2) 1
      (* (fact (- n 1)) n)))


;; M-x cider-jack-in-clj
;; takes a while ...
;;(day1.core/-main)

(def input (slurp "../../input"))
;;(System/getProperty "user.dir")

(def ivec (clojure.string/split-lines input))
(vector? ivec)

;; remove first last element of vector?
(def ivec (rest (pop ivec)))

;; convert string to Integer use Integer.

(def input (map (fn [x] (Integer. x)) ivec))

;; length input ?
(count input)

;; find two entries that sum to 2020 and multiply them together
;; can they both be the same number ?
(range 0 20)

;;for i in range 0 len-1
;; for j in range i+1 to len-1
;; i < j then v1 = input[i]
;;            v2 = input[j]
;;       if v1 + v2 = 2020 then v1 * v2
;;

;; vectorized version of list
(def vecinput (vec input))

(defn seek  "find two numbers that sum to 2020"
  []
  (let [len (count vecinput)]
    (map (fn [i]
           (map (fn [j]
                  (let [vi (get vecinput i)
                        vj (get vecinput j)]
                  (cond
                    (and (< i j)(= 2020 (+ vi vj)))  (list i j vi vj (+ vi vj) (* vi vj))
                    true nil)))
                (range 0 len)))
         (range 0 len))))


(seek)


(filter (fn [x] (not (= x nil))) (seek))

;; this is the proposed solution in a massive sea of nil results
;;(60 89 1704 316 2020 538464)
                               
;; (map (fn [i] (println "i = " (str i))) (range 1 10))

;; (conj (conj nil 1) 2)

;; (and true false)
;; (and true)
;; (and false)

;; (get [1 2 3] 0)

;; (println 567)
;; (println 123 456)
;; (println 123)

(defn seek2  "find two numbers that sum to 2020"
  []
  (let [len (count vecinput)]
    (for [i1 (range len)
          i2 (range len)
          :when (< i1 i2)]
      (let [v1 (get vecinput i1)
            v2 (get vecinput i2)]
        (if
            (= 2020 (+ v1 v2))
          (println (list i1 i2 v1 v2 (+ v1 v2) (* v1 v2))))))))

(seek2)

(defn seek3
  "find two numbers that sum to 2020"
  []
  (let [len (count vecinput)]
    (filter (fn [x] (not (= x nil)))
            (for [i1 (range len)
                  i2 (range len)]
              (if (< i1 i2)
                (let [v1 (get vecinput i1)
                      v2 (get vecinput i2)]
                  (if (= 2020 (+ v1 v2))
                    (list i1 i2 v1 v2 (+ v1 v2) (* v1 v2)))))))))

(seek3)

;; part1 solved
(defn part1 [] (seek3))
;; ((60 89 1704 316 2020 538464))
;; 538464 expected answer


(comment
  part2 requires that there are 3 such numbers to sum to 2020 and answer is their product
  )



(defn seek4
  "find three numbers that sum to 2020"
  []
  (let [len (count vecinput)]
    (filter (fn [x] (not (= x nil)))
            (for [i1 (range len)
                  i2 (range len)
                  i3 (range len)]
              (if (and (< i1 i2) (< i2 i3))
                (let [v1 (get vecinput i1)
                      v2 (get vecinput i2)
                      v3 (get vecinput i3)]
                  (if (= 2020 (+ v1 v2 v3))
                    (list i1 i2 i3 v1 v2 v3 (+ v1 v2 v3) (* v1 v2 v3)))))))))


(seek4)
;; part2 solved
(defn part2 [] (seek4))
;;(part2)
;;((30 121 172 615 903 502 2020 278783190))
;; proposed solution 278783190
;; accepted








