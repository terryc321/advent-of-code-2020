
;; clear clojure repl C-u C-c C-o   C-[uco]

;; aoc 2020 day 4 
;; lack multiline comments is a little annoying
;; comment s expression is totally useless 
;; (comment
;;   so whats the puzzle exactly \?

;; long line of passengers with passports , dont know which are valid and which are not

;; The automatic passport scanners are slow because they're having
;; trouble detecting which passports have all required fields. The
;; expected fields are as follows:

;; byr (Birth Year)
;; iyr (Issue Year)
;; eyr (Expiration Year)
;; hgt (Height)
;; hcl (Hair Color)
;; ecl (Eye Color)
;; pid (Passport ID)
;; cid (Country ID)

;; Passports are separated by blank lines.
  
;;   have a file input

;; iyr:1928 cid:150 pid:476113241 eyr:2039 hcl:a5ac0f
;; ecl:\#25f8d2
;; byr:2027 hgt:190

;; hgt:168cm eyr:2026 ecl:hzl hcl:\#fffffd cid:169 pid:920076943
;; byr:1929 iyr:2013

;; hgt:156cm ecl:brn eyr:2023
;; iyr:2011
;; hcl:\#6b5442 pid:328412891 byr:1948
;;   )


;; split by regex space ; split by regex colon : 
(map (fn [s] (clojure.string/split s #":"))
     (clojure.string/split "iyr:1928 cid:150 pid:476113241 eyr:2039 hcl:a5ac0f" #" "))
;; ==> (["iyr" "1928"] ["cid" "150"] ["pid" "476113241"] ["eyr" "2039"] ["hcl" "a5ac0f"])


(ns day4.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


;; (require ['clj-antlr.core :as 'antlr])
;; (def parser (antlr/parser "grammars/Day4.g4"))

;; slurp file ?
(def input (slurp "../../input.txt"))

(def string-lines (clojure.string/split-lines input))

;; take all the text lines and collect them together into useful units
;; how do we iterate over the string-lines
(defn collect
  "collect non empty lines together"
  []
  (let [lim (count string-lines)]
    (loop [i 0
           cur nil
           acc nil]
      (cond
        (>= i lim) (if (= cur nil) acc (conj acc cur))
        (= "" (get string-lines i)) (recur (+ i 1) nil (conj acc cur))
        true (recur (+ i 1) (if (= cur "")
                              (get string-lines i)
                              (str cur " " (get string-lines i))) acc)))))


;;(pprint (reverse (collect)))

;;(apply (fn [&args] (str x " ")) '("abc" "def" "ghi"))


(defn frankenstein
  "splits up string based on spaces , then by properties seperated by a colon
   subs s1 1 = substring everything but first character  
  "
  [s1]
  (map (fn [s2] (clojure.string/split s2 #":"))
       (clojure.string/split (subs s1 1) #" ")))

;;(.str (rest "alpha"))


(def vec-list-passports (map frankenstein (reverse (collect))))

;;(pprint vec-list-passports)


;; each passport currently looks like a list of vectors , convert to a single clojure map
 ;; (["hcl" "#d257c7"]
 ;;  ["eyr" "2036"]
 ;;  ["iyr" "2018"]
 ;;  ["ecl" "#5b11eb"]
 ;;  ["byr" "1950"])

;;
;; {} an empty map
;;(contains? {} "fred")
;;(contains? {"fred" 2} "fred")
;;(contains? {"fred" 2 "sally" 3} "fred")

(defn convert
  [xs]
  (let [lim (count xs)]
    (loop [i 0
           hash {} ]
      (cond
        (>= i lim) hash
        true (recur (+ i 1) (let [[key value] (nth xs i)]
                              (assoc hash key value)))))))


(def passports (map convert vec-list-passports))

;;(pprint passports)

(defn valid-passport
  "a passport is valid if it has all these keys
byr (Birth Year)
iyr (Issue Year)
eyr (Expiration Year)
hgt (Height)
hcl (Hair Color)
ecl (Eye Color)
pid (Passport ID)
cid (Country ID)  "
  [p]
  ;; can miss "cid"
  (let* [properties (map (fn [x] (contains? p x))
                         '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" ))
         alltrue (= (count (filter (fn [x] (= x false)) properties)) 0)]
    alltrue))


(def valid-passports
  (filter valid-passport passports))

(def part1 (count valid-passports))




;; clojure suffers from {and} macro condition cannot call apply on it
;;(apply and '(true false true))  
  ;; all-true ?

;; (filter (fn [x] (= x false)) '(true false true false))
;;   (= nil '())
;;   (filter (fn [x] (= x true)) '(true false true false))


;; (nth '(1 2 3) 0)
;; (concat [1 2 3] [4 5 6] [7 8 9])


;; (def test-string1 (get string-lines 0))
;;(pprint (parser test-string1))







