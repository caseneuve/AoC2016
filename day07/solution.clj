(ns day07.solution
  (:require [input :refer [f->lines lines]]
            [clojure.string :refer [replace]]
            [clojure.set :refer [intersection]]))

(def hyper #(map second (re-seq #"\[(\w+)\]" %)))
(def non-hyper #(re-seq #"\w+" (replace % #"\[\w+\]" " ")))

(defn abx [s n in?]
  (reduce into [] (map (partial partition n 1) ((if in? hyper non-hyper) s))))

(defn abba [it in?]
  (reduce
   (fn [r [a b c d]] (if (and (= a d) (= b c) (not= a b)) (reduced (not r)) r))
   in? (abx it 4 in?)))

(defn tls [it] (if (and (abba it true) (abba it false)) 1 0))

(defn aba [it in?]
  (reduce
   (fn [r [a b c]]
     (if (and (= a c) (not= a b)) (conj r (if in? (str a b a) (str b a b))) r))
   #{} (abx it 3 in?)))

(defn ssl [it] (if (first (intersection (aba it true) (aba it false))) 1 0))


(defn -main [day]
  (let [solve #(->> (f->lines day) (map %) (apply +))]
    {:part1 (solve tls) :part2 (solve ssl)}))


(comment
  (let [test-input {:1 ["abba[mnop]qrst\nabcd[bddb]xyyx\naaaa[qwer]tyui\nioxxoj[asdfgh]zxcvbn", '(1 0 0 1)],
                    :2 ["aba[bab]xyz\n xyx[xyx]xyx\naaa[kek]eke\nzazbz[bzb]cdb", '(1 0 1 1)]}]
    (for [[p [it exp]] test-input] (= exp (->> it lines (map (p {:1 tls :2 ssl}))))))
)
