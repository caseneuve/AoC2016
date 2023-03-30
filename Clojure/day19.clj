(ns day19
  (:require [input :refer [f->str]]
            [clojure.math :refer [pow]]))

(defn iosephus [n]
  ;; the binary trick for the Josephus Problem
  (-> n (Long/toString 2) (subs 1) (str "1") (Long/parseLong 2)))

(defn opposite [n]
  (loop [p 0]
    (let [x (int (pow 3 p))]
      (cond
        (= x n) n
        (> x n) (let [y (int (pow 3 (dec p))), d (- n y)] (cond-> d (> d y) (+ (- d y))))
        :else (recur (inc p))))))

(defn -main [day]
  (let [input (->> day f->str parse-long)]
    {:part1 (iosephus input) :part2 (opposite input)}))


(comment

  ;; part 2 heuristics:

  ;; Brute force solution would take ages -- but printing some solutions on small numbers shows the pattern very quickly
  ;; similarly to the part 1 (the Josephus Problem, see: https://youtu.be/uCsD3ZGzMgE)

  (defn -brute [n]
    (loop [[x & xs] (range 1 (inc n))]
      (if (nil? xs) x
          (let [[l r] (split-at (quot (dec (count xs)) 2) xs)]
            (recur (concat l (rest r) [x]))))))

  (doseq [x (range 1 100)]
    (let [n (-brute x)] (when (= n 1) (println "====")) (prn x n)))

  ;; 1 1 -> 3^0
  ;; ====
  ;; 2 1
  ;; 3 3 -> 3^1
  ;; ====
  ;; 4 1
  ;; 5 2
  ;; 6 3
  ;; 7 5
  ;; 8 7
  ;; 9 9 -> 3^2 syncs on powers of 3, resets to 1 afterwards
  ;; ====
  ;; 10 1
  ;; ... goes up by 1 til 18, then goes up by 2 until reaches next power of 3, i.e. 3^3
  ;; 18 9
  ;; 19 11
  ;; 20 13
  ;; 21 15
  ;; 22 17
  ;; 23 19
  ;; 24 21
  ;; 25 23
  ;; 26 25
  ;; 27 27
  ;; ...

  ;; when x is a power of 3, then x is the winner,
  ;; otherwise we need to find the highest power of 3 [hp3] smaller than x:
  ;; - if the difference [D] (x - hp3) is larger than hp3, the winner is 2D - hp3
  ;; - if it's smaller, then the winner is D

  ;; for example:
  ;; 3 = 3^1      --> 3 is the winner
  ;; 4 = 3^1 + 1  --> 1 is smaller than 3, so the winner is 1
  ;; 6 = 3^1 + 3  --> 3 is equal to 3, so the winner is still (6 - 3^1)
  ;; 7 = 3^1 + 4  --> 4 is higher than 3^1, so the winner is 2 * 4 - 3 = 5

  )
