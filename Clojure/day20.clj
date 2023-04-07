(ns day20
  (:require [input :refer [f->str]]))

(defn -main [day]
  (let [input (->> day f->str (re-seq #"\d+") (map parse-long) (partition 2) (sort-by first))
        max-ip 4294967295]
    (loop [[[a b] & rx] (rest input), [x y] (first input), result {:part1 [] :part2 0}]
      (cond
        (empty? rx) (-> result (update :part1 first) (update :part2 + (- max-ip y)))
        (> a (inc y)) (recur rx [a b] (-> result (update :part1 conj (inc y)) (update :part2 + (dec (- a y)))))
        :else         (recur rx [x (max b y)] result)))))
