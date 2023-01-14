(ns day03
  (:require [input :refer [f->nums]]))

(defn transpose [m] (apply mapv vector m))

(defn -main [day]
  (let [rows (->> day f->nums (partition 3))
        columns (->> rows transpose flatten (partition 3))
        sides (fn [[a b c]] [[a b c] [a c b] [b c a]])
        triangle? #(every? (fn [[a b c]] (> (+ a b) c)) (sides %))
        solve #(->> % (filter triangle?) count)]
    {:part1 (solve rows) :part2 (solve columns)}))
