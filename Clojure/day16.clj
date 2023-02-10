(ns day16
  (:require [input :refer [f->str]]))

;; that's a brutal one (in bb it takes more than 2 mins to finish p2, TODO: some math optimization needed)

(defn -main [day]
  (let [dr #(apply str % \0 (map {\1 \0 \0 \1} (reverse %)))
        dx (fn [it n] (take n (first (drop-while #(< (count %) n) (iterate dr it)))))
        chs (fn [it] (map #(if (apply = %) \1 \0) (partition 2 it)))
        chx (fn [it n] (loop [c (chs (dx it n))] (if (odd? (count c)) (apply str c) (recur (chs c)))))
        solve (partial chx (f->str day))]
    {:part1 (solve 272) :part2 (solve 35651584)}))
