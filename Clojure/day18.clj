(ns day18
  (:require [input :refer [f->str]]))

(defn -main [day]
  (let [input (->> day f->str seq)
        traps (->> ["^^." ".^^" "^.." "..^" ] (map seq) (into #{}))
        nxt (fn [r]
              (reduce
               (fn [n lcr] (conj n (if (contains? traps lcr) \^ \.)))
               [] (partition 3 1 (conj (vec (concat [\.] r)) \.))))]
    (loop [row input, result {:part1 nil :part2 0}, n 0]
      ;; (when (= 0 (mod n 10000)) (prn n (result :part2)))
      (let [s (count (filter #(= % \.) row))]
        (cond
          (= n 400000) result
          (= n 40) (recur (nxt row) (-> result (assoc :part1 (result :part2)) (update :part2 + s)) (inc n))
          :else    (recur (nxt row) (update result :part2 + s) (inc n)))))))
