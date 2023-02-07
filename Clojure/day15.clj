(ns day15
  (:require [input :refer [f->nums nums]]))

(defn triage [it s]
  (let [r (reduce (fn [t [p i]] (if (= 0 (mod (+ t i) p)) (inc t) (reduced t))) (inc s) it)]
    (when (> (- r s) (count it)) s)))

(defn -main [day]
  (let [input (->> day f->nums rest (take-nth 2) (partition 2) vec)
        solve #(first (drop-while nil? (pmap (partial triage (into input %)) (range))))]
    {:part1 (solve []) :part2 (solve [[11 0]])}))


(comment
  (let [test-input "Disc #1 has 5 positions; at time=0, it is at position 4.
Disc #2 has 2 positions; at time=0, it is at position 1."
        input (->> test-input nums rest (take-nth 2) (partition 2))]
    (= '(nil nil nil nil nil 5) (map (partial triage input) (range 6))))
  )
