(ns day01.solution
  (:require [input :refer [f->str]]
            [clojure.set :refer [intersection]]))

(defn manhattan [[ax ay] [bx by]] (+ (abs (- ax bx)) (abs (- ay by))))
(defn v+ [v1 v2] (map #(apply + %) (mapv vector v1 v2)))

(defn parse [it]
  (->> it (re-seq #"(\D)(\d+)") (map (fn [[_ a b]] [({"R" 1 "L" -1} a) (parse-long b)]))))

(defn solve [it p]
  (->> (reduce
        (fn [[pos dir visited] [turn steps]]
          (let [visited (conj visited pos)
                dir (mod (+ dir turn) 4)
                nesw ([[0 1] [1 0] [0 -1] [-1 0]] dir)
                route (->> pos (iterate #(v+ nesw %)) rest (take steps))
                seen (intersection (set route) visited)]
            (if (or (= p 1) (empty? seen)) [(last route) dir (into visited route)] (reduced seen))))
        [[0 0] 0 #{}] it)
       first (manhattan [0 0])))

(defn -main [day]
  (let [input (->> day f->str parse)]
    {:part1 (solve input 1) :part2 (solve input 2)}))


(comment
  (let [test-input {1 [["R2, L3" 5] ["R2, R2, R2" 2] ["R5, L5, R5, R3" 12]]
                    2 [["R8, R4, R4, R8" 4]] }]
    (for [[p its] test-input] (every? true? (for [[it exp] its] (= exp (solve (parse it) p))))))
  )
