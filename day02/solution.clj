(ns day02.solution
  (:require [input :refer [f->str]]))

(def KP1 [[-1 -1] [0 -1] [1 -1]
          [-1  0] [0  0] [1  0]
          [-1  1] [0  1] [1  1]])

(def KP2 [#_____________ [0 -2]
          #_____ [-1 -1] [0 -1] [1 -1]
          [-2 0] [-1  0] [0  0] [1  0] [2 0]
          #_____ [-1  1] [0  1] [1  1]
          #_____________ [0  2]])

(defn parse [it]
  (->> it
       (map {\U [0 -1] \D [0 1] \L [-1 0] \R [1 0] \newline nil})
       (partition-by nil?)
       (take-nth 2)))

(defn solve [it kp fmt]
  (->> (reduce
        (fn [[bts pos] dirs]
          (let [p (reduce #(if-let [np (some #{(mapv + %1 %2)} kp)] np %1) pos dirs)
                b (inc (.indexOf kp p))]
            [(conj bts b) p]))
        [[] (kp (dec 5))] it)
       first (map #(format fmt %)) (apply str)))

(defn -main [day]
  (let [input (->> day f->str parse)]
    {:part1 (solve input KP1 "%s")
     :part2 (solve input KP2 "%H")}))


(comment
  (let [test-input "ULL
RRDDD
LURDL
UUUUD"
        input (parse test-input)]
    {:1 (= "1985" (solve input KP1 "%s"))
     :2 (= "5DB3" (solve input KP2 "%H"))})
  )
