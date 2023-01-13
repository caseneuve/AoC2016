(ns day08.solution
  (:require [input :refer [f->lines lines]]))

(defn parse [it]
  (->> it
       (reduce
        (fn [i s]
          (let [d (mapv parse-long (re-seq #"\d+" s))]
            (conj i [(re-find #"row|col|rec" s) d]))) [])))

(defn solve [it X Y]
  (let [g (vec (for [_ (range Y)] (vec (repeat X 0))))
        tr #(apply mapv vector %)
        shift #(->> %1 cycle (drop (- %3 %2)) (take %3) vec)
        put (fn [g [x y]] (reduce #(assoc-in %1 %2 1) g (for [x* (range x) y* (range y)] [y* x*])))]
    (reduce
     (fn [g [i [idx by :as rect]]]
       (case i "row" (update g idx shift by X), "col" (tr (update (tr g) idx shift by Y)), (put g rect)))
     g it)))

(defn display [g] (let [g (map #(map {1 "█" 0 " "} %) g)] (doseq [r g] (prn (apply str r)))) g)

(defn -main [day]
  (let [input (->> day f->lines parse), g (display (solve input 50 6))]
    {:part1 (->> g flatten (apply +)) :part2 "ZFHFSFOGPO"}))


(comment
  (let [test-input (lines "rect 3x2
rotate column x=1 by 1
rotate row y=0 by 4
rotate column x=1 by 1")
        input (parse test-input)]
    (= [[0 1 0 0 1 0 1] [1 0 1 0 0 0 0] [0 1 0 0 0 0 0]]
       (display (solve input 7 3))))
  )
