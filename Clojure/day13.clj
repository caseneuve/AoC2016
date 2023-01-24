(ns day13
  (:require [input :refer [f->str]]))

(defn open? [it [x y]]
  (let [bin #(Long/toString % 2)
        s (+ (* x x) (* 3 x) (* 2 x y) y (* y y) it)]
    (even? ((frequencies (bin s)) \1))))

(defn steps [q p s it]
  (let [adj (map #(mapv + p %) [[0 1] [1 0] [-1 0] [0 -1]])]
    (into q (map #(vector % (inc s)) (filter (partial open? it) adj)))))

(defn bfs [it {:keys [p1 p2]}]
  (loop [q (steps clojure.lang.PersistentQueue/EMPTY [1 1] 0 it) , vi #{}]
    (let [[[x y :as p] s] (peek q)]
      (cond
        (empty? q) (count vi)
        (= p p1) s
        (or (contains? vi p) (< x 0) (< y 0) (> s (or p2 it))) (recur (pop q) vi)
        :else (recur (steps (pop q) p s it) (conj vi p))))))

(defn -main [day]
  (let [input (->> day f->str parse-long), solve (partial bfs input)]
    {:part1 (solve {:p1 [31 39]}) :part2 (solve {:p2 50})}))


(comment
    (= 11 (bfs 10 {:p1 [7 4]}))
  )
