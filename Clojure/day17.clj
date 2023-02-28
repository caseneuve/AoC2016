(ns day17
  (:require [input :refer [f->str]]
            [clj-commons.digest :refer [md5]]))

(defn open-doors [code path]
  (->> (str code path) md5 (take 4) (map int) (map vector "UDLR")
       (reduce (fn [acc [d n]] (cond-> acc (<= (int \b) n (int \f)) (conj d))) [])))

(defn next-steps [code pos path]
  (let [dirs (open-doors code path)]
    (for [d dirs
          :let [[x y] (map + pos ({\U [0 -1] \D [0 1] \L [-1 0] \R [1 0]} d))]
          :when (and (<= 0 x 3) (<= 0 y 3))]
      [[x y] (str path d)])))

(defn find-path [code part]
  (loop [q (into clojure.lang.PersistentQueue/EMPTY (next-steps code [0 0] "")), lp 0]
    (let [[xy p] (peek q), s (next-steps code xy p)]
      (cond
        (and (empty? (pop q)) (empty? s) (> lp 0)) lp
        (= xy [3 3]) (case part 1 p, (recur (pop q) (max lp (count p))))
        :else (recur (into (pop q) s) lp)))))

(defn -main [day]
  (let [input (->> day f->str)]
    {:part1 (find-path input 1)
     :part2 (find-path input 2)}))


(comment
  (let [test-input {1 [["ihgpwlah" "DDRRRD"] ["kglvqrro" "DDUDRLRRUDRD"] ["ulqzkmiv" "DRURDRUDDLLDLUURRDULRLDUUDDDRR"]]
                    2 [["ihgpwlah" 370] ["kglvqrro" 492] ["ulqzkmiv" 830]]}]
    (for [[p its] test-input] (every? true? (for [[i e] its] (= e (find-path i p))))))
  )
