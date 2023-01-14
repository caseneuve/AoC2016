(ns day09
  (:require [input :refer [f->str]]))

(defn decompress [it p]
  (loop [[c _ :as it] it, len 0]
    (cond (nil? c) len
          (= \( c)
          (let [[mrk rst] (split-with #(not= \) %) it)
                [idx rep] (map parse-long (re-seq #"\d+" (apply str mrk)))
                [dat  it] (split-at idx (next rst))]
            (recur it (+ len (* rep (case p 1 (count dat), (decompress dat p))))))
          :else (recur (next it) (inc len)))))

(defn -main [day]
  (let [solve (partial decompress (f->str day))]
    {:part1 (solve 1) :part2 (solve 2)}))


(comment
  (let [test-input {1 [["ADVENT" (count "ADVENT")] ["A(1x5)BC" (count "ABBBBBC")]
                       ["(3x3)XYZ" (count "XYZXYZXYZ")] ["(6x1)(1x3)A" (count "(1x3)A")]
                       ["X(8x2)(3x3)ABCY" (count "X(3x3)ABC(3x3)ABCY")]]
                    2 [["(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" 445]
                       ["(27x12)(20x12)(13x14)(7x10)(1x12)A" 241920] ["(3x3)XYZ" (count "XYZXYZXYZ")]
                       ["X(8x2)(3x3)ABCY" (count "XABCABCABCABCABCABCY")]]}]
    (for [[p its] test-input] (every? true? (for [[i e] its] (= e (decompress i p))))))
  )
