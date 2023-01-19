(ns day12
  (:require [input :refer [f->lines lines]]))

(defn execute [it regs]
  (let [len (count it), f parse-long]
    (loop [regs regs, i 0]
      (if (>= i len) (regs "a")
          (let [[op x y] (it i)]
            (case op
              "cpy" (recur (assoc regs y (or (f x) (regs x 0))) (inc i))
              "inc" (recur (update regs x inc) (inc i))
              "dec" (recur (update regs x dec) (inc i))
              (recur regs (if (not= 0 (or (f x) (regs x 0))) (+ i (f y)) (inc i)))))))))

;; part 2 will run for more than a minute, not proud, but can live with that for now...
(defn -main [day]
  (let [instructions (->> day f->lines (mapv #(re-seq #"-?\w+" %)))
        solve (partial execute instructions)]
    {:part1 (solve {}) :part2 (solve {"c" 1})}))


(comment
  (let [test-input (->> "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"
                        lines (mapv #(re-seq #"-?\w+" %)))]
    (= 42 (execute test-input {})))
  )
