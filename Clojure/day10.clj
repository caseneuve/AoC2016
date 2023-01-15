(ns day10
  (:require [input :refer [f->lines lines]]))

(defn parse [it]
  (->> it (map #(re-seq #"\w+" %))
       (reduce
        (fn [[m ii] [a b _ _ e f g _ _ _ k l]]
          (if (= a "value")
            [(update m (str e f) conj (parse-long b)) ii]
            [m (conj ii [(str a b) (str f g) (str k l)])])) [{} []])))

(defn inspect [chips instructions part]
    (reduce
     (fn [m [bot low high]]
       (let [[lo hi :as v] (sort (m bot)), out (vals (select-keys m ["output0" "output1" "output2"]))]
         (cond
           (and (= part 1) (= v (m "comp")))  (reduced bot)
           (and (= part 2) (= (count out) 3)) (reduced (->> out flatten (apply *)))
           (not= (count v) 2) m
           :else (-> m (dissoc bot) (update low conj lo) (update high conj hi)))))
     chips (cycle instructions)))

(defn -main [day]
  (let [[ch ii] (->> day f->lines parse), solve (partial inspect (assoc ch "comp" [17 61]) ii)]
    {:part1 (solve 1) :part2 (solve 2)}))


(comment
  (let [test-input "value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2"
        [ch ii] (->> test-input lines parse), solve (partial inspect (assoc ch "comp" [2 5]) ii)]
    {:1 (= "bot2" (solve 1)) :2 (= (* 5 2 3) (solve 2))}
    )
  )
