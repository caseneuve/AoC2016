(ns day14
  (:require [clj-commons.digest :refer [md5]]))

(defn hashes [it n] (pmap #(nth (iterate md5 (str it %)) n) (range)))

(defn same-chars [n h]
  (reduce (fn [a e] (if (apply = e) (reduced (first e)) a)) nil (partition n 1 h)))

(defn key64 [it n]
  (loop [i 0, [h & hr] (hashes it n), hm {}, hs []]
    (cond
      (> (count hs) 63) (nth (sort hs) 63)
      :else (let [k3 (same-chars 3 h), k5 (when k3 (same-chars 5 h))
                  idx (when-let [c (hm k5)] (filter #(<= i (+ 1000 %)) c))]
        ;; (when (= (mod i 1000) 0) (prn (/ i 1000) (count hs)))
        (recur (inc i) hr (cond-> hm k3 (update k3 conj i)) (if (seq idx) (into hs idx) hs))))))

;; part 1 takes about half a sec, but part 2 runs for over a minute -- weak `md5` performance

(defn -main [_]
  (let [it "ihaygndm", solve (partial key64 it)]
    {:part1 (solve 1) :part2 (solve 2017)}))


(comment
  (= 22728 (key64 "abc" 1))             ; not testing part 2 here, takes too long
  )
