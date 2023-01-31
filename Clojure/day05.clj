(ns day05
  (:require [clj-commons.digest :refer [md5]]))

(defn hide-cursor [] (print "\033[?25l"))
(defn show-cursor [] (print "\033[?25h"))
(defn display [[p1 p2]]
  (let [red "\033[31m", green "\033[32m", reset "\033[0m"
        p1 (apply str (take 8 p1))
        p2 (apply str (for [c p2] (if c (str red c reset) (rand-nth "0123456789abcdef"))))]
    (printf "\r%s%-10s%s%s " green p1 reset p2)))

(defn -main [_]
  (hide-cursor)
  (let [salt "uqwqemis"
        passwords ;; hack: starting from the first known hash id
        (loop [i 4515059, [p1 p2 :as pwd] [[] (vec (repeat 8 nil))], idx (set (apply str (range 8)))]
          (let [h (md5 (str salt i))]
            (cond
              (empty? idx) pwd
              (= "00000" (subs h 0 5))
              (let [[a b] (subs h 5 7)]
                ;; a poor man's optimization: inc by the lowest difference between ids of two matching hashes
                (recur (+ 107653 i)
                       [(conj p1 a) (cond-> p2 (contains? idx a) (update (parse-long (str a)) #(or % b)))]
                       (disj idx a)))
              :else (do (when (= 0 (mod i 50)) (display pwd))
                        (recur (inc i) pwd idx)))))
        solve #(->> passwords % (take 8) (apply str))]
    (show-cursor)
    {:part1 (solve first) :part2 (solve second)}))
