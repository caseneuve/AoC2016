(ns day04.solution
  (:require [input :refer [f->str]]))

(defn parse [it] (re-seq #"((\D+)(\d+))\[(\w+)\]\n?" it))

(defn most-common-5 [it]
  (->> it
       (re-seq #"[a-z]")
       frequencies (group-by val)
       sort reverse vals
       (map (comp sort #(map first %)))
       flatten (take 5) (apply str)))

(defn decrypt [it rotations]
  (->> it (map (comp char #(+ % 96) #(mod % 26) #(+ % rotations -96) int)) (apply str)))

(defn solve [it]
  (->> it
       (reduce
        (fn [acc [_ _ nm id ch]]
          (let [id (parse-long id), real? (= ch (most-common-5 nm))]
            (cond-> acc
              real? (update :part1 + id)
              (and real? (re-find #"northpole" (decrypt nm id))) (assoc :part2 id))))
        {:part1 0})))

(defn -main [day] (->> day f->str parse solve))


(comment
  (let [test-input (parse "aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]")]
    {:1 (= 1514 ((solve test-input) :part1))
     :2 (re-find #"very.encrypted.name." (decrypt "qzmt-zixmtkozy-ivhz-" 343))})
  )
