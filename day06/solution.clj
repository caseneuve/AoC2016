(ns day06.solution
  (:require [input :refer [f->lines lines]]))

(def r->c #(apply mapv vector %))
(def common #(->> % frequencies (sort-by val) keys ((juxt first last))))
(defn solve [it] (->> it r->c (map common) r->c (map #(apply str %))))

(defn -main [day] (->> day f->lines solve (zipmap [:part2 :part1])))


(comment
  (let [test-input "eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar"]
    (->> test-input lines solve (= '("advent" "easter"))))
  )
