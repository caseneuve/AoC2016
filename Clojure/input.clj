(ns input
  (:require [clojure.string :refer [split-lines trim]]))

(defn f->str [dir] (->> "input.txt" (str dir "/") slurp trim))
(defn f->lines [dir] (->> dir f->str split-lines))
(defn lines [s] (split-lines s))
(defn nums [s] (->> s (re-seq #"-?\d+") (mapv parse-long)))
(defn f->nums [dir] (->> dir f->str nums))
(defn words [s] (re-seq #"\w+" s))
