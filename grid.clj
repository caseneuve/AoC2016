(ns grid)

(defn manhattan [[ax ay] [bx by]] (+ (abs (- ax bx)) (abs (- ay by))))

(defn v+ [v1 v2] (map #(apply + %) (mapv vector v1 v2)))
