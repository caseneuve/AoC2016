(ns grid)

(defn manhattan [[ax ay] [bx by]] (+ (abs (- ax bx)) (abs (- ay by))))
