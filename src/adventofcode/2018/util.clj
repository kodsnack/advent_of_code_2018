(ns adventofcode.2018.util
  (:require [clojure.string])
)

(defn cartprod [xs ys]
  (for [x xs
        y ys]
    [x y]
    ))

(defn grid [minx-in maxx-ex miny-in maxy-ex]
  (cartprod
   (range minx-in maxx-ex)
   (range miny-in maxy-ex)
   ))
