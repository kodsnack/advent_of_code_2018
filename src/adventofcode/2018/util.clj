(ns adventofcode.2018.util
  (:require [clojure.string])
)

(defn abs [x]
  (if (< x 0)
    (- x)
    x
    ))

(defmacro as->> [alias & forms]
  `(as-> ~(last forms) ~alias ~@(butlast forms)))

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

(defn remove-empty [m]
  (reduce dissoc m (filter #(empty? (m %)) (keys m))))

(defn transpose [& colls]
  (apply map vector colls)
  )

(defn vec-add [& vectors]
  (apply mapv + vectors))
(defn vec-sub [& vectors]
  (apply mapv - vectors))
