(ns adventofcode.2018.unused-util
  (:gen-class)
  (:require [clojure.string])
)

(defn rotate [lookahead xs]
  (let [[front back] (split-at lookahead xs)]
    (concat back front)
))

(defn get-digits [lines]
  (let [
        digits (mapcat clojure.string/trim lines)
        numbers (map #(read-string (str %)) digits)
        ]
    numbers
))

(defn transpose [& colls]
  (apply map vector colls)
  )

(defn to-map [pairs]
  (->> pairs
       (apply transpose)
       (apply zipmap)
       ))
