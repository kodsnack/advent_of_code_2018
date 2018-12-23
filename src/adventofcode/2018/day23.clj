(ns adventofcode.2018.day23
  (:require clojure.string
            [adventofcode.2018.util :refer [abs as->> vec-sub]]
            ))

(defn parse-input [lines]
  (map (fn [line]
         (->> line
              (re-matches #".*?(-?\d+).*?(-?\d+).*?(-?\d+).*?(-?\d+).*?")
              (rest)
              (map read-string)
              (as->> [x y z r] {:pos [x y z] :r r})
              )
         )
       lines
       ))

(defn dist [p1 p2]
  (->> (vec-sub p1 p2)
       (map abs)
       (apply +)
       ))

(defn solve-a [lines]
  (let [bots (parse-input lines)
        {maxpos :pos, maxr :r} (apply max-key :r bots)
        ]
    (->> bots
         (filter (fn [bot]
                   (<= (dist (:pos bot) maxpos) maxr)))
         (count)
         )
    ))

(defn solve-b [lines]
  ())

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   }
  )

(defn day-lines [] (adventofcode.2018.core/day-lines 23))
