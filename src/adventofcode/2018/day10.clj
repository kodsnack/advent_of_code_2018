(ns adventofcode.2018.day10
  (:require clojure.string))

(def example-input "position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>")

(defn parse-input [lines]
  (map (fn [line]
         (->> line
              (re-matches #".*?(-?\d+).*?(-?\d+).*?(-?\d+).*?(-?\d+).*?")
              (rest)
              (map read-string)
              (split-at 2)
              (map #(apply vector %))
              (apply vector)
              ))
       lines
       ))

(defn step [points]
  (map (fn [[[x y] [vx vy]]]
         [[(+ x vx) (+ y vy)] [vx vy]]
         )
       points
       ))

(defn minmax [points]
  (let [pointset (->> points
                      (map first)
                      (set)
                      )]
    [(apply min (map first pointset))
     (apply min (map second pointset))
     (apply max (map first pointset))
     (apply max (map second pointset))
     pointset
     ]))

(defn format-state [points]
  (let [[minx miny maxx maxy pointset] (minmax points)]
    (->> (range miny (inc maxy))
         (map (fn [y]
                (->> (range minx (inc maxx))
                     (map (fn [x]
                            (if (pointset [x y]) \# \.)
                            ))
                     )))
         (map clojure.string/join)
         (clojure.string/join \newline)
         )))

(defn area [points]
  (let [[minx miny maxx maxy] (minmax points)]
    (* (- maxx minx) (- maxy miny))
    ))

(defn solve-a [lines]
  (->> lines
       (parse-input)
       (iterate step)
       (partition 2 1)
       (some (fn [[st1 st2]]
                 (if (< (area st1) (area st2))
                   st1
                   nil
                   )))
       (format-state)
       ))

(defn solve-b [lines]
  ())

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   }
  )

(defn run-print []
  (println (solve-a (day-lines)))
  )

(defn day-lines [] (adventofcode.2018.core/day-lines 10))
