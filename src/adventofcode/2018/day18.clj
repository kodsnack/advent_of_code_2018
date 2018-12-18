(ns adventofcode.2018.day18
  (:require clojure.string
            [adventofcode.2018.util :refer [grid transpose]]
            ))

(def example-input ".#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.")

(defn parse-state [lines]
  (mapv #(apply vector %) lines))

(defn format-state [state]
  (->> state
       (map clojure.string/join)
       (clojure.string/join \newline)
       ))

(defn adjacent [state [y x :as pos]]
  (->> (grid (dec y) (+ 2 y) (dec x) (+ 2 x))
       (filter #(not= pos %))
       (filter (fn [[y _]] (<= 0 y)))
       (filter (fn [[y _]] (< y (count state))))
       (filter (fn [[_ x]] (<= 0 x)))
       (filter (fn [[_ x]] (< x (count (first state)))))
       ))

(defn count-adjacent [state type pos]
  (->> pos
       (adjacent state)
       (filter #(= type (get-in state %)))
       (count)
       ))

(defn update-cell [state pos]
  (case (get-in state pos)
        \. (if (<= 3 (count-adjacent state \| pos)) \| \.)
        \| (if (<= 3 (count-adjacent state \# pos)) \# \|)
        \# (if (and (<= 1 (count-adjacent state \# pos))
                    (<= 1 (count-adjacent state \| pos))
                    )
             \#
             \.
             )
        ))

(defn step [state]
  (->> (grid 0 (count state) 0 (count (first state)))
       (reduce (fn [state' pos]
                 (assoc-in state' pos (update-cell state pos)))
               state
               )))

(defn resource-value [state]
  (->> state
       (apply concat)
       (map (fn [c] [(= c \|) (= c \#)]))
       (reduce (fn [[treesum lumbsum] [tree lumb]] [(+ treesum (if tree 1 0)) (+ lumbsum (if lumb 1 0))])
               [0 0]
               )
       (apply *)
       ))

(defn solve-a [lines]
  (->> lines
       (parse-state)
       (iterate step)
       (drop 10)
       (first)
       (resource-value)
       ))

(defn solve-b [lines] ())

(defn run [input-lines & args]
  { :A (solve-a input-lines)
    :B (solve-b input-lines)
    }
)


(defn day-lines [] (adventofcode.2018.core/day-lines 18))
(def states [])
(defn show-state [] (println (str \newline (format-state (last states)))))
(defn start-day-lines [] (def states [(parse-state (day-lines))]) (show-state))
(defn start-example [] (def states [(parse-state (clojure.string/split-lines example-input))]) (show-state))
(defn n []
  (def states (conj states (step (last states))))
  (show-state))
(defn p []
  (def states (pop states))
  (show-state))
(defn animate
  ([] (animate 30))
  ([dt]
   (while true
     (n)
     (Thread/sleep dt)
     )))
