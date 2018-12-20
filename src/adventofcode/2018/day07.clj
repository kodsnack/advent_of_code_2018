(ns adventofcode.2018.day07
  (:require clojure.string))

(def example-input "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")

(defn parse-dependencies [lines]
  (reduce (fn [deps line]
            (let [[_ [dependency] [dependent]] (re-matches #".*Step ([A-Z]) .* step ([A-Z]).*" line)]
              (update deps dependent #(conj (or % #{}) dependency))
              ))
          {}
          lines
          ))

(defn start [dependencies]
  {:completed []
   :remaining (->> dependencies
                   (vals)
                   (apply concat)
                   (reduce (fn [deps task]
                             (update deps task #(or % #{})))
                           dependencies
                           )
                   )
   })

(defn step [state]
  (let [next-task (->> state
                       (:remaining)
                       (filter (fn [[task prereqs]] (empty? prereqs)))
                       (map first)
                       (sort)
                       (first)
                       )
        ]
    (-> state
        (update :completed #(conj % next-task))
        (update :remaining #(dissoc % next-task))
        (update :remaining (fn [remaining]
                             (->> remaining
                                  (map (fn [[task prereqs]]
                                         [task (disj prereqs next-task)]))
                                  (into {})
                                  )))
        )))

(defn finished? [state] (empty? (:remaining state)))

(defn solve-a [lines]
  (->> lines
       (parse-dependencies)
       (start)
       (iterate step)
       (filter finished?)
       (first)
       (:completed)
       (apply str)
       ))

(defn solve-b [lines]
  (
       ))

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   })

(defn day-lines [] (adventofcode.2018.core/day-lines 7))
