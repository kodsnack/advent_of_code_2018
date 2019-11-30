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

(defn start-timed [num-workers dependencies]
  {:time 0
   :completed []
   :remaining (->> dependencies
                   (vals)
                   (apply concat)
                   (reduce (fn [deps task]
                             (update deps task #(or % #{})))
                           dependencies
                           )
                   )
   :workers (reduce (fn [workers worker]
                      (assoc workers worker [nil 0]))
                    {}
                    (range 0 num-workers)
                    )
   })

(defn task-time [task]
  (+ 60 (- (int task) 64)))

(defn assign-work [state]
  (let [available-tasks (->> state
                             (:remaining)
                             (filter (fn [[task prereqs]]
                                       (empty? prereqs)))
                             (map first)
                             (sort)
                             )
        available-workers (->> state
                               (:workers)
                               (filter (fn [[_ [task _]]]
                                         (nil? task)))
                               (map first)
                               )
        assignments (map vector available-workers available-tasks)
        ]
    (reduce (fn [state' [worker task]]
              (-> state'
                  (assoc-in [:workers worker] [task (task-time task)])
                  (update :remaining #(dissoc % task))
                  ))
            state
            assignments
            )))

(defn perform-work [state]
  (->> (keys (:workers state))
       (reduce (fn [state' worker]
                 (update-in state' [:workers worker 1] dec))
               state
               )
       ))

(defn finish-work [state]
  (->> state
       (:workers)
       (filter (fn [[worker [task time-left]]]
                 (= 0 time-left)
                 ))
       (reduce (fn [state' [worker [task _]]]
                 (-> state'
                     (update :completed #(conj % task))
                     (assoc-in [:workers worker] [nil 0])
                     (update :remaining (fn [remaining]
                                          (->> remaining
                                               (map (fn [[t prereqs]]
                                                      [t (disj prereqs task)]))
                                               (into {})
                                               )))
                     ))
               state
               )
       )
  )

(defn timestep [state]
    (-> state
         (assign-work)
         (perform-work)
         (finish-work)
         (update :time inc)
         ))

(defn finished? [state]
  (and (empty? (:remaining state))
       (every? (fn [[worker [task time-left]]] (nil? task)) (:workers state))
       ))

(defn solve [num-workers lines]
  (->> lines
       (parse-dependencies)
       (start-timed num-workers)
       (iterate (partial timestep))
       (filter finished?)
       (first)
       ))

(defn solve-a [lines]
  (->> lines
       (solve 1)
       (:completed)
       (apply str)
       ))

(defn solve-b [lines]
  (->> lines
       (solve 5)
       (:time)
       ))

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   })

(defn day-lines [] (adventofcode.2018.core/day-lines 7))
