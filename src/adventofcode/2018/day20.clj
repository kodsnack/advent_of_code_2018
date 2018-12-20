(ns adventofcode.2018.day20
  (:require clojure.string))

(def example-input "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")

(defn step [{:keys [distmap pos pos-stack] [next & regex] :regex :as state}]
  (case next
    \^ (assoc state :regex regex)
    \$ (assoc state :regex regex)
    \( (-> state
           (update :pos-stack #(conj % pos))
           (assoc :regex regex)
           )
    \| (-> state
           (assoc :pos (first pos-stack))
           (assoc :regex regex)
           )
    \) (-> state
           (assoc :pos (first pos-stack))
           (update :pos-stack pop)
           (assoc :regex regex)
           )
    (let [[y x :as prev] pos
          step (case next
                 \N [(dec y) x]
                 \S [(inc y) x]
                 \W [y (dec x)]
                 \E [y (inc x)]
                 )
          distmap' (update distmap step #(or % (inc (distmap prev))))
          ]
      {:distmap distmap'
       :pos step
       :pos-stack pos-stack
       :regex regex
       })
    ))

(defn finished? [state] (empty? (:regex state)))

(defn start [regex]
  {:distmap {[0 0] 0}
   :pos [0 0]
   :pos-stack ()
   :regex regex
   })

(defn traverse [state]
  (->> state
       (iterate step)
       (filter finished?)
       (first)
       ))

(defn max-dist [state]
  (->> state
       (:distmap)
       (vals)
       (apply max)
       ))

(defn format-state [state] (str state))

(defn solve-a [lines]
  (->> lines
       (first)
       (start)
       (traverse)
       (max-dist)
       ))

(defn solve-b [result]
  ())

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   })

(defn day-lines [] (adventofcode.2018.core/day-lines 20))
(def states [])
(defn show-state [] (println (format-state (last states))))
(defn start-day-lines [] (def states [(start (first (day-lines)))]) (show-state))
(defn start-example [] (def states [(start example-input)]) (show-state))
(defn n []
  (def states (conj states (step (last states))))
  (show-state))
(defn p []
  (def states (pop states))
  (show-state))
