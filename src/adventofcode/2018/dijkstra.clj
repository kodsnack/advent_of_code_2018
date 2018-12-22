(ns adventofcode.2018.dijkstra
  (:require [adventofcode.2018.util :refer [remove-empty]]
            ))

(defn add-move [state next-moves move-cost move cost]
  (-> state
      (update :route-costs #(assoc % move cost))
      (update :moves (fn [moves]
                       (->> move
                            (next-moves)
                            (remove #(contains? (:route-costs state) %))
                            (reduce (fn [moves new-move]
                                      (update moves
                                              (+ cost (move-cost move new-move))
                                              #(conj (or % (sorted-set)) new-move)
                                              ))
                                    moves
                                    )
                            )))
      ))

(defn get-cost [state move]
  (get-in state [:route-costs move]))

(defn start [initial-move]
  {:route-costs {}
   :moves {0 (sorted-set initial-move)}
   })

(defn step [state next-moves move-cost cost move]
  (cond-> state
    true (update-in [:moves cost] #(disj % move))

    (not (and (contains? (:route-costs state) move)
              (<= ((:route-costs state) move) cost)
              ))
    (add-move next-moves move-cost move cost)

    true (update :moves remove-empty)
    ))
