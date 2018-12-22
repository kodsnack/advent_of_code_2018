(ns adventofcode.2018.astar
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
                                              #(conj (or % ()) new-move)
                                              ))
                                    moves
                                    )
                            )))
      ))

(defn get-cost [state move]
  (get-in state [:route-costs move]))

(defn next-move [state]
  (let [min-cost (apply min (keys (:moves state)))
        move (peek (get-in state [:moves min-cost]))
        ]
    [move min-cost]
    ))

(defn start [initial-move]
  {:route-costs {}
   :moves {0 (list initial-move)}
   })

(defn step [state next-moves move-cost]
  (let [[move cost] (next-move state)]
    (cond-> state
      true (update-in [:moves cost] pop)

      (not (and (contains? (:route-costs state) move)
                (<= ((:route-costs state) move) cost)
                ))
      (add-move next-moves move-cost move cost)

      true (update :moves remove-empty)
      )))
