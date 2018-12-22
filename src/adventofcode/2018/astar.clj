(ns adventofcode.2018.astar
  (:require [adventofcode.2018.util :refer [remove-empty]]
            ))

(defn add-move [state next-moves move-cost min-remaining-cost move cost]
  (-> state
      (update :route-costs #(assoc % move cost))
      (update :moves (fn [moves]
                       (->> move
                            (next-moves)
                            (remove #(contains? (:route-costs state) %))
                            (reduce (fn [moves new-move]
                                      (let [new-cost (+ cost (move-cost move new-move))]
                                        (update moves
                                              (+ new-cost (min-remaining-cost new-move))
                                              #(conj (or % ()) [new-cost new-move])
                                              )))
                                    moves
                                    )
                            )))
      ))

(defn get-cost [state move]
  (get-in state [:route-costs move]))

(defn next-move [state]
  (let [est-cost (apply min (keys (:moves state)))
        [min-cost move] (peek (get-in state [:moves est-cost]))
        ]
    [move min-cost est-cost]
    ))

(defn start [initial-move]
  {:route-costs {}
   :moves {0 (list [0 initial-move])}
   })

(defn step [state next-moves move-cost min-remaining-cost]
  (let [[move cost est-cost] (next-move state)]
    (cond-> state
      true (update-in [:moves est-cost] pop)

      (not (and (contains? (:route-costs state) move)
                (<= ((:route-costs state) move) cost)
                ))
      (add-move next-moves move-cost min-remaining-cost move cost)

      true (update :moves remove-empty)
      )))
