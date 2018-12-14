(ns adventofcode.2018.day14
  (:require clojure.string))

(defn new-recipes [score1 score2]
  (let [scoresum (+ score1 score2)
        str-digits (str scoresum)
        digits (mapv (comp read-string str) str-digits)
        ]
    digits
    ))

(defn expand-scoreboard [{scoreboard :board elfi1 :elfi1 elfi2 :elfi2}]
  (let [new (new-recipes (scoreboard elfi1) (scoreboard elfi2))]
    {:board (apply conj scoreboard new)
     :elfi1 elfi1
     :elfi2 elfi2
     }
    ))

(defn move-elves [{scoreboard :board elfi1 :elfi1 elfi2 :elfi2}]
  (let [new-elfi1 (mod (+ elfi1 (inc (scoreboard elfi1))) (count scoreboard))
        new-elfi2 (mod (+ elfi2 (inc (scoreboard elfi2))) (count scoreboard))
        ]
    {:board scoreboard
     :elfi1 new-elfi1
     :elfi2 new-elfi2
     }
  ))

(defn step [state]
  (move-elves (expand-scoreboard state))
  )

(defn solve-a [num-steps]
  (->> {:board [3 7] :elfi1 0 :elfi2 1}
       (iterate step)
       (drop-while (fn [{scoreboard :board}]
                     (< (count scoreboard) (+ 10 num-steps))))
       (first)
       (:board)
       (drop num-steps)
       (take 10)
       (clojure.string/join)
   ))


(defn run [input-lines & args]
  (let [num-steps (read-string (first input-lines))]
    {:A (solve-a input-lines)
     :B (solve-b input-lines)
     }))

(defn day-lines [] (adventofcode.2018.core/day-lines 13))
(def lines (day-lines))
(def tick 0)
(defn show-tick [tick]
  (as-> lines $
    (parse-state $)
    (iterate update-state-remove-crashes $)
    (nth $ tick)
    (print-state $)
    ))
(defn n [] (def tick (inc tick)) (show-tick tick))
(defn p [] (def tick (dec tick)) (show-tick tick))
