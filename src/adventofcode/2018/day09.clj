(ns adventofcode.2018.day09
  (:require clojure.string))

(defn parse-input [lines]
  (let [[_ num-players last-marble] (re-matches #"(\d+) players.*worth (\d+) points" (first lines))]
    {:players (read-string num-players)
     :last-marble (read-string last-marble)
     }))

(defn start [state]
  (assoc state
         :circle (list 0)
         :current-marble 0
         :next-marble 1
         :player 0
         :scores (->> state
                      (:players)
                      (range)
                      (map #(vector % 0))
                      (into {})
                      )
         ))

(defn place [state]
  (let [place-index (-> state
                        :current-marble
                        (+ 2)
                        (mod (count (:circle state))))
        place-index (if (= 0 place-index) (count (:circle state)) place-index)
        ]
    (-> state
        (update :circle (fn [circle]
                          (let [[front back] (split-at place-index circle)]
                            (concat front [(:next-marble state)] back)
                            )))
        (assoc :current-marble place-index)
        )))

(defn score [state]
  (let [point-index (-> state
                        (:current-marble)
                        (- 7)
                        (mod (count (:circle state))))
        [front [point & rest]] (split-at point-index (:circle state))
        ]
    (-> state
        (update-in [:scores (:player state)] #(+ % point (:next-marble state)))
        (assoc :circle (concat front rest))
        (assoc :current-marble point-index)
        )))

(defn step [state]
  (let [score-turn (= 0 (mod (:next-marble state) 23))]
    (cond-> state
      score-turn score
      (not score-turn) place
      true (update :next-marble inc)
      true (update :player #(mod (inc %) (:players state)))
      )))

(defn solve-a [lines]
  (->> lines
       (parse-input)
       (start)
       (iterate step)
       (filter (fn [state]
                 (println (:next-marble state))
                 (> (:next-marble state) (:last-marble state))))
       (first)
       (:scores)
       (vals)
       (apply max)
       ))

(defn solve-b [lines]
  ())

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   }
  )

(defn day-lines [] (adventofcode.2018.core/day-lines 9))
