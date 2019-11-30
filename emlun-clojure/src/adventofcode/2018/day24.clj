(ns adventofcode.2018.day24
  (:require clojure.string
            [adventofcode.2018.util :refer [as->>]]
            ))

(defn parse-specials [s]
  (->> s
      (as->> $ (or $ ""))
      (clojure.string/trim)
      (as->> $ (clojure.string/split $ #"\s*,\s*"))
      (remove empty?)
      (map keyword)
      (set)
      ))

(defn parse-special [special]
  (if (nil? special)
    {:immunities #{} :weaknesses #{}}
    (let [[_ immunities] (re-matches #".*?immune to ([^;)]+).*?" special)
          [_ weaknesses] (re-matches #".*?weak to ([^;)]+).*?" special)]
      {:immunities (parse-specials immunities)
       :weaknesses (parse-specials weaknesses)
       }
      )))

(defn parse-army [team lines]
  (->> lines
       (mapv (fn [line]
               (let [[_ qty hp special dmg dtype init] (re-matches #"(\d+) units each with (\d+) hit points (\(.*\) )?with an attack that does (\d+) (\S+) damage at initiative (\d+)" line)]
                 {:qty (read-string qty)
                  :hp (read-string hp)
                  :special (parse-special special)
                  :dmg (read-string dmg)
                  :dtype (keyword dtype)
                  :init (read-string init)
                  :team team
                  })))
       ))

(defn parse-armies [[immune-lines infection-lines]]
  {:units (->> [(parse-army :immune immune-lines)
                (parse-army :infect infection-lines)
                ]
               (apply concat)
               (map-indexed (fn [id unit] [id (assoc unit :id id)]))
               (into {})
               )
   })

(defn parse-input [lines]
  (->> lines
       (map clojure.string/trim)
       (remove empty?)
       (remove #(= % "Immune System:"))
       (partition-by #(= % "Infection:"))
       (remove #(= % ["Infection:"]))
       (parse-armies)
       ))

(defn effective-power [unit]
  (* (:qty unit) (:dmg unit)))

(defn compute-damage [unit target]
  (cond
    ((:immunities (:special target)) (:dtype unit))
    0

    ((:weaknesses (:special target)) (:dtype unit))
    (* 2 (effective-power unit))

    :else
    (effective-power unit)
    ))

(defn choose-target [state targets unit]
  (->> state
       (:units)
       (vals)
       (remove #(= (:team %) (:team unit)))
       (remove (set (vals targets)))
       (remove (fn [target] (>= 0 (compute-damage unit target))))
       (sort-by (fn [target]
                  [(- (compute-damage unit target))
                   (- (effective-power target))
                   (- (:init target))
                   ]))
       (first)
       ))

(defn targeting-phase [state]
  (->> state
       (:units)
       (vals)
       (sort-by (fn [unit] [(- (effective-power unit)) (- (:init unit))]))
       (reduce (fn [targets unit]
                 (assoc targets unit (choose-target state targets unit)))
               {}
               )
       (assoc state :targets)
       ))

(defn attack-phase [state]
  (->> state
       (:targets)
       (remove (comp nil? second))
       (sort-by (comp - :init first))
       (reduce (fn [units [unit target]]
                 (if (< 0 (get-in units [(:id unit) :qty]))
                   (update-in units
                              [(:id target) :qty]
                              (fn [qty]
                                (- qty (int (/ (compute-damage (units (:id unit)) (units (:id target)))
                                               (:hp target))))
                                ))
                   units
                   ))
               (:units state)
               )
       (assoc state :units)
       ))

(defn cleanup-phase [state]
  (-> state
      (update :units (fn [units]
                       (reduce (fn [units [id unit]]
                                 (if (< 0 (:qty unit))
                                   units
                                   (dissoc units id)
                                   ))
                               units
                               units
                               )))
      (dissoc :targets)
      ))

(defn step [state]
  (->> state
       (targeting-phase)
       (attack-phase)
       (cleanup-phase)
       ))

(defn victory? [state]
  (> 2 (count (group-by :team (vals (:units state))))))

(defn break-stalemate [states]
  (->> states
       (partition 2 1)
       (map (fn [[state1 state2]]
              (cond
                (victory? state1) state1
                (= state1 state2) {:units {}}
                :else state1
                )))
       ))

(defn unitsum [state]
  (->> state
       (:units)
       (vals)
       (map :qty)
       (apply +)
       ))

(defn boost [team amount state]
  (update state :units (fn [units]
                         (reduce (fn [units [id unit]]
                                   (if (= team (:team unit))
                                     (update-in units [id :dmg] #(+ % amount))
                                     units
                                     ))
                                 units
                                 units
                                 ))))

(defn immune-wins? [state]
  (some #(= :immune (:team %)) (vals (:units state))))

(defn simulate-boost [base-state boost-amount]
  (->> base-state
       (boost :immune boost-amount)
       (iterate step)
       (break-stalemate)
       ;(map (fn [state] (pprint state) state))
       (filter victory?)
       (first)
       ))

(defn find-min-boost [base-state]
  (loop [min-boost 0
         max-boost (* (unitsum base-state) (apply max (map :hp (vals (:units base-state)))))
         ]
    (if (= max-boost min-boost)
      max-boost
      (let [boost-amount (int (/ (+ max-boost min-boost) 2))
            end-state (simulate-boost base-state boost-amount)
            ]
        (if (immune-wins? end-state)
          (recur min-boost boost-amount)
          (recur (inc boost-amount) max-boost)
          )))))

(defn solve-a [lines]
  (->> lines
       (parse-input)
       (iterate step)
       (break-stalemate)
       (filter victory?)
       (first)
       (unitsum)
       ))

(defn solve-b [lines]
  (let [base-state (parse-input lines)
        min-boost (find-min-boost base-state)
        ]
    (unitsum (simulate-boost base-state min-boost))
    ))

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   }
  )
