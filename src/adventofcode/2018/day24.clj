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
  (= 1 (count (group-by :team (vals (:units state))))))

(defn unitsum [state]
  (->> state
       (:units)
       (vals)
       (map :qty)
       (apply +)
       ))

(defn solve-a [lines]
  (->> lines
       (parse-input)
       (iterate step)
       (filter victory?)
       (first)
       (unitsum)
       ))

(defn solve-b [lines]
  ())

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   }
  )
