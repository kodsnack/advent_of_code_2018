(ns adventofcode.2018.day09
  (:require clojure.string))

(defn parse-input [lines]
  (let [[_ num-players last-marble] (re-matches #"(\d+) players.*worth (\d+) points" (first lines))]
    {:players (read-string num-players)
     :last-marble (read-string last-marble)
     }))

(defn start [state]
  (assoc state
         :front [0]
         :back ()
         :next-marble 1
         :player 0
         :scores (->> state
                      (:players)
                      (range)
                      (map #(vector % 0))
                      (into {})
                      )
         ))

(defn shift-circle-forward [{:keys [front back] :as state} steps]
  (let [[backfront backback] (split-at steps back)]
    (if (= steps (count backfront))
      (-> state
          (update :front #(apply conj % backfront))
          (assoc :back backback)
          )
      (-> state
          (assoc :front [])
          (assoc :back (concat front back))
          (recur (- steps (count backfront)))
          ))))

(defn shift-circle-backward [{:keys [front back] :as state} steps]
  (let [front-split-i (- (count front) steps)]
    (if (> front-split-i 0)
      (let [frontfront (subvec front 0 front-split-i)
            frontback (subvec front front-split-i)
            ]
        (assoc state
               :front frontfront
               :back (concat frontback back)
               ))
      (-> state
          (update :front #(apply conj % back))
          (assoc :back ())
          (recur (- front-split-i))
          ))))

(defn place [state]
  (-> state
      (shift-circle-forward 1)
      (update :front #(conj % (:next-marble state)))
      ))

(defn score [state]
  (-> state
      (shift-circle-backward 7)
      (as-> $ (update-in $ [:scores (:player $)]
                         (fn [score]
                           (+ score
                              (get-in $ [:front (dec (count (:front $)))])
                              (:next-marble $)
                              ))))
      (update :front #(subvec % 0 (dec (count %))))
      (shift-circle-forward 1)
      ))

(defn step [state]
  (let [score-turn (= 0 (mod (:next-marble state) 23))]
    (cond-> state
      score-turn score
      (not score-turn) place
      true (update :next-marble inc)
      true (update :player #(mod (inc %) (:players state)))
      )))

(defn solve [state]
  (->> state
       (start)
       (iterate step)
       (filter (fn [state]
                 (> (:next-marble state) (:last-marble state))))
       (first)
       (:scores)
       (vals)
       (apply max)
       ))

(defn solve-a [lines]
  (->> lines
       (parse-input)
       (solve)
       ))

(defn solve-b [lines]
  (-> lines
       (parse-input)
       (update :last-marble #(* % 100))
       (solve)
       ))

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   }
  )

(defn day-lines [] (adventofcode.2018.core/day-lines 9))
