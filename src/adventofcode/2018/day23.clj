(ns adventofcode.2018.day23
  (:require clojure.string
            [adventofcode.2018.util :refer [abs as->> vec-add vec-mul vec-sub]]
            ))

(defn parse-input [lines]
  (mapv (fn [line]
          (->> line
               (re-matches #".*?(-?\d+).*?(-?\d+).*?(-?\d+).*?(-?\d+).*?")
               (rest)
               (map read-string)
               (as->> [x y z r] {:pos [x y z] :r r})
               )
          )
        lines
        ))

(defn dist [p1 p2]
  (->> (vec-sub p1 p2)
       (map abs)
       (apply +)
       ))

(defn vec-middle [v1 v2]
  (mapv #(quot % 2) (vec-add v1 v2)))

(defn is-within-range [bot pos]
  (<= (dist (:pos bot) pos) (:r bot)))

(defn is-within-range-of-all [bots pos]
  (every? #(is-within-range % pos) bots))

(defn ranges-overlap [bot1 bot2]
  (<= (dist (:pos bot1) (:pos bot2)) (+ (:r bot1) (:r bot2))))

(defn overlaps [bots]
  (reduce (fn [result bot]
            (assoc result bot
                   (->> bots
                        (filter #(ranges-overlap bot %))
                        (set)
                        )))
          {}
          bots
          ))

(defn remove-bot-from-all-groups [overlaps-map rem-bot]
  (-> overlaps-map
      (dissoc rem-bot)
      (->> (reduce (fn [result [bot group]]
                     (assoc result bot (disj group rem-bot)))
                   {}
                   ))
      ))

(defn remove-least-overlapping-bot [overlaps-map]
  (let [[worst-bot overlap-with-worst]
        (->> overlaps-map
             (sort-by #(count (second %)))
             (first)
             )
        ]
    (remove-bot-from-all-groups overlaps-map worst-bot)
    ))

(defn all-groups-overlap-fully [overlaps-map]
  (every? (fn [[bot group]]
            (= (count overlaps-map) (count group))
            )
          overlaps-map))

(defn remove-bots-until-all-overlap [bots]
  (->> bots
       (overlaps)
       (iterate remove-least-overlapping-bot)
       (drop-while #(not (all-groups-overlap-fully %)))
       (first)
       (keys)
       (set)
   ))

(def adjacent-steps
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        :when (not (= [0 0 0] [dx dy dz]))
        ]
    [dx dy dz]
    ))

(defn bisect-towards [bots next-bot pos]
  (loop [pos pos
         target (:pos next-bot)
         ]
    (if (<= (dist pos target) 3)
      pos
      (let [midpos (vec-middle pos target)]
        (if (is-within-range-of-all bots midpos)
          (recur midpos target)
          (recur pos midpos)
          )))))

(defn step-towards [bots next-bot pos]
  (if (is-within-range next-bot pos)
    pos
    (let [botpos (:pos next-bot)
          curdist (dist pos botpos)
          stepsizes (take-while #(> % 0) (iterate #(quot % 2) curdist))
          dxyzs (mapcat (fn [stepsize]
                          (map #(vec-mul stepsize %) adjacent-steps))
                        stepsizes
                        )
          step (->> dxyzs
                    (map #(vec-add % pos))
                    (filter #(< (dist % botpos) curdist))
                    (sort-by #(dist % botpos))
                    (filter #(is-within-range-of-all bots %))
                    (first)
                    )
          ]
      (if (nil? step)
        nil
        (recur bots next-bot step)
        ))))

(defn step-towards-origin [bots pos]
  (let [curdist (dist pos [0 0 0])
        stepsizes (take-while #(> % 0) (iterate #(quot % 2) curdist))
        dxyzs (mapcat (fn [stepsize]
                        (map #(vec-mul stepsize %) adjacent-steps))
                      stepsizes
                      )
        step (->> dxyzs
                  (map #(vec-add % pos))
                  (filter #(< (dist % [0 0 0]) curdist))
                  (sort-by #(dist % [0 0 0]))
                  (filter #(is-within-range-of-all bots %))
                  (first)
                  )
        ]
    (if (nil? step)
      pos
      (recur bots step)
      )))

(defn pull-to-next [{:keys [processed-bots more-bots pos]}]
  (let [next-bot (first more-bots)
        rest-bots (next more-bots)
        ]
    {:processed-bots (conj processed-bots next-bot)
     :more-bots rest-bots
     :pos (step-towards processed-bots
                        next-bot
                        (bisect-towards processed-bots next-bot pos)
                        )
     }
  ))

(defn solve-a [lines]
  (let [bots (parse-input lines)
        {maxpos :pos, maxr :r} (apply max-key :r bots)
        ]
    (->> bots
         (filter (fn [bot]
                   (<= (dist (:pos bot) maxpos) maxr)))
         (count)
         )
    ))

(defn solve-b [lines]
  (let [bots (parse-input lines)
        best-bots (remove-bots-until-all-overlap bots)
        sorted-bots (sort-by :r best-bots)
        initial-state {:processed-bots []
                       :more-bots sorted-bots
                       :pos (:pos (first sorted-bots))
                       }
        ]
    (->> initial-state
         (iterate pull-to-next)
         (drop-while #(seq (:more-bots %)))
         (first)
         (:pos)
         (step-towards-origin best-bots)
         (dist [0 0 0])
         )))

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   }
  )

(defn day-lines [] (adventofcode.2018.core/day-lines 23))
