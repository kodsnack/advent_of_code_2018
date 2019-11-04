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

(defn find-best-step-in-range [bots pos target]
  (let [curdist (dist pos target)
        stepsizes (take-while #(> % 0) (iterate #(quot % 2) curdist))
        dxyzs (mapcat (fn [stepsize]
                        (map #(vec-mul stepsize %) adjacent-steps))
                      stepsizes
                      )
        step (->> dxyzs
                  (map #(vec-add % pos))
                  (filter #(< (dist % target) curdist))
                  (sort-by #(dist % target))
                  (filter #(is-within-range-of-all bots %))
                  (first)
                  )
        ]
    step
    ))

(defn step-into-range [bots next-bot pos]
  (if (is-within-range next-bot pos)
    pos
    (if-let [step (find-best-step-in-range bots pos (:pos next-bot))]
      (recur bots next-bot step))))

(defn step-towards-origin [bots pos]
  (if-let [step (find-best-step-in-range bots pos [0 0 0])]
    (recur bots step)
    pos
    ))

(defn find-pos-in-range-of-all
  ([processed-bots more-bots pos]
   (if-let [next-bot (first more-bots)]
     (recur (conj processed-bots next-bot)
            (next more-bots)
            (step-into-range processed-bots next-bot pos)
            )
     pos
     ))
  ([bots]
   (let [sorted-bots (sort-by :r bots)]
     (find-pos-in-range-of-all []
                               sorted-bots
                               (:pos (first sorted-bots))
                               ))))

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
        initial-pos (find-pos-in-range-of-all best-bots)
        final-pos (step-towards-origin best-bots initial-pos)
        ]
    (dist [0 0 0] final-pos)))

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   }
  )

(defn day-lines [] (adventofcode.2018.core/day-lines 23))
