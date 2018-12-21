(ns adventofcode.2018.day15
  (:require clojure.string
            [adventofcode.2018.util :refer [as->>]]
            ))

(def examples
  [
   {:map "#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######"
    :rounds 47, :winner :goblins, :hp 590, :outcome 27730}
   {:map "#######\n#G..#E#\n#E#E.E#\n#G.##.#\n#...#E#\n#...E.#\n#######"
    :rounds 37, :winner :elves, :hp 982, :outcome 36334}
   {:map "#######\n#E..EG#\n#.#G.E#\n#E.##E#\n#G..#.#\n#..E#.#\n#######"
    :rounds 46, :winner :elves, :hp 859, :outcome 39514}
   {:map "#######\n#E.G#.#\n#.#G..#\n#G.#.G#\n#G..#.#\n#...E.#\n#######"
    :rounds 35, :winner :goblins, :hp 793, :outcome 27755}
   {:map "#######\n#.E...#\n#.#..G#\n#.###.#\n#E#G#G#\n#...#G#\n#######"
    :rounds 54, :winner :goblins, :hp 536, :outcome 28944}
   {:map "#########\n#G......#\n#.E.#...#\n#..##..G#\n#...##..#\n#...#...#\n#.G...G.#\n#.....G.#\n#########"
    :rounds 20, :winner :goblins, :hp 937, :outcome 18740}
   {:map "#######\n#E....#\n#.....#\n#.....#\n#.....#\n#....G#\n#######"}
   {:map "#######\n#....E#\n#.....#\n#.....#\n#.....#\n#G....#\n#######"}
   {:map "#######\n#G....#\n#.....#\n#.....#\n#.....#\n#....E#\n#######"}
   {:map "#######\n#....G#\n#.....#\n#.....#\n#.....#\n#E....#\n#######"}
   {:map "#######\n#G...G#\n#.....#\n#.....#\n#.....#\n#..E..#\n#######"}
   {:map "#######\n#G....#\n#.###.#\n#.#...#\n###.#.#\n#E....#\n#######"}
   {:map "#######\n#.....#\n#....G#\n#E....#\n#....G#\n#.....#\n#######"}
   {:map "#######\n#..E..#\n#.###.#\n#.#...#\n#...#.#\n###G###\n#######"}
   ])

(defn format-example [s]
  (->> s
       (clojure.string/split-lines)
       (take 7)
       (map #(take 7 %))
       (map clojure.string/join)
       (clojure.string/join "\n")
       ))

(defn append-cell [state ch]
  (update state :map (fn [m]
                       (update m (dec (count m)) #(conj % ch)))))

(defn append-unit [state ch]
  (-> state
      (update :units (fn [units]
                       (conj units {:type ch
                                    :pos [(dec (count (:map state))) (count (last (:map state)))]
                                    :hp 200
                                    :power 3
                                    })
                       ))
      (append-cell \.)
      ))

(defn parse-state [lines]
  (->> lines
    (reduce (fn [state line]
              (reduce (fn [state ch]
                        (case ch
                          (\E \G) (append-unit state ch)
                          (append-cell state ch)
                          ))
                      (update state :map (fn [m] (conj m [])))
                      line
                      ))
            {
             :units []
             :moved-units []
             :map []
             :rounds 0
             }
            )
    (as->> $ (update $ :units #(apply list %)))
    ))

(defn parse-example [i]
  (parse-state (clojure.string/split-lines (:map (examples i)))))

(defn format-map [world]
  (->> world
       (map (fn [row] (map (fn [c] (if (number? c) (mod c 10) c)) row)))
       (map-indexed (fn [i line]
                      (format "%3d %s" i (clojure.string/join line))))
       (clojure.string/join \newline)
       ))

(defn place-units [state]
  (reduce (fn [lines {[y x] :pos type :type}]
            (update-in lines [y x] (fn [_] type))
            )
          (:map state)
          (concat (:moved-units state) (:units state))
          ))

(defn vec-add [& vectors]
  (apply mapv + vectors))
(defn vec-sub [& vectors]
  (apply mapv - vectors))

(defn adjacent [pos]
  [(vec-add pos [-1 0])
   (vec-add pos [0 -1])
   (vec-add pos [0 1])
   (vec-add pos [1 0])
   ])

(defn unoccupied [map poss]
  (filter #(= \. (get-in map %)) poss))

(defn flood [world heads destinations]
  (if (or (some destinations heads) (empty? heads))
    [world (keep destinations heads)]
    (let [next-n (inc (get-in world (first heads)))
          [new-map new-heads] (reduce (fn [[world new-heads] pos]
                                        (reduce (fn [[world new-heads] new-pos]
                                                  (if (= \. (get-in world new-pos))
                                                    [(assoc-in world new-pos next-n) (conj new-heads new-pos)]
                                                    [world new-heads]
                                                    ))
                                                [world new-heads]
                                                (adjacent pos)
                                                ))
                                      [world []]
                                      heads
                                      )
          ]
      (recur new-map new-heads destinations)
      )
    ))

(defn find-path [flood-map paths]
  (let [step (get-in flood-map (first (first paths)))
        eliminate-duplicates (fn [paths]
                               (->> paths
                                    (sort-by first)
                                    (partition-by first)
                                    (map first)
                                    ))
        ]
    (if (= 0 step)
      (->> paths
           (map rest)
           (sort-by first)
           (first)
           )
      (recur flood-map
             (->> paths
                  (mapcat (fn [path]
                            (map #(cons % path)
                                 (->> path
                                      (first)
                                      (adjacent)
                                      (filter #(= (dec step) (get-in flood-map %)))
                                      ))))
                  (eliminate-duplicates)
                  (apply vector)
                  )))))

(defn plot-path [world path]
  (reduce (fn [world step]
            (assoc-in world step \+)
            )
          world
          path
  ))

(defn navigate [state start-pos destinations]
  (let [[flood-map closests] (flood (-> state
                                        (place-units)
                                        (assoc-in start-pos 0)
                                        )
                                    [start-pos]
                                    destinations
                                    )
        chosen-dest (first (sort closests))
        ]
    (if chosen-dest
      (find-path flood-map [(list chosen-dest)])
      )))

(defn all-units [state]
  (concat (:units state) (:moved-units state)))

(defn other-units [state]
  (concat (pop (:units state)) (:moved-units state)))

(defn enemies [state]
  (filter #(not= (:type (first (:units state))) (:type %))
          (other-units state)))

(defn choose-path [state]
  (let [unit (first (:units state))
        map-with-units (place-units state)
        ]
    (->> state
         (enemies)
         (mapcat (fn [unit] (unoccupied map-with-units (adjacent (:pos unit)))))
         (set)
         (navigate state (:pos unit))
         )))

(defn choose-step [state]
  (first (choose-path state)))

(defn unit-at [state pos]
  (->> state
       (all-units)
       (filter #(= pos (:pos %)))
       (first)
       ))

(defn can-attack [state]
  (let [unit (first (:units state))
        ]
    (->> unit
         (:pos)
         (adjacent)
         (filter (set (map :pos (enemies state))))
         (sort)
         )))

(defn damage-if-target [pos power unit]
  (if (= pos (:pos unit))
    (update unit :hp #(- % power))
    unit
    ))

(defn attack [state]
  (if-let [possible-attacks (seq (can-attack state))]
    (let [target-pos (->> possible-attacks
                          (sort-by (fn [[y x]] [(:hp (unit-at state [y x])) y x]))
                          (first)
                          )
          power (:power (first (:units state)))
          damage (partial damage-if-target target-pos power)
          remove-dead (partial filter #(> (:hp %) 0))
          damage-targeted (fn [units]
                            (->> units
                                 (map damage)
                                 (remove-dead)
                                 ))
          ]
      (-> state
          (update :units (comp #(apply list %) damage-targeted))
          (update :moved-units (comp #(apply vector %) damage-targeted))
          ))
    state
    ))

(defn shift-unit [state]
  (assoc state
         :units (pop (:units state))
         :moved-units (conj (:moved-units state) (first (:units state)))
         ))

(defn move-unit [state]
  (if-let [possible-attacks (seq (can-attack state))]
    (attack state)
    (if-let [chosen-step (choose-step state)]
      (-> state
          (update :units (fn [units]
                           (apply list
                                  (assoc (first units) :pos chosen-step)
                                  (pop units)
                                  )))
          (attack)
          )
      state
      )))

(defn step [state]
  (if (seq (:units state))
    (let [updated (shift-unit (move-unit state))]
      (if (seq (:units updated))
        updated
        (assoc updated
               :rounds (inc (:rounds updated))
               :units (apply list (sort-by :pos (:moved-units updated)))
               :moved-units []
               )))))

(defn victory [state]
  (->> state
       (all-units)
       (map :type)
       (set)
       (count)
       (= 1)
       ))

(defn hpsum [state]
  (reduce + (map :hp (all-units state))))

(defn outcome [state]
  (* (:rounds state) (hpsum state)))

(defn print-navigation [state]
  (let [start-pos (:pos (first (:units state)))]
    (-> state
        (place-units)
        (assoc-in start-pos 0)
        (flood [start-pos] #{})
        (first)
        (format-map)
        (println)
        )))

(defn format-state [state]
  (cond-> state
    true (place-units)
    (empty? (can-attack state)) (plot-path (choose-path state))
    true (format-map)
    ))

(defn print-state [state]
  (println "Round" (:rounds state))
  (println (format-state state))
  (println "Units:")
  (doseq [unit (:moved-units state)]
    (println unit))
  (println (first (:units state)) " <--")
  (doseq [unit (rest (:units state))]
    (println unit))
  )

(defn finish
  ([states] (finish states true))
  ([states output]
    (cond->> (last states)
      true (iterate step)
      output (map (fn [s] (do (print-state s) s)))
      true (reductions conj [])
      true (filter #(victory (last %)))
      true (first)
      true (concat states)
      true (apply vector)
      ))
  )

(defn elf? [unit] (= \E (:type unit)))

(defn elves-win? [state]
  (and (victory state)
       (every? elf? (all-units state))
       ))

(defn perfect-victory? [start-state end-state]
  (and (elves-win? end-state)
       (= (count (filter elf? (all-units start-state)))
          (count (filter elf? (all-units end-state)))
          )))

(defn simulate [start-state elf-power output]
  (-> start-state
      (update :units (fn [units]
                       (->> units
                            (map (fn [unit]
                                   (cond-> unit
                                     (elf? unit) (assoc :power elf-power))))
                            (apply list)
                            )))
      (vector)
      (finish output)
      (last)
      ))

(defn solve-a [lines]
  (-> lines
       (parse-state)
       (simulate 3 false)
       (outcome)
       ))

(defn find-min-elf-power [base-state]
  (loop [min-str 4
         max-str 200
         ]
    (if (= max-str min-str)
      max-str
      (let [elf-power (int (/ (+ max-str min-str) 2))
            end-state (simulate base-state elf-power false)
            ]
        (if (perfect-victory? base-state end-state)
          (recur min-str elf-power)
          (recur (inc elf-power) max-str)
          )))))

(defn solve-b [lines]
  (let [base-state (parse-state lines)
        min-power (find-min-elf-power base-state)
        ]
    (outcome (simulate base-state min-power false))
    ))

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   }
  )

(defn day-lines [] (adventofcode.2018.core/day-lines 15))
(def states [(parse-example 0)])
(defn show-state [] (print-state (last states)))
(defn start-day-lines [] (def states [(parse-state (day-lines))]) (show-state))
(defn start-example [i] (def states [(parse-example i)]) (show-state))
(defn run-example [i] (def states (finish [(parse-example i)])) [(outcome (last states)) (= (outcome (last states)) (:outcome (examples i)))])
(defn run-examples []
  (doseq [example (filter :outcome examples)]
    (->> example
      (:map)
      (clojure.string/split-lines)
      (parse-state)
      (as->> $ (finish [$] false))
      (last)
      (as->> $ [(outcome $) (= (:outcome example) (outcome $))])
      (println)
      )))
(defn run-a ([] (run-a true)) ([output] (def states (finish [(parse-state (day-lines))] output)) (outcome (last states))))
(defn n [] (def states (conj states (step (last states)))) (show-state))
(defn p [] (def states (pop states)) (show-state))
