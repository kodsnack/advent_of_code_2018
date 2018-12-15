(ns adventofcode.2018.day15
  (:require clojure.string
            [clojure.spec.alpha :as spec]))

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
    :rounds 20, :winner :goblins, :hp 937, :outcome 18746}
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
  (as-> state $
    (update $ :units (fn [units]
                       (conj units {:type ch
                                    :pos [(dec (count (:map state))) (count (last (:map state)))]
                                    :hp 200
                                    :power 3
                                    })
                       ))
    (append-cell $ \.)
    ))

(defn parse-state [lines]
  (as-> lines $
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
               $
               )
       (update $ :units #(apply list %))
       ))

(defn parse-example [i]
  (parse-state (clojure.string/split-lines (:map (examples i)))))

(defn format-map [map]
  (->> map
       (map-indexed (fn [i line]
                      (format "%3d %s" i (clojure.string/join line))))
       (clojure.string/join \newline)
       ))

(defn format-state [state]
  (->> state
       (place-units)
       (format-map)
       ))

(defn print-state [state]
  (println "Round" (:rounds state))
  (println (format-state state))
  (println "Units:")
  (doseq [unit (sort-by :pos (:units state))]
    (println unit))
  (println "Moved:")
  (doseq [unit (:moved-units state)]
    (println unit))
  )

(defn vec-add [& vectors]
  (apply mapv + vectors))

(defn place-units [state]
  (reduce (fn [lines {[y x] :pos type :type}]
            (update-in lines [y x] (fn [_] type))
            )
          (:map state)
          (concat (:moved-units state) (:units state))
          ))

(defn adjacent [pos]
  [(vec-add pos [-1 0])
   (vec-add pos [0 -1])
   (vec-add pos [0 1])
   (vec-add pos [1 0])
   ])

(defn flood [map heads destination]
  (if (and (seq heads) (= \. (get-in map destination)))
    (let [next-n (inc (get-in map (first heads)))
          [new-map new-heads] (reduce (fn [[map new-heads] pos]
                                        (reduce (fn [[map new-heads] new-pos]
                                                  (if (= \. (get-in map new-pos))
                                                    [(assoc-in map new-pos next-n) (conj new-heads new-pos)]
                                                    [map new-heads]
                                                    ))
                                                [map new-heads]
                                                (adjacent pos)
                                                ))
                                      [map []]
                                      heads
                                      )
          ]
      (recur new-map new-heads destination)
      )
    [map (get-in map destination)]
    ))

(defn first-step [flood-map destination]
  (let [step (get-in flood-map destination)]
    (if (= 1 step)
      destination
      (recur flood-map
             (->> destination
                  (adjacent)
                  (filter #(= (dec step) (get-in flood-map %)))
                  (first)
                  )
             )
      )))

(defn navigate [state start-pos target-pos]
  (let [[flood-map min-steps] (flood (-> state
                                          (place-units)
                                          (assoc-in start-pos 0)
                                          (assoc-in target-pos \.)
                                          )
                                      [start-pos]
                                      target-pos
                                      )
        reachable (not= \. min-steps)
        ]
    (if reachable (first-step flood-map target-pos))))

(defn move-unit [state]
  (assoc state
         :units (pop (:units state))
         :moved-units (conj (:moved-units state) (first (:units state)))
   ))

(defn step [state]
  (if (seq (:units state))
    (move-unit state)
    (assoc state
           :rounds (inc (:rounds state))
           :units (apply list (:moved-units state))
           :moved-units []
           )
    ))

(defn flip [[y x]]
  [x y])

(defn solve-a [lines] ())

(defn solve-b [lines] ())

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   }
)

(defn day-lines [] (adventofcode.2018.core/day-lines 13))
(def states [(parse-example 0)])
(defn start-example [i] (def states [(parse-example i)]) (show-state))
(defn show-state [] (print-state (last states)))
(defn n [] (def states (conj states (step (last states)))) (show-state))
(defn p [] (def states (pop states)) (show-state))
