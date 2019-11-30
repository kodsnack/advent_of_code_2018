(ns adventofcode.2018.day06)

(defn parse-input [lines]
  (mapv (fn [line]
          (->> line
               (re-matches #"(-?\d+), (-?\d+)")
               (rest)
               (mapv read-string)
               )
          )
        lines
        ))

(defn initial-state [points]
  (let [heads (apply merge (map-indexed (fn [i p] { i #{p}}) points))
        ids (set (keys heads))
        ]
    {
     :heads heads
     :sizes (reduce (fn [result id] (assoc result id 1)) {} ids)
     :visited (set (apply concat (vals heads)))
     :banned #{}
     }))

(defn adjacent [[x y]]
  #{[(dec x) y]
    [(inc x) y]
    [x (dec y)]
    [x (inc y)]
    })

(defn grow-zones [state]
  (let [wanted-steps (reduce (fn [result [id heads]]
                               (assoc result id (set (mapcat adjacent heads))))
                             {}
                             (:heads state)
                             )
        step-counts (->> wanted-steps
             (vals)
             (apply concat)
             (reduce (fn [result step]
                       (update result step #(inc (or % 0))))
                     {})
             )
        contested-steps (->> step-counts
                             (filter (fn [[step count]] (> count 1)))
                             (map first)
                             (set)
                             )
        new-heads (reduce (fn [result [id wanted]]
                                 (assoc result id (->> wanted
                                                       (remove contested-steps)
                                                       (remove (:visited state))
                                                       (remove (:banned state))
                                                       (set)
                                                       )))
                               {}
                               wanted-steps
                               )
        ]
    (-> state
         (assoc :heads new-heads)
         (update :visited #(apply conj % (apply concat (vals new-heads))))
         (update :banned #(apply conj % (concat contested-steps)))
         (update :sizes (fn [sizes]
                          (reduce (fn [new-sizes [id steps]]
                                    (update new-sizes id #(+ % (count steps))))
                                  sizes
                                  new-heads
                                  )))
         )))

(defn bounding-box [points]
  (let [xs (map first points)
        ys (map second points)
        ]
    [(apply min xs)(apply max xs) (apply min ys) (apply max ys)]))

(defn inside-box [[minx maxx miny maxy] [x y]]
  (and
   (<= minx x)
   (<= x maxx)
   (<= miny y)
   (<= y maxy)
   ))

(defn solve-a [lines]
  (let [points (parse-input lines)
        box (bounding-box points)
        end-state (->> (initial-state points)
                       (iterate grow-zones)
                       (filter (fn [state]
                                 (not-any? #(inside-box box %) (apply concat (vals (:heads state))))))
                       (first)
                       )
        finite-zones (->> end-state
                          (:heads)
                          (filter (fn [[id steps]] (empty? steps)))
                          (map first))
        largest-zone (apply max-key (fn [id] ((:sizes end-state) id)) finite-zones)
        ]
    ((:sizes end-state) largest-zone)
    ))

(defn solve-b [lines] ())

(defn run [input-lines & args]
  { :A (solve-a input-lines)
    :B (solve-b input-lines)
    }
)

(defn day-lines [] (adventofcode.2018.core/day-lines 6))
