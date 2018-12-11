(ns adventofcode.2018.day11
  (:require clojure.string))

(defn grid-serial-number [lines] (read-string (first lines)))

(defn rack-id [x y] (+ x 10))

(defn keep-hundreds-digit [x]
  (mod (int (/ x 100)) 10))

(defn power-level [serial [x y]]
  (->> [x y]
    (apply rack-id)
    (* y)
    (+ serial)
    (* (rack-id x y))
    (keep-hundreds-digit)
    (+ (- 5))
    ))

(defn cartprod [xs ys]
  (for [x xs
        y ys]
    [x y]
    ))

(defn grid [minx-in maxx-ex miny-in maxy-ex]
  (cartprod
    (range minx-in maxx-ex)
    (range miny-in maxy-ex)
  ))

(defn sqgrid [minx miny size]
  (grid minx (+ minx size) miny (+ miny size)))

(defn transpose [& colls]
  (apply map vector colls)
  )

(defn cumsum [coll] (reductions + 0 coll))

(defn row-add [row1 row2] (mapv + row1 row2))

(defn sum-grid [rows]
  (->> rows
       (mapv cumsum)
       (reductions row-add (take (inc (count (first rows))) (repeat 0)))
       (map #(apply vector %))
       (apply vector)
       ))

(defn to-map [pairs]
  (->> pairs
    (apply transpose)
    (apply zipmap)
  ))

(defn to-grid-map [f grid]
  (to-map
    (for [[x y] grid]
      [[x y] (f [x y])])
  ))

(defn zero-power-grid []
  (to-grid-map (fn [_] 0) (sqgrid 1 1 300)))

(defn power-grid [serial]
  (to-grid-map (partial power-level serial) (sqgrid 1 1 300)))

(defn power-grid-rows [serial]
  (mapv (fn [y]
          (mapv #(power-level serial [% y]) (range 1 301)))
        (range 1 301)
        ))

(defn region-border-coords [size [x y]]
  (let [maxx (dec (+ size x))
        maxy (dec (+ size y))
        xs (range x (inc maxx))
        ys (range y (inc maxy))
        coords (concat
                 (map #(vector % maxy) xs)
                 (map #(vector maxx %) ys)
                 )
        ]
    (butlast coords)
  ))

(defn sum-border [pg size [x y]]
  (let [coords (region-border-coords size [x y])
        ]
    (->> coords
      (map pg)
      (reduce +)
      )
  ))

(defn square-sum [sumgrid [x y size]]
  (let [xi (dec x)
        yi (dec y)
        sg (fn [x y] (get-in sumgrid [y x]))]
    (-
     (+ (sg xi yi)          (sg (+ xi size) (+ yi size)))
     (+ (sg (+ xi size) yi) (sg xi (+ yi size)))
    )
  ))

(defn next-total-power-grid [pg prev-totpg size]
  (to-grid-map
    (fn [[x y]] (+ (prev-totpg [x y]) (sum-border pg size [x y])))
    (sqgrid 1 1 (- 301 size))
    ))

(defn total-power-grid [size pg]
  (if (= 1 size)
    pg
    (next-total-power-grid pg (total-power-grid (dec size) pg) size)
    ))

(defn triplet [size [x y]]
  [x y size])

(defn max-triplet [grid size]
  (->> (sqgrid 1 1 (- 301 size))
    (apply max-key grid)
    (triplet size)
  ))

(defn solve-a [sumgrid]
  (apply max-key
         (fn [[x y]] (square-sum sumgrid [x y 3]))
         (sqgrid 1 1 298)
         ))

(defn solve-b [pg]
  (loop [[size & rest-sizes] (range 1 301)
         max-value (pg [1 1])
         max-tripl [1 1 1]
         prev-totpg (zero-power-grid)
         ]
    (if (nil? size)
      max-tripl
      (let [totpg (next-total-power-grid pg prev-totpg size)
            maxtrip (max-triplet totpg size)
            [x y size] maxtrip
            maxval (totpg [x y])
            ]
        (if (> maxval max-value)
          (recur
            rest-sizes
            maxval
            maxtrip
            totpg
            )
          (recur
            rest-sizes
            max-value
            max-tripl
            totpg
            )
        )
      )
    )
  )
)

(defn run [input-lines & args]
  (let [serial (grid-serial-number input-lines)
        pg (power-grid serial)
        pg-rows (power-grid-rows serial)
        sumgrid (sum-grid pg-rows)
        ]
    { :A (clojure.string/join "," (solve-a sumgrid))
     :B (clojure.string/join "," (solve-b pg))
     }
  )
)
