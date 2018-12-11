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

(defn cumsum [coll] (reductions + 0 coll))

(defn row-add [row1 row2] (mapv + row1 row2))

(defn sum-grid [rows]
  (->> rows
       (mapv cumsum)
       (reductions row-add (take (inc (count (first rows))) (repeat 0)))
       (map #(apply vector %))
       (apply vector)
       ))

(defn power-grid-rows [serial]
  (mapv (fn [y]
          (mapv #(power-level serial [% y]) (range 1 301)))
        (range 1 301)
        ))

(defn square-sum [sumgrid [x y size]]
  (let [xi (dec x)
        yi (dec y)
        ]
    (-
     (+ ((sumgrid yi) xi)          ((sumgrid (+ yi size)) (+ xi size)))
     (+ ((sumgrid yi) (+ xi size)) ((sumgrid (+ yi size)) xi))
    )
  ))

(defn solve-a [sumgrid]
  (apply max-key
         (fn [[x y]] (square-sum sumgrid [x y 3]))
         (sqgrid 1 1 298)
         ))

(defn solve-b [sumgrid]
  (let [triplets (for [size (range 1 301)
                       x (range 1 (- 301 size))
                       y (range 1 (- 301 size))
                       ]
                   [x y size])
        ]
    (apply max-key
           (partial square-sum sumgrid)
           triplets
           )))

(defn run [input-lines & args]
  (let [serial (grid-serial-number input-lines)
        pg-rows (power-grid-rows serial)
        sumgrid (sum-grid pg-rows)
        ]
    { :A (clojure.string/join "," (solve-a sumgrid))
     :B (clojure.string/join "," (solve-b sumgrid))
     }
  )
)
