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

(defn sqgrid [min-in max-ex]
  (grid min-in max-ex min-in max-ex))

(defn total-power-level [serial [x y]]
  (let [coords (grid x (+ x 3) y (+ y 3))]
    (->> coords
      (map (partial power-level serial))
      (reduce +)
      )
    ))

(defn solve-a [lines]
  (let [serial (grid-serial-number lines)
        [x y] (apply max-key (partial total-power-level serial) (sqgrid 1 299))
        ]
    (str x "," y)))

(defn solve-b [lines]
  "hej")

(defn run [input-lines & args]
  { :A (solve-a input-lines)
   :B (solve-b input-lines)
   }
  )
