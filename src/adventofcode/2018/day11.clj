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
  (mapcat (fn [x] (map #(vector x %) ys)) xs
  ))

(defn grid [minx maxx miny maxy]
  (cartprod
    (take (- maxx minx) (iterate inc minx))
    (take (- maxy miny) (iterate inc miny))
  ))

(defn total-power-level [serial [x y]]
  (let [coords (grid x (+ x 3) y (+ y 3))]
    (->> coords
      (map (partial power-level serial))
      (reduce +)
      )
    ))

(defn solve-a [lines]
  (let [serial (grid-serial-number lines)
        [x y] (apply max-key (partial total-power-level serial) (grid 1 299 1 299))
        ]
    (str x "," y)))

(defn solve-b [lines]
  "hej")

(defn run [input-lines & args]
  { :A (solve-a input-lines)
   :B (solve-b input-lines)
   }
  )
