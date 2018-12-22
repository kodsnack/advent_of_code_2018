(ns adventofcode.2018.day22
  (:require clojure.string
            [adventofcode.2018.util :refer [grid]]
            ))

(defn parse-input [[depth-line target-line]]
  {:depth (read-string (second (clojure.string/split depth-line #"\s+")))
   :target (let [[_ x y] (re-matches #".*:\s+(\d+),(\d+).*" target-line)]
             (mapv read-string [y x]))
   })

(defn geoindex [{:keys [erosion-level target]} [y x]]
  (cond
    (= [y x] [0 0]) 0
    (= [y x] target) 0
    (= 0 y) (* x 16807)
    (= 0 x) (* y 48271)
    :else (* (erosion-level [y (dec x)]) (erosion-level [(dec y) x]))
    ))

(defn erosion-level [cave pos]
  (mod (+ (:depth cave) (geoindex cave pos)) 20183))

(defn region-type [cave pos]
  (mod (get-in cave [:erosion-level pos]) 3))

(defn add-cave-pos [cave pos]
  (as-> cave $
    (assoc-in $ [:geoindex pos] (geoindex $ pos))
    (assoc-in $ [:erosion-level pos] (erosion-level $ pos))
    (assoc-in $ [:type pos] (region-type $ pos))
    ))

(defn expand-cave-y [cave]
  (let [y (inc (first (:maxpos cave)))
        xs (range 0 (inc (second (:maxpos cave))))
        ]
    (->> xs
         (map #(vector y %))
         (reduce add-cave-pos cave)
         (as->> $ (update-in $ [:maxpos 0] inc))
         )))

(defn expand-cave-x [cave]
  (let [ys (range 0 (inc (first (:maxpos cave))))
        x (inc (second (:maxpos cave)))
        ]
    (->> ys
         (map #(vector % x))
         (reduce add-cave-pos cave)
         (as->> $ (update-in $ [:maxpos 1] inc))
         )))

(defn compute-cave [{depth :depth [target-y target-x :as target] :target}]
  (-> {:geoindex {}
       :erosion-level {}
       :type {}
       :depth depth
       :target target
       :maxpos [0 0]
       }
      (add-cave-pos [0 0])
      (as-> $ (iterate expand-cave-x $))
      (nth target-x)
      (as-> $ (iterate expand-cave-y $))
      (nth (inc target-y))
      ))

(defn solve-a [lines]
  (->> lines
       (parse-input)
       (compute-cave)
       (:type)
       (vals)
       (apply +)
       ))

(defn solve-b [lines]
  ())

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   }
  )

(defn day-lines [] (adventofcode.2018.core/day-lines 22))
