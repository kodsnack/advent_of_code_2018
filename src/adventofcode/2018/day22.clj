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

(defn type [cave pos]
  (mod (get-in cave [:erosion-level pos]) 3))

(defn compute-cave [{depth :depth [target-y target-x :as target] :target}]
  (->> (grid 0 (inc target-y) 0 (inc target-x))
       (reduce (fn [cave pos]
                 (as-> cave $
                     (assoc-in $ [:geoindex pos] (geoindex $ pos))
                     (assoc-in $ [:erosion-level pos] (erosion-level $ pos))
                     (assoc-in $ [:type pos] (type $ pos))
                     ))
               {:geoindex {}
                :erosion-level {}
                :type {}
                :depth depth
                :target target
                })
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
