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
    :else (* (get-in erosion-level [y (dec x)]) (get-in erosion-level [(dec y) x]))
    ))

(defn erosion-level [cave [y x]]
  (mod (+ (:depth cave) (get-in cave [:geoindex y x])) 20183))

(defn region-type [cave [y x]]
  (mod (get-in cave [:erosion-level y x]) 3))

(defn minimal-cave [cave]
  (assoc cave
         :geoindex []
         :erosion-level []
         :type []
         ))

(defn expand-cave-y [cave]
  (let [y (count (:type cave))]
    (reduce (fn [cave x]
              (as-> cave $
                (update-in $ [:geoindex y] #(conj % (geoindex $ [y x])))
                (update-in $ [:erosion-level y] #(conj % (erosion-level $ [y x])))
                (update-in $ [:type y] #(conj % (region-type $ [y x])))
                ))
            (-> cave
                (update :geoindex #(conj % []))
                (update :erosion-level #(conj % []))
                (update :type #(conj % []))
                )
            (range 0 (count (first (:type cave))))
            )))

(defn expand-cave-x [cave]
  (let [x (count (first (:type cave)))]
    (reduce (fn [cave y]
              (as-> cave $
                (update-in $ [:geoindex y] #(conj % (geoindex $ [y x])))
                (update-in $ [:erosion-level y] #(conj % (erosion-level $ [y x])))
                (update-in $ [:type y] #(conj % (region-type $ [y x])))
                ))
            cave
            (range 0 (count (:type cave)))
            )
    ))

(defn expand-cave [cave [target-y target-x]]
  (->> cave
      (iterate expand-cave-y)
      (filter #(= (count (:type %)) (inc target-y)))
      (first)
      (iterate expand-cave-x)
      (filter #(= (count (first (:type %))) (inc target-x)))
      (first)
      ))

(defn compute-cave [{:keys [target] :as cave}]
  (expand-cave (minimal-cave cave) target))

(defn solve-a [lines]
  (->> lines
       (parse-input)
       (compute-cave)
       (:type)
       (reduce #(apply + %1 %2) 0)
       ))

(defn solve-b [lines]
  ())

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   }
  )

(defn day-lines [] (adventofcode.2018.core/day-lines 22))
