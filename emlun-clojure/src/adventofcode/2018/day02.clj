(ns adventofcode.2018.day02)

(def lines (adventofcode.2018.core/day-lines 2))

(defn get-counts [lines]
  (map
    (fn [line]
      (reduce
        (fn [counts c]
          (assoc counts c (inc (get counts c 0)))
        )
        {}
        line)
    )
    lines))

(defn count-filter
  "([pred] [pred coll])
  Shortcut for (count (filter pred coll))."
  {
   :test (fn []
           (assert (= 2 (count-filter #(= 5 %) [1 2 5 3 5 7])))
           (assert (= [1 2] (map (count-filter #(= 5 %)) [[1 5] [5 2 5]])))
           )
  }
  [ pred coll ]
    (reduce (fn [count next] (+ count (if (pred next) 1 0))) 0 coll)
)

((fn [{div2 2, div3 3}] (* div2 div3))
  (reduce
    (fn [result next]
      { 2 (+ (result 2) (next 2))
        3 (+ (result 3) (next 3))
      }
    )
    { 2 0, 3 0 }
    (map
      (fn [counts]
        {
          2 (if (some #(= 2 (second %)) counts) 1 0)
          3 (if (some #(= 3 (second %)) counts) 1 0)
        }
      )
      (get-counts lines)))
)

(defn first-recurrence [coll]
  (loop [history #{}
         [present & future] coll
         ]
    (if-not (nil? present)
      (if (contains? history present)
        present
        (recur (conj history present) future)))))

(defn run [input-lines & args]
  (let [ diffs (get-diffs input-lines) ]
    { :A (reduce + diffs)
      :B (first-recurrence (reductions + 0 (cycle diffs)))
    }
  ))
