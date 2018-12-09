(ns adventofcode.2018.day01)

(defn get-diffs [lines]
  (map read-string lines))

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
