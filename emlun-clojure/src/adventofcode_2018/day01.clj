(ns adventofcode-2018.day01)

(defn get-diffs [lines]
  (map read-string lines)
)

(defn first-recurrence
  { :test #(assert (= 5 (first-recurrence [1 5 2 5 2]))) }

  ( [future]
    { :pre (seq? future) }
      (first-recurrence #{} future)
  )

  ( [history [present & future]]
    { :pre [(or (seq? future) (nil? future)) (coll? history)] }
      (if (nil? present)
        nil
        (if (contains? history present)
          present
          (recur (conj history present) future)
        )
      )
  )
)

(defn run
  [input-lines & args]
  (let [ diffs (get-diffs input-lines) ]
    {
     :A (reduce + diffs)
     :B (first-recurrence (reductions + 0 (cycle diffs)))
    }
  )
)
