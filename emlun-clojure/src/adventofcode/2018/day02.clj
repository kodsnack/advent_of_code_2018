(ns adventofcode.2018.day02)

(defn char-counts [line]
  (reduce
    (fn [counts c]
      (assoc counts c (inc (get counts c 0)))
    )
    {}
    line))

(defn solve-a [lines]
  (->> lines
    (map char-counts)
    (map (fn [counts]
      {
        2 (if (some #(= 2 (second %)) counts) 1 0)
        3 (if (some #(= 3 (second %)) counts) 1 0)
      }))
    (concat [+ { 2 0, 3 0 }])
    (apply merge-with)
    ((fn [{div2 2, div3 3}] (* div2 div3)))
  ))

(defn without-char [i word]
  (let [[head tail] (split-at i word)]
    (str (apply str head) (apply str (rest tail)))
    ))

(defn nonunique [coll]
  (loop [
         seen #{}
         [next & remaining] coll
         nonunique #{}
         ]
    (if (nil? next)
      nonunique
      (recur
        (conj seen next)
        remaining
        (if (contains? seen next)
          (conj nonunique next)
          nonunique
        )
      )
    )
    ))

(defn solve-b [lines]
  (let [
        withouts-per-line
          (map
            (fn [line]
              (reduce
                (fn [result i]
                  (assoc result i (without-char i line))
                )
                {}
                (take-while #(< % (count line)) (iterate inc 0))
              )
            )
            lines)
        all-withouts (reduce
                       (fn [result withouts]
                         (merge-with #(flatten (vector %1 %2)) result withouts))
                       {}
                       withouts-per-line)
        ]
    (first (mapcat #(nonunique (second %)) all-withouts))
  ))

(defn run [input-lines & args]
  { :A (solve-a input-lines)
    :B (solve-b input-lines)
  }
)
