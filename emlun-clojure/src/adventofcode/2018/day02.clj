(ns adventofcode.2018.day02)

(defn counts [coll]
  (reduce
    (fn [counts c]
      (update counts c #(inc (or % 0)))
    )
    {}
    coll))

(defn solve-a [lines]
  (->> lines
    (map (comp vals counts))
    (map (fn [counts]
      [
        (if (some #(= 2 %) counts) 1 0)
        (if (some #(= 3 %) counts) 1 0)
      ]))
    (reduce #(map + %1 %2))
    (apply *)
  ))

(defn without-char [i word]
  (let [[head tail] (split-at i word)]
    (str (apply str head) (apply str (rest tail)))
    ))

(defn nonunique [coll]
  (->> coll
    (counts)
    (filter #(< 1 (second %)))
    (map first)
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
