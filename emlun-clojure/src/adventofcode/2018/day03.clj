(ns adventofcode.2018.day03)

(defn all-rect-coords [left-x top-y width height]
  (let [xs (take width (iterate inc left-x))
        ys (take height (iterate inc top-y))
        ]
    (mapcat
      (fn [x]
        (map (fn [y] [x y]) ys))
      xs
    )))

(defn parse-line [line]
  (map read-string (rest (re-matches #"\#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" line))))

(defn solve-a [lines]
  (let [claims (reduce (fn [counts line]
            (let [[id & rect] (parse-line line)]
              (reduce (fn [cnts coord]
                        (update cnts coord #(inc (or % 0)))
                        )
                      counts
                      (apply all-rect-coords rect))
            )
            )
          {}
          lines)]
    (count (filter #(< 1 %) (vals claims)))))

(defn run [input-lines & args]
  { :A (solve-a input-lines)
    :B (solve-b input-lines)
  }
)
