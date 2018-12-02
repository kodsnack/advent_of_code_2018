(ns adventofcode.2018.core
  (:require clojure.string))

(defn pad
  ([day] (pad day "0"))
  ([day padding]
    (if (number? day)
      (recur (str day) padding)
      (if (= 1 (count day))
        (str padding day)
        day))))

(defn day-namespace [day]
  (symbol (str "adventofcode.2018.day" (pad day))))

(defn day-file [day]
  (str "resources/day" (pad day) ".in"))

(defn day-header [day]
  (str "=== Day " (pad day " ") " ==="))

(defn format-day-results [result]
  (clojure.string/join "\n"
    [
      (str "A: " (:A result))
      (str "B: " (:B result))
    ]))

(defn day-lines
  ([day] (day-lines day (day-file day)))
  ([day file]
    (let [input (if (= "-" file) *in* file)]
      (clojure.string/split-lines (slurp input)))))

(defn run-day [day file]
  (try
    (do
      (let [lines (day-lines day file)
            dayspace (day-namespace day)
            ]
        (require dayspace)
        (let [run (resolve (symbol (str dayspace) "run"))]
          (assoc (run lines) :day day))
      )
    )
    (catch java.io.FileNotFoundException e nil)))

(defn run-day-with-file [day]
  (run-day day (day-file day)))

(defn -main
  "Run the solver for the given day (or all) with lines from file as argument. If file is -, use standard input; if not given, use default."
  ([]
    (->> (iterate inc 1)
      (take 25)
      (map run-day-with-file)
      (filter #(not (nil? %)))
      (map (fn [result]
        (clojure.string/join "\n\n" [
          (day-header (:day result))
          (format-day-results result)
        ])
      ))
      (clojure.string/join "\n\n\n")
      println))

  ([day] (-main day (day-file day)))
  ([day file & args]
    (println (format-day-results (run-day day file)))))
