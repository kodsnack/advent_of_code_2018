(ns adventofcode.2018.day25
  (:require clojure.set
            clojure.string
            [adventofcode.2018.util :refer [abs as->> cartprod first-fixpoint vec-sub]]
            ))

(defn parse-input [lines]
  (->> lines
       (map #(read-string (str "[" % "]")))
       (map (comp set list))
       ))

(defn dist [p1 p2]
  (->> (vec-sub p1 p2)
       (map abs)
       (apply +)
       ))

(defn dists [set1 set2]
  (->> (cartprod set1 set2)
       (map #(apply dist %))
       ))

(defn min-dist-lt? [threshold set1 set2]
  (->> (dists set1 set2)
       (some #(<= % threshold))
       ))

(defn merge-constellations [constellations]
  (->> constellations
       (reduce (fn [constellations constellation]
                 (->> constellations
                      (filter (fn [existing-constellation]
                                (min-dist-lt? 3 constellation existing-constellation)))
                      (first)
                      (as->> existing-constellation
                             (if existing-constellation
                               (-> constellations
                                   (disj existing-constellation)
                                   (conj (clojure.set/union constellation existing-constellation))
                                   )
                               (conj constellations constellation)
                               ))
                      ))
               #{}
               )
       ))

(defn solve-a [lines]
  (->> lines
       (parse-input)
       (first-fixpoint merge-constellations)
       (count)
       ))

(defn solve-b [lines]
  ())

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   }
  )
