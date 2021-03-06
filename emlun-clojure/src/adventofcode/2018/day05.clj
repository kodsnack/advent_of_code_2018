(ns adventofcode.2018.day05
  (:require clojure.string
            [adventofcode.2018.util :refer [first-fixpoint]]
            ))

(defn is-lowercase [c] (= (str c) (clojure.string/lower-case c)))
(defn is-uppercase [c] (= (str c) (clojure.string/upper-case c)))

(defn will-react [a b]
  (if (or (nil? a) (nil? b))
    false
    (and
     (= (clojure.string/lower-case a) (clojure.string/lower-case b))
     (or
      (and (is-uppercase a) (is-lowercase b))
      (and (is-lowercase a) (is-uppercase b))
      ))))

(defn step-reduction [line]
  (->> line
       (reduce (fn [[processed prev] next]
                 (if (will-react prev next)
                   [processed nil]
                   [(conj processed prev) next]
                   ))
               [[] nil]
               )
       (apply conj)
       (clojure.string/join)
       ))

(defn eliminate [line ch]
  (->> line
       (filter #(not= ch (first (clojure.string/lower-case %))))
       (apply str)
  ))

(defn solve-a [line] (count (first-fixpoint step-reduction line)))

(defn solve-b [line]
  (let [alphabet (set (clojure.string/lower-case line))
        ]
    (apply min
           (map #(solve-a (eliminate line %))
                alphabet
                ))))

(defn run [input-lines & args]
  { :A (solve-a (first input-lines))
    :B (solve-b (first input-lines))
    }
)
