(ns adventofcode.2018.day05
  (:require clojure.string))

(defn is-lowercase [c] (= (str c) (clojure.string/lower-case c)))
(defn is-uppercase [c] (= (str c) (clojure.string/upper-case c)))

(defn first-fixpoint [f x]
  (->> x
       (iterate f)
       (partition 2 1)
       (drop-while #(apply not= %))
       (first)
       (first)
       ))

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

(defn solve-a [line] (count (first-fixpoint step-reduction line)))

(defn solve-b [lines] ())

(defn run [input-lines & args]
  { :A (solve-a (first input-lines))
    :B (solve-b (first input-lines))
    }
)
