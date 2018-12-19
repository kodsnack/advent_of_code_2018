(ns adventofcode.2018.day16
  (:require clojure.string))

(defn parse-registers [line] (read-string line))
(defn parse-inst [line]
  (as-> line $
    (read-string (str "[" $ "]"))
    {:op ($ 0)
     :A ($ 1)
     :B ($ 2)
     :C ($ 3)
     }
    ))

(defn parse-samples [lines]
  (->> lines
       (map clojure.string/trim)
       (filter (comp not empty?))
       (partition-by #(clojure.string/starts-with? % "Before"))
       (partition 2)
       (map #(apply concat %))
       (map #(take 3 %))
       (map (fn [[before-line inst-line after-line]]
              {:before (parse-registers (second (clojure.string/split before-line #":\s*")))
               :inst (parse-inst inst-line)
               :after (parse-registers (second (clojure.string/split after-line #":\s*")))
               }
              ))
       ))

(defn oprr [f]
  (fn [registers {:keys [A B C]}]
    (assoc registers C (f (registers A) (registers B)))
    ))

(defn opri [f]
  (fn [registers {:keys [A B C]}]
    (assoc registers C (f (registers A) B))
    ))

(defn opir [f]
  (fn [registers {:keys [A B C]}]
    (assoc registers C (f A (registers B)))
    ))

(defn boolf [f] (comp {true 1 false 0} f))

(def ops {
          :addr (oprr +)
          :addi (opri +)
          :mulr (oprr *)
          :muli (opri *)
          :banr (oprr bit-and)
          :bani (opri bit-and)
          :borr (oprr bit-or)
          :bori (opri bit-or)
          :setr (oprr (fn [A B] A))
          :seti (fn [registers inst] (assoc registers (:C inst) (:A inst)))
          :gtir (opir (boolf >))
          :gtri (opri (boolf >))
          :gtrr (oprr (boolf >))
          :eqir (opir (boolf =))
          :eqri (opri (boolf =))
          :eqrr (oprr (boolf =))
          })

(defn compare-sample [{:keys [before inst after]}]
  (->> ops
       (vals)
       (map (fn [op]
              (op before inst)))
       (filter #(= % after))
       (count)
       ))

(defn solve-a [lines]
  (->> lines
       (parse-samples)
       (map compare-sample)
       (filter #(>= % 3))
       (count)
       ))

(defn solve-b [result]
  ())

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   })

(defn day-lines [] (adventofcode.2018.core/day-lines 16))
