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

(defn parse-samples-and-program [lines]
  (let [[samples [last-sample]] (->> lines
                                   (map clojure.string/trim)
                                   (filter (comp not empty?))
                                   (partition-by #(clojure.string/starts-with? % "Before"))
                                   (partition 2)
                                   (map #(apply concat %))
                                   (split-with #(>= 3 (count %)))
                                   )
        [last-sample program] (split-at 3 last-sample)
        ]
    [
     (map (fn [[before-line inst-line after-line]]
            {:before (parse-registers (second (clojure.string/split before-line #":\s*")))
             :inst (parse-inst inst-line)
             :after (parse-registers (second (clojure.string/split after-line #":\s*")))
             }
            )
          (conj samples last-sample)
          )
     (map parse-inst program)
     ]))

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

(defn initial-hypotheses []
  (->> (range 0 16)
       (map (fn [i] [i (set (keys ops))]))
       (into {})
       ))

(defn reduce-hypotheses [hypotheses [sample & samples]]
  (if (nil? sample)
    hypotheses
    (let [op-results (map (fn [[sym op]]
                            {:sym sym
                             :result (op (:before sample) (:inst sample))
                             })
                          ops
                          )
          possible-ops (->> op-results
                            (filter #(= (:result %) (:after sample)))
                            (map :sym)
                            (set)
                            )
          updated-hypotheses (update hypotheses (:op (:inst sample))
                                     #(set (filter possible-ops %)))
          ]
      (recur updated-hypotheses samples)
      )))

(defn remove-hypothesis [[opcode opsym] hypotheses]
  (->> hypotheses
       (filter #(not= opcode (first %)))
       (map (fn [[oc syms]]
              [oc (disj syms opsym)]
              ))
       ))

(defn find-hypothesis-with-only-one-sym [hypotheses]
  (let [{found true other-hypotheses false} (group-by (comp #(= 1 %) count second) hypotheses)]
    (if found
      (let [[[opcode opsyms]] found]
        [opcode (first opsyms)]
        ))))

(defn conclude-opcodes [conclusion hypotheses]
  (if (empty? hypotheses)
    conclusion
    (let [[opcode opsym] (find-hypothesis-with-only-one-sym hypotheses)]
      (recur
       (assoc conclusion opcode opsym)
       (remove-hypothesis [opcode opsym] hypotheses)
       ))))

(defn initial-state []
  {:registers [0 0 0 0]
   })

(defn execute-instruction [state inst opcodes]
  (update state :registers #((ops (opcodes (:op inst))) % inst))
  )

(defn run-program [state [inst & program] opcodes]
  (if (nil? inst)
    state
    (recur (execute-instruction state inst opcodes) program opcodes)
    ))

(defn solve-a [lines]
  (->> lines
       (parse-samples-and-program)
       (first)
       (map compare-sample)
       (filter #(>= % 3))
       (count)
       ))

(defn solve-b [lines]
  (let [[samples program] (parse-samples-and-program lines)
        opcodes (->> samples
                     (reduce-hypotheses (initial-hypotheses))
                     (conclude-opcodes {})
                     )
        ]
    (->> (run-program (initial-state) program opcodes)
         (:registers)
         (first)
     )))

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   })

(defn day-lines [] (adventofcode.2018.core/day-lines 16))
