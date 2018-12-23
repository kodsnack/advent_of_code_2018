(ns adventofcode.2018.day19
  (:require clojure.string))

(defn parse-registers [line] (read-string line))
(defn parse-inst [line]
  (as-> line $
    (read-string (str "[:" $ "]"))
    {:op ($ 0)
     :A ($ 1)
     :B ($ 2)
     :C ($ 3)
     }
    ))

(defn parse-program [[ip-decl & lines]]
  {:ip (read-string (second (clojure.string/split ip-decl #"\s+")))
   :instructions (mapv parse-inst lines)
   })

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
          :setr (opri (fn [A B] A))
          :seti (fn [registers inst] (assoc registers (:C inst) (:A inst)))
          :gtir (opir (boolf >))
          :gtri (opri (boolf >))
          :gtrr (oprr (boolf >))
          :eqir (opir (boolf =))
          :eqri (opri (boolf =))
          :eqrr (oprr (boolf =))
          })

(defn initial-state []
  {:registers [0 0 0 0 0 0]
   :ip 0
   })

(defn step [program state]
  (let [ip (:ip state)
        inst ((:instructions program) ip)
        ]
    (-> state
        (assoc-in [:registers (:ip program)] ip)
        (update :registers #((ops (:op inst)) % inst))
        (as-> $ (assoc $ :ip ((:registers $) (:ip program))))
        (update :ip inc)
        )))

(defn halted? [program state]
  (not (contains? (:instructions program) (:ip state))))

(defn run-program [program state]
  (if (halted? program state)
    state
    (recur program (step program state))
    ))

(defn divisors [f]
  (->> (range 1 (inc f))
       (keep (fn [b]
               (if (= 0 (mod f b)) b nil)))
       (apply +)
       ))

(defn solve-reverse-engineered [initial-a lines]
  (-> lines
      (parse-program)
      (as-> $ (iterate (partial step $) (assoc-in (initial-state) [:registers 0] initial-a)))
      (as-> $ (filter (fn ([state]
                           (= 1 (:ip state))))
                      $))
      (first)
      (:registers)
      (as-> $ (apply max $))
      (divisors)
  ))

(defn solve-a [lines]
  (solve-reverse-engineered 0 lines))

(defn solve-b [lines]
  (solve-reverse-engineered 1 lines))

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B nil
   }
  nil
  )

(defn format-program [program]
  (->> program
       (:instructions)
       (map-indexed (fn [i inst]
                      (str i " " inst)
                      ))
       (clojure.string/join \newline)
       ))

(defn format-state [program state]
  (->> program
       (format-program)
       (clojure.string/split-lines)
       (map-indexed (fn [i line]
                      (if (= i (:ip state))
                        (str line " <--")
                        (str line)
                        )))
       (clojure.string/join \newline)
       (str state \newline)
       ))

(def example-input "#ip 0
seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5")
(defn day-lines [] (adventofcode.2018.core/day-lines 19))
(def states [])
(def program nil)
(defn show-state [] (println (format-state program (last states))))
(defn start [lines] (def program (parse-program lines)) (def states [(initial-state)]) (show-state))
(defn start-day-lines [] (start (day-lines)))
(defn start-example [] (start (clojure.string/split-lines example-input)))
(defn n []
  (def states (conj states (step program (last states))))
  (show-state))
(defn p []
  (def states (pop states))
  (show-state))
