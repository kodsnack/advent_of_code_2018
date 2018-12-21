(ns adventofcode.2018.day21
  (:require clojure.string
            [adventofcode.2018.util :refer [as->>]]
            ))

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

(defn initial-state [program a]
  {:program program
   :registers [a 0 0 0 0 0]
   :ip 0
   :count 0
   :register-3-values #{}
   :register-3-values-vec []
   })

(defn step [{:keys [program] :as state}]
  (let [ip (:ip state)
        inst ((:instructions program) ip)
        ]
    (-> state
        (assoc-in [:registers (:ip program)] ip)
        (update :registers #((ops (:op inst)) % inst))
        (as-> $ (assoc $ :ip ((:registers $) (:ip program))))
        (update :ip inc)
        (update :count inc)
        )))

(defn halted? [state]
  (not (contains? (:instructions (:program state)) (:ip state))))

(defn run-program [state]
  (if (halted? state)
    state
    (recur (step state))
    ))

(defn solve-a [lines]
  (-> lines
       (parse-program)
       (update :instructions #(apply vector (drop-last 3 %)))
       (initial-state 0)
       (run-program)
       (:registers)
       (as-> $ ($ 3))
       ))

(defn run-one-cycle [state]
  (let [state' (step state)]
    (if (= 28 (:ip state'))
      (-> state'
          (update :register-3-values #(conj % (get-in state' [:registers 3])))
          (update :register-3-values-vec #(conj % (get-in state' [:registers 3])))
          )
      (recur state')
    )))

(defn solve-b [lines]
  (->> lines
      (parse-program)
      (as->> $ (initial-state $ 0))
      (iterate run-one-cycle)
      (map-indexed vector)
      (filter (fn [[cycle state]]
                (println cycle (count (:register-3-values state)) (get-in state [:registers 3]))
                (< (count (:register-3-values state)) cycle)
                ))
      (first)
      ;; (as->> $ (get-in $ [:registers 3]))
      ))



(defn solve-b-2 []
  (loop [d 0
         exit-conditions {}
         d-values []
         loopnum 0
         ]
    (let [end-d (loop [f (bit-or d 0x10000)
                       d 0xe55233
                       ]
                  (if (>= f 256)
                    (let [d (+ d (mod f 256))
                          d (bit-and d 0xffffff)
                          d (* d 65899)
                          d (bit-and d 0xffffff)
                          f (int (/ f 256))
                          ]
                      (recur f d))
                    d
                    ))
          ]
      (if (contains? exit-conditions end-d)
        [exit-conditions d-values]
        (recur end-d (assoc exit-conditions end-d loopnum) (conj d-values end-d) (inc loopnum))
        ))))

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B nil
   }
  nil
  )

(defn format-state [state]
  (->> state
       (:program)
       (:instructions)
       (map-indexed (fn [i inst]
                      (if (= i (:ip state))
                        (str i " " inst " <--")
                        (str i " " inst)
                        )))
       (clojure.string/join \newline)
       (format "Count: %d\t ip: %d\n%s\n%s\n%s" (:count state) (:ip state) (:registers state) (:register-3-values state))
       ))

(def example-input "#ip 0
seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5")
(defn day-lines [] (adventofcode.2018.core/day-lines 21))
(def states [])
(defn show-state [] (println (format-state (last states))))
(defn start [lines] (def states [(initial-state (parse-program lines) 0)]) (show-state))
(defn start-day-lines [] (start (day-lines)))
(defn n []
  (def states (conj states (step (last states))))
  (show-state))
(defn p []
  (def states (pop states))
  (show-state))
(defn continue []
  (if (not (halted? (last states)))
    (do
      (n)
      (recur)
      )))
