(ns adventofcode.2018.day12
  (:require clojure.string))

(defn string-to-bools [state-string]
  (mapv #(= \# %) state-string))

(defn bools-to-set [first-index state-vec]
  (->> state-vec
       (map-indexed vector)
       (filter second)
       (map first)
       (map #(+ first-index %))
       (set)
       )
  )

(defn set-to-bools [state-set]
  (let [mini (apply min state-set)
        maxi (apply max state-set)
        ]
    [mini
     (->> (range mini (inc maxi))
          (mapv #(contains? state-set %))
          )
     ]
     ))

(defn parse-input [lines]
  {
   :init (as-> lines $
          (first $)
          (clojure.string/split $ #": ")
          (last $)
          (string-to-bools $)
          (bools-to-set 0 $)
          )
   :updates (as-> lines $
              (drop 2 $)
              (map #(clojure.string/split % #"\s*=>\s*") $)
              (reduce (fn [result [from to]] (assoc result (string-to-bools from) (= "#" to))) {} $)
              )
   }
  )

(defn update-state [updates state-set]
  (let [[start-i state-vec] (set-to-bools state-set)
        updated-vec (->> (concat [false false false false] state-vec [false false false false])
                         (partition 5 1)
                         (map updates)
                         )
        new-start-i (- start-i 2)
        ]
    (bools-to-set new-start-i updated-vec)
    ))

(defn format-state-vec [state-vec]
  (clojure.string/join (map #(if % \# \.) state-vec)))

(defn format-state [state-set]
  (let [
        [start-i state-vec] (set-to-bools state-set)
        ]
    [start-i
     (format-state-vec state-vec)
     ]
    ))

(defn print-states [state-sets]
  (let [formatted-states (map format-state state-sets)
        min-start-index (apply min (map first formatted-states))
        ]
    (do
      (println (str "    " (apply str (repeat (- min-start-index) " ")) "          1         2         3"))
      (println (str "    " (apply str (repeat (- min-start-index) " ")) "0         0         0         0"))
      (doseq [[n [start-i state-str]] (map-indexed vector formatted-states)
              :let [pad-length (- start-i min-start-index)
                    padding (apply str (repeat pad-length \.))
                    ]
              ]
        (println (clojure.core/format "%2d: %s%s" n padding state-str))
        ))))

(defn run-state-updates [updates steps history state-set]
  (if (= steps 0)
    state-set
    (let [new-state (update-state updates state-set)
          [start-i state-vec] (set-to-bools state-set)
          ]
      (if-let [[match-index diff] (first
                                   (keep-indexed
                                    (fn [i [hist-start-i hist-state]]
                                      (if (= state-vec hist-state)
                                        [i (- start-i hist-start-i)]
                                        nil))
                                    history
                                    ))
               ]
        (let [loop-length (inc match-index)
              num-loops (biginteger (/ steps loop-length))
              steps-left (mod steps loop-length)
              pot-diff (* diff num-loops)
              ]
          (recur updates steps-left () (set (map #(+ pot-diff %) state-set)))
          )
        (recur updates (dec steps) (cons [start-i state-vec] history) new-state)
        ))
    )
  )

(defn solve [lines steps]
  (let [{init :init updates :updates} (parse-input lines)]
    (->> init
         (run-state-updates updates steps ())
         (reduce +)
         )
    ))

(defn solve-a [lines] (solve lines 20))
(defn solve-b [lines] (solve lines 50000000000))

(defn run [input-lines & args]
  { :A (solve-a input-lines)
    :B (solve-b input-lines)
    }
)
