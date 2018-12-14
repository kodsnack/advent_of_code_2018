(ns adventofcode.2018.day14
  (:require clojure.string))

(defn new-recipes [score1 score2]
  (let [scoresum (+ score1 score2)]
    (if (> scoresum 9)
      [1 (- scoresum 10)]
      [scoresum]
      )))

(defn expand-scoreboard [{scoreboard :board elfi1 :elfi1 elfi2 :elfi2}]
  (let [new (new-recipes (scoreboard elfi1) (scoreboard elfi2))]
    {:board (into scoreboard new)
     :elfi1 elfi1
     :elfi2 elfi2
     }
    ))

(defn move-elves [{scoreboard :board elfi1 :elfi1 elfi2 :elfi2}]
  (let [new-elfi1 (mod (+ elfi1 (inc (scoreboard elfi1))) (count scoreboard))
        new-elfi2 (mod (+ elfi2 (inc (scoreboard elfi2))) (count scoreboard))
        ]
    {:board scoreboard
     :elfi1 new-elfi1
     :elfi2 new-elfi2
     }
  ))

(def step (comp move-elves expand-scoreboard))

(defn solve-a [num-steps]
  (->> {:board [3 7] :elfi1 0 :elfi2 1}
       (iterate step)
       (drop-while (fn [{scoreboard :board}]
                     (< (count scoreboard) (+ 10 num-steps))))
       (first)
       (:board)
       (drop num-steps)
       (take 10)
       (clojure.string/join)
   ))

(defn solve-b [target-seq]
  (let [target-length (count target-seq)
        suffix-length (inc target-length)
        equals-target #(= target-seq %)
        contains-target (fn [{scoreboard :board}]
                          (as-> scoreboard $
                               (subvec $ (- (count $) suffix-length))
                               (partition target-length 1 $)
                               (some equals-target $)
                               ))
        ]
    (->> {:board [3 7] :elfi1 0 :elfi2 1}
         (iterate step)
         (drop-while #(< (count (:board %)) suffix-length))
         (filter contains-target)
         (first)
         (:board)
         ((fn [board]
            (if (= target-seq (subvec board (- (count board) suffix-length) (dec (count board))))
              (- (count board) suffix-length)
              (- (count board) target-length)
              )))
         )))

(defn run [input-lines & args]
  {:A (solve-a (read-string (first input-lines)))
   :B (solve-b (map #(read-string (str %)) (first input-lines)))
   })
