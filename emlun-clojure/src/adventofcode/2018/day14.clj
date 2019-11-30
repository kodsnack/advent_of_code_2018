(ns adventofcode.2018.day14
  (:require clojure.string
            [adventofcode.2018.util :refer [as->>]]
            ))

(defn new-recipes [score1 score2]
  (let [scoresum (+ score1 score2)]
    (if (> scoresum 9)
      [1 (- scoresum 10)]
      [scoresum]
      )))

(defn expand-scoreboard [{:keys [board elfi1 elfi2]}]
  {:board (into board (new-recipes (board elfi1) (board elfi2)))
   :elfi1 elfi1
   :elfi2 elfi2
   })

(defn move-elves [{:keys [board elfi1 elfi2]}]
  {:board board
   :elfi1 (mod (+ elfi1 (inc (board elfi1))) (count board))
   :elfi2 (mod (+ elfi2 (inc (board elfi2))) (count board))
   })

(def step (comp move-elves expand-scoreboard))

(defn solve-a [num-steps]
  (->> {:board [3 7] :elfi1 0 :elfi2 1}
       (iterate step)
       (drop-while (fn [{scoreboard :board}]
                     (< (count scoreboard) (+ 10 num-steps))))
       (first)
       (:board)
       (as->> $ (subvec $ num-steps (+ num-steps 10)))
       (clojure.string/join)
   ))

(defn solve-b [target-seq]
  (let [target-length (count target-seq)
        suffix-length (inc target-length)
        find-target (fn [{scoreboard :board}]
                      (let [suffix (subvec scoreboard (- (count scoreboard) suffix-length))]
                        (or
                         (when (= target-seq (subvec suffix 0 target-length))
                           (- (count scoreboard) suffix-length))
                         (when (= target-seq (subvec suffix 1))
                           (- (count scoreboard) target-length))
                         )))
        ]
    (->> {:board [3 7] :elfi1 0 :elfi2 1}
         (iterate step)
         (drop-while #(< (count (:board %)) suffix-length))
         (keep find-target)
         (first)
         )))

(defn run [input-lines & args]
  {:A (solve-a (read-string (first input-lines)))
   :B (solve-b (map #(read-string (str %)) (first input-lines)))
   })
