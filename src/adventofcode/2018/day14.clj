(ns adventofcode.2018.day14
  (:require clojure.string))

(defn new-recipes [score1 score2]
  (let [scoresum (+ score1 score2)]
    (if (> scoresum 9)
      [1 (- scoresum 10)]
      [scoresum]
      )))

(defn expand-scoreboard [{scoreboard :board elfi1 :elfi1 elfi2 :elfi2}]
  {:board (into scoreboard (new-recipes (scoreboard elfi1) (scoreboard elfi2)))
   :elfi1 elfi1
   :elfi2 elfi2
   })

(defn move-elves [{scoreboard :board elfi1 :elfi1 elfi2 :elfi2}]
  {:board scoreboard
   :elfi1 (mod (+ elfi1 (inc (scoreboard elfi1))) (count scoreboard))
   :elfi2 (mod (+ elfi2 (inc (scoreboard elfi2))) (count scoreboard))
   })

(def step (comp move-elves expand-scoreboard))

(defn solve-a [num-steps]
  (as-> {:board [3 7] :elfi1 0 :elfi2 1} $
       (iterate step $)
       (drop-while (fn [{scoreboard :board}]
                     (< (count scoreboard) (+ 10 num-steps)))
                   $)
       (first $)
       (:board $)
       (subvec $ num-steps (+ num-steps 10))
       (clojure.string/join $)
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
