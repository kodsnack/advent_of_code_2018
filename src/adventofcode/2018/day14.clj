(ns adventofcode.2018.day14
  (:require clojure.string))

(defn new-recipes [score1 score2]
  (let [scoresum (+ score1 score2)
        str-digits (str scoresum)
        digits (mapv (comp read-string str) str-digits)
        ]
    digits
    ))

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
  (let [cellsize 1000000]
  (->> {:board [3 7] :elfi1 0 :elfi2 1}
       (iterate step)
       (partition 1 cellsize)
       (map first)
       (map-indexed vector)
       (drop-while (fn [[cellnum {scoreboard :board i1 :elfi1 i2 :elfi2}]]
                     (->> (drop (* cellnum cellsize) scoreboard)
                          (partition (count target-seq) 1)
                          (some #(= target-seq %))
                          (not)
                          )))
       (first)
       (second)
       (:board)
       ((fn [scoreboard]
         (->> scoreboard
              (map-indexed vector)
              (partition (count target-seq) 1)
              (filter #(= target-seq (map second %)))
              (first)
              (first)
              (first)
              )))
       )))

(defn run [input-lines & args]
  {:A (solve-a (read-string (first input-lines)))
   :B (solve-b (map #(read-string (str %)) (first input-lines)))
   })
