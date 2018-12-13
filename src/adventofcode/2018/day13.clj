(ns adventofcode.2018.day13
  (:require clojure.string))

(defn append-cell [state ch]
  (update state :map (fn [m]
                       (update m (dec (count m)) (fn [r]
                                                   (conj r ch)))))
  )

(defn append-cart [state ch]
  (as-> state $
    (update $ :carts (fn [carts]
                       (conj carts {:dir ch
                                    :pos [(dec (count (:map state))) (count (last (:map state)))]
                                    :turn :left
                                    })
                       ))
    (append-cell $ ({\> \-, \< \-, \^ \|, \v \|} ch))
    ))

(defn parse-state [lines]
  (reduce
   (fn [state line]
     (reduce
      (fn [state ch]
        (case ch
          \. (append-cell state ch)
          \- (append-cell state ch)
          \+ (append-cell state ch)
          \| (append-cell state ch)
          \\ (append-cell state ch)
          \/ (append-cell state ch)
          \> (append-cart state ch)
          \< (append-cart state ch)
          \^ (append-cart state ch)
          \v (append-cart state ch)
          \space (append-cell state ch)
          )
        )
      (update state :map (fn [m] (conj m [])))
      line
      )
     )
   {
    :carts []
    :map []
    }
   lines
   ))

(defn format-state [state]
  (->> (reduce
        (fn [lines {[y x] :pos dir :dir}]
          (update-in lines [y x] (fn [_] dir))
          )
        (:map state)
        (:carts state)
        )
       (map (fn [line] (clojure.string/join line)))
       (clojure.string/join \newline)
       ))

(defn print-state [state]
  (println (format-state state)))

(defn next-dir [dir turn]
  (
   (case turn
     :left {\v \>
            \> \^
            \^ \<
            \< \v
            }
     :straight identity
     :right {\v \<
             \> \v
             \^ \>
             \< \^
             }
     ) dir)
  )

(defn turn-dir [dir turn]
  (next-dir dir
            (case turn
              \\ (if (#{\^ \v} dir) :left :right)
              \/ (if (#{\< \>} dir) :left :right)
            )))

(defn vec-add [& vectors] (apply mapv + vectors))

(defn dir-to-dydx [dir]
  ({\v [1 0]
    \> [0 1]
    \^ [-1 0]
    \< [0 -1]
    } dir))

(defn update-pos [map cart]
  (update cart :pos (fn [pos]
                      (vec-add pos (dir-to-dydx (:dir cart))))))

(defn update-dir [map cart]
  (let [map-pos (get-in map (:pos cart))]
    (case map-pos
      \+ (assoc cart :dir (next-dir (:dir cart) (:turn cart)))
      \\ (assoc cart :dir (turn-dir (:dir cart) map-pos))
      \/ (assoc cart :dir (turn-dir (:dir cart) map-pos))
      cart
      )))

(defn update-turn [map cart]
  (if (= \+ (get-in map (:pos cart)))
    (update cart :turn
            {:left :straight
             :straight :right
             :right :left
             })
    cart
    ))

(defn update-cart [map cart]
  (->> cart
       (update-pos map)
       (update-dir map)
       (update-turn map)))

(defn update-state [state]
  (let [sorted-carts (sort-by :pos (:carts state))]
    (reduce (fn [state cart]
              (update state :carts (fn [carts]
                                     (conj carts (update-cart (:map state) cart)))))
            (assoc state :carts [])
            sorted-carts)
    )
  )

(defn find-crash [state]
  (->> (:carts state)
       (reduce
        (fn [pos-counts cart]
          (update pos-counts (:pos cart) (fn [cnt]
                                           (inc (or cnt 0)))))
        {}
        )
       (some (fn [[pos count]]
               (if (> count 1)
                 pos
                 nil)))
       ))

(defn flip [[y x]] [x y])

(defn solve-a [lines]
  (->> lines
       (parse-state)
       (iterate update-state)
       (some find-crash)
       (flip)
       (clojure.string/join ",")
       ))

(defn solve-b [lines] nil)

(defn run [input-lines & args]
  { :A (solve-a input-lines)
    :B (solve-b input-lines)
    }
)
