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
       (map-indexed (fn [i line] (format "%3d %s" i (clojure.string/join line))))
       (clojure.string/join \newline)
       ))

(defn print-state [state]
  (println (format-state state))
  (doseq [cart (:carts state)] (println cart))
  )

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

(defn get-crashes [carts]
  (->> carts
       (reduce
        (fn [pos-counts cart]
          (update pos-counts (:pos cart) (fn [cnt]
                                           (inc (or cnt 0)))))
        {}
        )
       (keep (fn [[pos count]]
               (if (> count 1)
                 pos
                 nil)))
       (set)
       ))

(defn update-state-remove-crashes-step [state [cart & carts]]
  (if (nil? cart)
    state
    (let [updated-state (update state :carts (fn [carts]
                                               (conj carts (update-cart (:map state) cart))))
          crashes (get-crashes (concat (:carts updated-state) carts))
          ]
      (recur (update updated-state :carts (fn [carts] (filter #(not (crashes (:pos %))) carts)))
             (filter #(not (crashes (:pos %))) carts)
             )
      )))

(defn update-state-remove-crashes [state]
  (update-state-remove-crashes-step
   (assoc state :carts [])
   (sort-by :pos (:carts state))))

(defn find-end-state [state]
  (if (= 1 (count (:carts state)))
    state
    nil))

(defn flip [[y x]] [x y])

(defn solve-a [lines]
  (->> lines
       (parse-state)
       (iterate update-state)
       (some (comp seq get-crashes :carts))
       (first)
       (flip)
       (clojure.string/join ",")
       ))

(defn solve-b [lines]
  (->> lines
       (parse-state)
       (iterate update-state-remove-crashes)
       (some find-end-state)
       (:carts)
       (first)
       (:pos)
       (flip)
       (clojure.string/join ",")
       ))

(defn run [input-lines & args]
  { :A (solve-a input-lines)
    :B (solve-b input-lines)
    }
)
