(ns adventofcode.2018.day17
  (:require clojure.string clojure.pprint))

(def example-input "x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504")

(defn parse-world [lines]
  (->> lines
       (mapcat (fn [line]
                 (let [[_ minx _ maxx] (re-matches #".*x=(\d+)(\.\.(\d+))?.*" line)
                       [_ miny _ maxy] (re-matches #".*y=(\d+)(\.\.(\d+))?.*" line)
                       minx (read-string minx)
                       miny (read-string miny)
                       ]
                   (for [x (range minx (inc (or (some-> maxx read-string) minx)))
                         y (range miny (inc (or (some-> maxy read-string) miny)))
                         ]
                     (do
                     [x y])
                     ))))
       (set)
       ))

(defn start [world]
  (let [miny (apply min (map second world))]
    {:world world
     :falls [[500 (max 0 miny)]]
     :settled #{}
     :visited #{[500 0]}
     :min-y miny
     :max-y (apply max (map second world))
     })
    )

(defn below [[x y]] [x (inc y)])

(defn blocked? [{:keys [world settled]} pos]
  (or (world pos) (settled pos)))

(defn fallable? [state [_ y :as pos]]
  (not (or (blocked? state pos)
           ((:visited state) pos)
           (> y (:max-y state))
           )))

(defn fall [state [x y :as pos]]
  (-> state
      (update :falls #(conj % pos))
      (update :visited #(conj % pos))
      ))

(defn move [state f [x y]]
  (let [safe-xs (->> x
                     (iterate f)
                     (take-while #(not (blocked? state [% y])))
                     (take-while #(blocked? state (below [% y]))))
        ]
    (if-let [last-safe-x (last safe-xs)]
      (let [first-unsafe-x (f last-safe-x)]
        (if (blocked? state [first-unsafe-x y])
          [safe-xs nil]
          [safe-xs first-unsafe-x]
          ))
      [() nil]
      )))

(defn move-left [state pos] (move state dec pos))
(defn move-right [state pos] (move state inc pos))

(defn add-xs [xys y new-xs]
  (apply conj xys (map #(vector % y) new-xs)))

(defn sweep [state [_ y :as pos]]
  (let [[left-safe-xs left-unsafe-x] (move-left state pos)
        [right-safe-xs right-unsafe-x] (move-right state pos)
        settling (not (or left-unsafe-x right-unsafe-x))
        set-to-add-to (if settling :settled :visited)
        ]
    (cond-> state
      true (update set-to-add-to #(add-xs % y (concat left-safe-xs right-safe-xs)))
      left-unsafe-x (update :falls #(conj % [left-unsafe-x y]))
      left-unsafe-x (update :visited #(conj % [left-unsafe-x y]))
      right-unsafe-x (update :falls #(conj % [right-unsafe-x y]))
      right-unsafe-x (update :visited #(conj % [right-unsafe-x y]))
      )))

(defn finished? [state] (empty? (:falls state)))

(defn step [state]
  (let [last-fall (last (:falls state))
        next-tile (below last-fall)
        ]
    (if (fallable? state next-tile)
      (fall state next-tile)
      (-> state
          (update :falls pop)
          (sweep last-fall)
          )
      )))

(defn count-tiles [{:keys [visited settled]}]
  (->> (concat visited settled)
       (distinct)
       (count)
   ))

(defn format-state [{:keys [falls settled visited world]}]
  (let [[x y] (last falls)
        xs [x]
        ys [y]
        ]
    (->> (range (- (apply min ys) 20) (+ 20 (inc (apply max ys))))
         (map (fn [y]
                (->> (range (- (apply min xs) 40) (+ 40 (inc (apply max xs))))
                     (map (fn [x]
                            (cond
                              (= (falls (dec (count falls))) [x y]) \@
                              (settled [x y]) \~
                              (visited [x y]) \|
                              (world [x y]) \#
                              :else \.
                              )))
                     (clojure.string/join)
                     )))
         (clojure.string/join \newline)
         ((fn [s] (clojure.string/join \newline [s (format "x=%d y=%d falls=%d visited=%d settled=%d" x y (count falls) (count visited) (count settled))])))
         )))

(defn solve-a [lines]
  (->> lines
       (parse-world)
       (start)
       (iterate step)
       (filter finished?)
       (first)
       (count-tiles)
       ))

(defn solve-b [lines] ())

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   }
  )

(defn day-lines [] (adventofcode.2018.core/day-lines 17))
(def states [])
(defn show-state [] (println (str \newline (format-state (last states)))))
(defn start-day-lines [] (def states [(start (parse-world (day-lines)))]) (show-state))
(defn start-example [] (def states [(start (parse-world (clojure.string/split-lines example-input)))]) (show-state))
(defn n []
  (def states (conj states (step (last states))))
  (show-state))
(defn p []
  (def states (pop states))
  (show-state))
(defn animate
  ([] (animate 100))
  ([dt]
   (while true
     (n)
     (Thread/sleep dt)
     )))
