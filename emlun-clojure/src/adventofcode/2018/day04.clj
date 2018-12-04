(ns adventofcode.2018.day04)

(def guard-pattern #"\[(\d{4}-\d{2}-\d{2}) (\d{2}:\d{2})\] Guard \#(\d+) begins shift")
(def sleep-pattern #"\[(\d{4}-\d{2}-\d{2}) (\d{2}):(\d{2})\] falls asleep")
(def wake-pattern  #"\[(\d{4}-\d{2}-\d{2}) (\d{2}):(\d{2})\] wakes up")

(defn read-num [s] (read-string (if (= \0 (first s)) (apply str (rest s)) s)))

(defn parse-event [line]
  (let [guard-matches (re-matches guard-pattern line)
        sleep-matches (re-matches sleep-pattern line)
        wake-matches (re-matches wake-pattern line)
        ]
    (if-let [[_ date time id] guard-matches]
      {:type :guard, :date date, :time time, :id id}

      (if-let [[_ date h m] sleep-matches]
        {:type :sleep, :date date, :time (str h ":" m), :hour (read-num h), :minute (read-num m)}

        (if-let [[_ date h m] wake-matches]
          {:type :wake, :date date, :time (str h ":" m), :hour (read-num h), :minute (read-num m)}
        )
    ))))

(defn timestamp [event] (str (:date event) " " (:time event)))

(defn sort-events [events] (sort-by timestamp events))

(defn add-sleep [guard-map start-event end-event]
  (reduce
    (fn [guard-map minute]
      (update-in guard-map [:sleep-minutes minute] #(inc (or % 0)))
    )
    guard-map
    (take-while #(< % (:minute end-event)) (iterate inc (:minute start-event)))
  ))

(defn all-sleeps [sorted-events]
  (loop [[event & tail] sorted-events
         guard-maps {}
         current-guard nil
         prev-event nil
         ]
    (if (nil? event)
      guard-maps
      (case (:type event)
        :guard
          (if (= :sleep (:type prev-event))
            (recur tail (update guard-maps current-guard #(add-sleep % prev-event event)) (:id event) event)
            (recur tail guard-maps (:id event) event)
            )
        :sleep (recur tail guard-maps current-guard event)
        :wake (recur tail (update guard-maps current-guard #(add-sleep % prev-event event)) current-guard event)
      )
    )
  )
)

(defn solve-a [lines]
  (let [events (map parse-event lines)
        sorted-events (sort-events events)
        guard-maps (all-sleeps sorted-events)
        sleep-tots (reduce (fn [tots [id guard-map]] (assoc tots id (reduce + (vals (:sleep-minutes guard-map))))) {} guard-maps)
        [sleepiest-id {minutes :sleep-minutes}] (apply max-key #(sleep-tots (first %)) guard-maps)
        sleepiest-minute (apply max-key second minutes)
        ]
    (* (read-string sleepiest-id) (first sleepiest-minute))
  ))

(defn solve-b [lines]
  (let [events (map parse-event lines)
        sorted-events (sort-events events)
        guard-maps (all-sleeps sorted-events)
        [sleepiest-id {minutes :sleep-minutes}] (apply max-key #(apply max (vals (:sleep-minutes (second %)))) guard-maps)
        sleepiest-minute (apply max-key second minutes)
        ]
    (* (read-string sleepiest-id) (first sleepiest-minute))
  ))

(defn run [input-lines & args]
  { :A (solve-a input-lines)
    :B (solve-b input-lines)
  }
)
