(ns adventofcode.2018.day04)

(def guard-pattern #"\[(\d{4}-\d{2}-\d{2} \d{2}:\d{2})\] Guard \#(\d+) begins shift")
(def sleep-pattern #"\[(\d{4}-\d{2}-\d{2} (\d{2}):(\d{2}))\] falls asleep")
(def wake-pattern  #"\[(\d{4}-\d{2}-\d{2} (\d{2}):(\d{2}))\] wakes up")

(defn read-num [s] (read-string (if (= \0 (first s)) (apply str (rest s)) s)))

(defn parse-event [line]
  (let [guard-matches (re-matches guard-pattern line)
        sleep-matches (re-matches sleep-pattern line)
        wake-matches (re-matches wake-pattern line)
        ]
    (if-let [[_ tstamp id] guard-matches]
      {:type :guard, :timestamp tstamp, :id id}

      (if-let [[_ tstamp h m] sleep-matches]
        {:type :sleep, :timestamp tstamp, :hour (read-num h), :minute (read-num m)}

        (if-let [[_ tstamp h m] wake-matches]
          {:type :wake, :timestamp tstamp, :hour (read-num h), :minute (read-num m)}
        )
    ))))

(defn add-sleep [guard-map start-event end-event]
  (reduce
    (fn [guard-map minute]
      (update-in guard-map [:sleep-minutes minute] #(inc (or % 0)))
    )
    guard-map
    (range (:minute start-event) (:minute end-event))
  ))

(defn all-sleeps [sorted-events]
  (first (reduce
    (fn [[guard-maps current-guard] [prev-event event]]
      (case (:type event)
        :guard [guard-maps (:id event)]
        :sleep [guard-maps current-guard]
        :wake [(update guard-maps current-guard #(add-sleep % prev-event event)) current-guard]
    ))
    [{} nil nil]
    (partition 2 1 sorted-events)
  )))

(defn strategy-a [sleep-minutes] (reduce + (vals sleep-minutes)))

(defn strategy-b [sleep-minutes] (apply max (vals sleep-minutes)))

(defn solve [guard-maps guard-strategy]
  (let [
        [sleepiest-id {minutes :sleep-minutes}] (apply max-key #(guard-strategy (:sleep-minutes (second %))) guard-maps)
        sleepiest-minute (apply max-key second minutes)
        ]
    (* (read-string sleepiest-id) (first sleepiest-minute))
  ))

(defn run [input-lines & args]
  (let [events (map parse-event input-lines)
        sorted-events (sort-by :timestamp events)
        guard-maps (all-sleeps sorted-events)
        ]
    { :A (solve guard-maps strategy-a)
      :B (solve guard-maps strategy-b)
    }
  )
)
