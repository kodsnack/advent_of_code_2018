(ns adventofcode.2018.core-test
  (:require [clojure.test :refer :all]
            [adventofcode.2018.core :refer :all]))

(defn matches [match matchee]
  (every?
    #(= (match %) (matchee %))
    (keys match)
  )
)

(def solutions {
  1  { :A 416, :B 56752 }
  2  { :A 7657, :B "ivjhcadokeltwgsfsmqwrbnuy" }
  3  { :A nil, :B nil }
  4  { :A nil, :B nil }
  5  { :A nil, :B nil }
  6  { :A nil, :B nil }
  7  { :A nil, :B nil }
  8  { :A nil, :B nil }
  9  { :A nil, :B nil }
  10 { :A nil, :B nil }
  11 { :A nil, :B nil }
  12 { :A nil, :B nil }
  13 { :A nil, :B nil }
  14 { :A nil, :B nil }
  15 { :A nil, :B nil }
  16 { :A nil, :B nil }
  17 { :A nil, :B nil }
  18 { :A nil, :B nil }
  19 { :A nil, :B nil }
  20 { :A nil, :B nil }
  21 { :A nil, :B nil }
  22 { :A nil, :B nil }
  23 { :A nil, :B nil }
  24 { :A nil, :B nil }
  25 { :A nil, :B nil }
})

(deftest test-all-days
  (doseq [day (take 25 (iterate inc 1))]
    (let
      [
        result (run-day-with-file day)
        correct (solutions day)
      ]

      (if (not (nil? result))
        (testing (str "Day " day)
          (is (= (:A correct) (:A result)))
          (is (= (:B correct) (:B result)))
        )
      )
    )
  )
)
