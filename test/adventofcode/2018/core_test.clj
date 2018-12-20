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
  3  { :A 113716, :B 742 }
  4  { :A 102688, :B 56901 }
  5  { :A 11108, :B 5094 }
  6  { :A nil, :B nil }
  7  { :A nil, :B nil }
  8  { :A nil, :B nil }
  9  { :A nil, :B nil }
  10 { :A nil, :B nil }
  11 { :A "20,68", :B "231,273,16" }
  12 { :A 1991, :B 1100000000511 }
  13 { :A "26,99", :B "62,48" }
  14 { :A "1150511382", :B 20173656 }
  15 { :A 227290, :B 53725 }
  16 { :A 580, :B 537 }
  17 { :A 35707, :B 29293 }
  18 { :A 646437, :B 208080 }
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
