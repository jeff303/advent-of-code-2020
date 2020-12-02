(ns advent-of-code.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code.day1 :as day1]))

(deftest day1-test
  (testing "day 1 solution"
    (is (= 898299 (day1/day1-part1)))
    (is (= 143933922 (day1/day1-part2)))))
