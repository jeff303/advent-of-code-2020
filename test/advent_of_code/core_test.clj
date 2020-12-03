(ns advent-of-code.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code.day1 :as day1]
            [advent-of-code.day2 :as day2]))

(deftest day1-test
  (testing "day 1 solutions"
    (is (= 898299 (day1/day1-part1)))
    (is (= 143933922 (day1/day1-part2)))))

(deftest day2-test
  (testing "day 2 solutions"
    (is (= 591 (day2/day2-part1)))
    (is (= 335 (day2/day2-part2)))))
