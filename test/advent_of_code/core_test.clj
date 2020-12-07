(ns advent-of-code.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code.util :as util]
            [advent-of-code.day1 :as day1]
            [advent-of-code.day2 :as day2]
            [advent-of-code.day3 :as day3]
            [advent-of-code.day4 :as day4]
            [advent-of-code.day5 :as day5]
            [advent-of-code.day6 :as day6]))

(deftest day1-test
  (testing "day 1 solutions"
    (is (= 898299 (day1/day1-part1)))
    (is (= 143933922 (day1/day1-part2)))))

(deftest day2-test
  (testing "day 2 solutions"
    (is (= 591 (day2/day2-part1)))
    (is (= 335 (day2/day2-part2)))))

(deftest day3-test
  (testing "day 3 solutions"
    (is (= 151 (day3/day3-part1)))
    (is (= 7540141059 (day3/day3-part2)))))

(deftest day4-test
  (testing "day 4 solutions"
    (is (= 200 (day4/day4-part1)))
    (is (= 116 (day4/day4-part2)))))

(deftest day5-test
  (testing "day 5 functions"
    (is (= 567 (day5/encoded-line-to-seat-id "BFFFBBFRRR")))
    (is (= 119 (day5/encoded-line-to-seat-id "FFFBBBFRRR")))
    (is (= 820 (day5/encoded-line-to-seat-id "BBFFBBFRLL"))))
  (testing "day 5 solutions"
    (is (= 996 (day5/day5-part1)))
    (is (= 671 (day5/day5-part2)))))

(deftest day6-test
  (testing "day 6 functions"
    (let [test-input-groups (util/read-problem-input-split-by "test_day6_input" #"\n\n")
          counts-by-group (map day6/count-yes-answers test-input-groups)
          total-count (reduce + counts-by-group)]
      (is (= 11 total-count))))
  (testing "day 6 solutions"
    (is (= 6778 (day6/day6-part1)))))
