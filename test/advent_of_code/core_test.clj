(ns advent-of-code.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code.util :as util]
            [advent-of-code.day1 :as day1]
            [advent-of-code.day2 :as day2]
            [advent-of-code.day3 :as day3]
            [advent-of-code.day4 :as day4]
            [advent-of-code.day5 :as day5]
            [advent-of-code.day6 :as day6]
            [advent-of-code.day7 :as day7]
            [advent-of-code.day8 :as day8]
            [advent-of-code.day9 :as day9]
            [advent-of-code.day10 :as day10]
            [advent-of-code.day11 :as day11]
            [advent-of-code.day12 :as day12]
            [advent-of-code.day13 :as day13]
            [advent-of-code.day14 :as day14]))

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
    (let [test-input-groups (day6/parse-test-input-as-groups)
          counts-by-group (map day6/count-union-yes-answers test-input-groups)
          total-count (reduce + counts-by-group)]
      (is (= 11 total-count))))
  (testing "day 6 solutions"
    (is (= 6778 (day6/day6-part1)))
    (is (= 3406 (day6/day6-part2)))))

(deftest day7-test
  (testing "day 7 functions"
    (let [rule (day7/input-line-to-rule "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.")]
      (is (= {"vibrant plum" {"faded blue" 5 "dotted black" 6}} rule)))
    (let [rules (day7/parse-test-inputs-as-rule-map)
          vc #{}
          target-color "shiny gold"]
      (is (= {"bright white" {"shiny gold" 1}
              "dark olive" {"dotted black" 4 "faded blue" 3}
              "dark orange" {"bright white" 3 "muted yellow" 4}
              "dotted black" {}
              "faded blue" {}
              "light red" {"bright white" 1 "muted yellow" 2}
              "muted yellow" {"faded blue" 9 "shiny gold" 2}
              "shiny gold" {"dark olive" 1 "vibrant plum" 2}
              "vibrant plum" {"dotted black" 6 "faded blue" 5}} rules))
      (is (= true (day7/target-color-reachable rules vc "light red" target-color)))
      (is (= true (day7/target-color-reachable rules vc "bright white" target-color)))
      (is (nil? (day7/target-color-reachable rules vc "vibrant plum" target-color))))
    (let [rules-pt2 (day7/parse-test-inputs-as-rule-map "_part2")
          vc #{}]
      (is (= 126 (day7/count-total-reachable-bags rules-pt2 vc "shiny gold")))))
  (testing "day 7 solutions"
    (is (= 172 (day7/day7-part1)))
    (is (= 39645 (day7/day7-part2)))))

(deftest day8-test
  (testing "day 8 functions"
    (let [prog (into [] (day8/parse-test-inputs-to-program))
          prog-fixed (into [] (day8/parse-test-inputs-to-program "_fixed"))]
      (is (= [5 false] (day8/run-program prog 0 0 #{})))
      (is (= [8 true] (day8/run-program prog-fixed 0 0 #{})))
      (is (= 8 (day8/fix-program prog)))))
  (testing "day 8 solutions"
    (is (= 1179 (day8/day8-part1)))
    (is (= 1089 (day8/day8-part2)))))

(deftest day9-test
  (testing "day 9 functions"
    (let [nums (day9/parse-test-inputs-as-numbers)
          preamble-length 5
          nums-vec (into [] nums)]
      (is (= 127 (day9/find-first-invalid-number nums preamble-length)))
      (is (= 62 (day9/find-encryption-weakness nums-vec 127)))))
  (testing "day 9 solutions"
    (is (= 127 (day9/day9-part1-test)))
    (is (= 776203571 (day9/day9-part1)))
    (is (= 62 (day9/day9-part2-test)))
    (is (= 104800569 (day9/day9-part2)))))

(deftest day10-test
  (testing "day 10 functions"
    (is (= 1 (day10/count-possible-combos 1)))
    (is (= 2 (day10/count-possible-combos 2)))
    (is (= 4 (day10/count-possible-combos 3)))
    (is (= 7 (day10/count-possible-combos 4))))
  (testing "day 10 solutions"
    (is (= 220 (day10/day10-part1-test)))
    (is (= 2080 (day10/day10-part1)))
    (is (= 8 (day10/day10-part2-test "_small")))
    (is (= 19208 (day10/day10-part2-test)))
    (is (= 6908379398144 (day10/day10-part2)))))

(deftest day11-test
  (testing "day 11 functions"
    (let [small-grid (day11/get-test-input-grid)]
      (is
        (=
          '([1 6] [1 8] [3 8] [7 7] [3 6] [2 4])
          (day11/get-line-of-sight-neighbors small-grid 9 9 2 7)))))
  (testing "day 11 solutions"
    (is (= 37 (day11/day11-part1-test)))
    (is (= 2483 (day11/day11-part1)))
    (is (= 26 (day11/day11-part2-test)))
    (is (= 2285 (day11/day11-part2)))))

(deftest day12-test
  (testing "day 12 functions"
    (let [acc {:waypoint {:x -1 :y 5}}]
      (is (= {:waypoint {:x -5 :y -1}} (day12/rotate-waypoint -90 acc)))
      (is (= {:waypoint {:x 1 :y -5}} (day12/rotate-waypoint 180 acc)))
      (is (= {:waypoint {:x 5 :y 1}} (day12/rotate-waypoint 90 acc)))))
  (testing "day 12 solutions"
    (is (= 25 (day12/day12-part1-test)))
    (is (= 2297 (day12/day12-part1)))
    (is (= 286 (day12/day12-part2-test)))
    (is (= 89984 (day12/day12-part2)))))

(deftest day13-test
  (testing "day 13 functions")
  (testing "day 13 solutions"
    (is (= 295 (day13/day13-part1-test)))
    (is (= 4938 (day13/day13-part1)))
    (is (= 1068781 (day13/day13-part2-test)))
    (is (= 3417 (day13/day13-part2-test "_part2_extra1")))
    (is (= 754018 (day13/day13-part2-test "_part2_extra2")))
    (is (= 779210 (day13/day13-part2-test "_part2_extra3")))
    (is (= 1261476 (day13/day13-part2-test "_part2_extra4")))
    (is (= 1202161486 (day13/day13-part2-test "_part2_extra5")))
    (is (= 230903629977901 (day13/day13-part2)))))

(deftest day14-test
  (testing "day 14 functions"
    (is (= 73 (day14/apply-bitmasks 11 (day14/get-bitmasks "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X") false)))
    (let [part2-bms (day14/get-bitmasks "000000000000000000000000000000X1001X" true)
          float-bits (:float-bits part2-bms)]
      (is (= [5 0] float-bits))
      (is (= #{68 69 100 101} (set (day14/permute-all-floating-bits 100 float-bits))))))
  (testing "day 14 solutions"
    (is (= 165 (day14/day14-part1-test)))
    (is (= 6386593869035 (day14/day14-part1)))
    (is (= 208 (day14/day14-part2-test)))
    (is (= 4288986482164 (day14/day14-part2)))))
