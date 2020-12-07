(ns advent-of-code.day1
  (:gen-class)
  (:require [advent-of-code.util :as util]))

(defn two-sum
  ([nums target-sum]
   (two-sum nums target-sum 0 (dec (count nums))))
  ([nums target-sum start-idx end-idx]
   (loop [from start-idx
           to end-idx]
      (if (or (>= from to) (>= to (count nums)))
        [nil nil]
        (let [begin (nth nums from)
              end (nth nums to)
              sum (+ begin end)]
          (cond
            (< sum target-sum) (recur (inc from) to)
            (> sum target-sum) (recur from (dec to))
            :default [from to]))))))


(defn get-input-nums []
  (let [input-lines (util/read-problem-input "input_day1")]
    (sort (map #(Integer/parseInt %) input-lines))))

(defn day1-part1 []
  (let [nums (get-input-nums)
        [start-idx end-idx] (two-sum nums 2020)]
    (if (not (nil? start-idx))
      (* (nth nums start-idx) (nth nums end-idx)))))

(def two-sum-memoized (memoize two-sum))

(defn three-sum [nums target-sum]
  (first
    (keep-indexed (fn [i num]
                    (let [two-sum-target (- target-sum num)
                          two-sum-ans (two-sum-memoized nums
                                                        two-sum-target
                                                        (inc i)
                                                        (dec (count nums)))
                          [two-sum-start two-sum-end] two-sum-ans]
                      (if (not (nil? two-sum-start))
                        [i two-sum-start two-sum-end])))
                  nums)))

(defn day1-part2 []
  (let [nums (get-input-nums)
        indexes (three-sum nums 2020)]
    (if (->> indexes first nil? not)
      (reduce * (map #(nth nums %) indexes)))))

;; TODO: figure out how to not have this run from tests
(let [part1 (day1-part1)
      part2 (day1-part2)]
  (doall
    (keep-indexed
      (fn [i soln]
        (if
          (nil? soln)
          (prn "No solution to part " (inc i))
          (prn "Solution to part" (inc i) ": " soln))) [part1 part2])))
