(ns advent-of-code.day9
  (:gen-class)
  (:require [advent-of-code.util :as util]
            [clojure.string :as str]
            [clojure.core.reducers :as r]
            [clojure.set :as s]))

(defn parse-inputs-as-numbers
  ([]
   (parse-inputs-as-numbers "input_day9"))
  ([res-file]
   (let [input-lines (util/read-problem-input-as-lines res-file)]
     (map #(Long. %) input-lines))))

(defn two-sum-from-sorted-set [nums target-sum]
  (first
    (filter
      some?
      (for [num nums]
        (let [find-num (- target-sum num)]
          (if
            (contains? nums find-num)
            [num find-num]))))))

(defn find-first-invalid-number
  ([numbers window-length]
   (let [preamble (into [] (take window-length numbers))
         preamble-sorted (into (sorted-set) preamble)
         [next-num & remaining-nums] (drop window-length numbers)]
     (find-first-invalid-number next-num remaining-nums preamble preamble-sorted)))
  ([next-num remaining-nums window-in-order window-sorted]
   (if
     (nil? next-num)
     nil
     (let [two-sum (two-sum-from-sorted-set window-sorted next-num)]
       (if
         (nil? two-sum)
         next-num
         (let [[recur-num & recur-remaining] remaining-nums
               [drop-num & rest-nums] window-in-order
               recur-window (conj (vec rest-nums) next-num)
               recur-window-sorted (conj (disj window-sorted drop-num) next-num)]
           (find-first-invalid-number recur-num recur-remaining recur-window recur-window-sorted)))))))

(defn day9-part1 []
  (let [nums (parse-inputs-as-numbers)]
    (find-first-invalid-number nums 25)))

(defn subseq-sum* [nums from to]
  (reduce + (subvec nums from to)))

(def subseq-sum (memoize subseq-sum*))

; brute force with memoization; no time to implement fancy Leetcode shit
(defn find-continuous-subseq-summing-to [nums target]
  (first
    (filter
      some?
      (for [i (range 0 (count nums))
            j (range 0 i)]
        (let [sum (subseq-sum nums j i)]
          (if
            (= target sum)
            (subvec nums j i)))))))

(defn find-encryption-weakness [nums bad-num]
  (let [s (find-continuous-subseq-summing-to nums bad-num)
        min-max-reducer (fn [[cur-min cur-max] n]
                          [(if (nil? cur-min) n (min cur-min n))
                           (if (nil? cur-max) n (max cur-max n))])
        [min-num max-num] (reduce min-max-reducer [nil nil] s)]
    (+ min-num max-num)))

(defn day9-part2 []
  (let [nums (into [] (parse-inputs-as-numbers))
        invalid-num (day9-part1)]
    (find-encryption-weakness nums invalid-num)))
