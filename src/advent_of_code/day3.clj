(ns advent-of-code.day3
  (:gen-class)
  (:require [advent-of-code.util :as util]))

(defn password-valid-part1
  ;; in part1, the n-low and n-high represent a min/max number of occurrences per character, respectively
  [requirements password]
  (let [grouped-by-char (group-by identity (seq (char-array password)))]
    (every?
      true?
      (for [[pwchar [min-occ max-occ]] requirements]
        (<= min-occ (count (get grouped-by-char pwchar)) max-occ)))))

(defn password-valid-part2
  ;; in part2, the n-low and n-high represent two positions (1-based) where the character must occur (exactly once)
  [requirements password]
  (every?
    true?
    (for [[pwchar positions] requirements]
      (=
        1
        (->> positions (map #(nth password (dec %))) (filter #(= pwchar %)) count)))))

(defn- parse-line-to-tree-markers [line]
  (vec
    (for [c line]
      (= c \#))))

(defn get-tree-markers []
  (let [input-lines (util/read-problem-input-as-lines "input_day3")]
    (map parse-line-to-tree-markers input-lines)))

(defn count-tree-hits [tree-markers move-right move-down]
  (let [num-rows (count tree-markers)
        num-cols (count (first tree-markers))]
    (loop [x 0
           y 0
           hits 0]
      (if (>= y num-rows)
        hits
        (let [hit (nth (nth tree-markers y) (mod x num-cols))]
          (recur (+ x move-right) (+ y move-down) (+ hits (if hit 1 0))))))))

(defn day3-part1 []
  (let [tree-markers (get-tree-markers)]
    (count-tree-hits tree-markers 3 1)))

(defn day3-part2 []
  (let [tree-markers (get-tree-markers)]
    (reduce
      *
      (for [[move-x move-y] [[1 1] [3 1] [5 1] [7 1] [1 2]]]
        (count-tree-hits tree-markers move-x move-y)))))