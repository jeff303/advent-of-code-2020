(ns advent-of-code.day2
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

(defn get-requirements-and-passwords []
  (let [input-lines (util/read-problem-input-as-lines (util/get-day-input *ns*))
        parsed-lines (map #(re-matches #"^(\d+)-(\d+) (.): (.*)$" %) input-lines)]
    (for [parsed-line parsed-lines]
      (let [[_ n-low n-high pw-char pw] parsed-line
            pw-requirement {(nth pw-char 0) [(Integer/parseInt n-low) (Integer/parseInt n-high)]}]
        [pw-requirement pw]))))

(defn day2-part1 []
  (count
    (filter
      true?
      (for [[req pw] (get-requirements-and-passwords)]
        (password-valid-part1 req pw)))))

(defn day2-part2 []
  (count
    (filter
      true?
      (for [[req pw] (get-requirements-and-passwords)]
        (password-valid-part2 req pw)))))
