(ns advent-of-code.day5
  (:gen-class)
  (:require [advent-of-code.util :as util]
            [clojure.core.reducers :as red]
            [clojure.math.numeric-tower :as math]))

(defn binary-decode-by-char [encoded-str lower-char upper-char]
  (loop [enc-str encoded-str lower 0 upper (dec (math/expt 2 (count encoded-str)))]
    (if
      (or (= lower upper) (empty? enc-str))
      lower
      (let [adj (/ (- upper lower) 2)
            [first-char & rest-str] enc-str]
        (cond
          (= first-char lower-char) (recur rest-str lower (math/floor (+ lower adj)))
          (= first-char upper-char) (recur rest-str (math/ceil (+ lower adj)) upper))))))

(defn encoded-line-to-row-and-col [line]
  (rest (re-matches #"((?:F|B){7})((?:L|R){3})" line)))

(defn encoded-line-to-seat-id [line]
  (let [[row col] (encoded-line-to-row-and-col line)
        row-num (binary-decode-by-char row \F \B)
        col-num (binary-decode-by-char col \L \R)]
    (+ (* 8 row-num) col-num)))

(defn day5-part1 []
  (let [input-lines (util/read-problem-input "input_day5")]
    (red/reduce max 0 (red/map encoded-line-to-seat-id input-lines))))

(defn find-missing-seat-id [sorted-seat-ids]
  (loop [seat-id (first sorted-seat-ids)
         rest-seat-ids (rest sorted-seat-ids)]
    (cond
      (empty? rest-seat-ids)
      nil
      (= 2 (- (first rest-seat-ids) seat-id))
      (inc seat-id)
      :default
      (let [[next-id & recur-ids] rest-seat-ids]
        (recur next-id recur-ids)))))

(defn day5-part2 []
  (let [input-lines (util/read-problem-input "input_day5")
        sorted-seat-ids (->> input-lines
                             (map encoded-line-to-seat-id)
                             sort)]
    (find-missing-seat-id sorted-seat-ids)))
