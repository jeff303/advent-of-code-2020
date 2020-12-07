(ns advent-of-code.day6
  (:gen-class)
  (:require [advent-of-code.util :as util]
            [clojure.string :as str]
            [clojure.core.reducers :as r]))

(defn count-yes-answers [group-input]
  (let [group-yes-answers (into #{} (r/filter #(not= \newline %) group-input))]
    (count group-yes-answers)))

(defn day6-part1 []
  (let [input-groups (util/read-problem-input-split-by "input_day6" #"\n\n")
        counts-by-group (r/map count-yes-answers input-groups)]
    (r/reduce + counts-by-group)))
