(ns advent-of-code.day6
  (:gen-class)
  (:require [advent-of-code.util :as util]
            [clojure.string :as str]
            [clojure.core.reducers :as r]
            [clojure.set :as s]))

(defn count-union-yes-answers [group-input]
  (let [group-yes-answers (into #{} (r/filter #(not= \newline %) group-input))]
    (count group-yes-answers)))

(defn- parse-input-as-groups
  ([] (parse-input-as-groups (util/get-day-input *ns*)))
  ([input-res]
   (util/read-problem-input-split-by input-res #"\n\n")))

(defn parse-test-input-as-groups []
  (parse-input-as-groups (util/get-day-test-input *ns*)))

(defn day6-part1 []
  (let [input-groups (parse-input-as-groups)
        counts-by-group (r/map count-union-yes-answers input-groups)]
    (r/reduce + counts-by-group)))

(defn count-intersection-yes-answers [group-input]
  (let [individual-input (str/split-lines group-input)
        individual-yes-ans (r/map #(set (map identity %)) individual-input)
        individual-yes-ans-combined (reduce s/intersection (into [] individual-yes-ans))]
    (count individual-yes-ans-combined)))

(defn day6-part2 []
  (let [input-groups (parse-input-as-groups)
        counts-by-group (r/map count-intersection-yes-answers input-groups)]
    (r/reduce + counts-by-group)))