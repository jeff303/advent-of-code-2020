(ns advent-of-code.day7
  (:gen-class)
  (:require [advent-of-code.util :as util]
            [clojure.string :as str]
            [clojure.core.reducers :as r]))

(defn input-line-to-rule [input-line]
  (let [[_ start-color contain-rules] (re-matches #"(.*) bags contain (.*)" input-line)
        contain-rule-seq (re-seq #"\d+[^,.]*(?= bag)" contain-rules)]
    {start-color
      (into
        {}
        (for [contain-rule contain-rule-seq]
          (let [[num color] (str/split contain-rule #" " 2)]
            [color (Integer. num)])))}))

(defn parse-inputs-as-rule-map
  ([]
   (parse-inputs-as-rule-map (util/get-day-input *ns*)))
  ([res-file]
   (let [input-lines (util/read-problem-input-as-lines res-file)
         individual-rules (r/map input-line-to-rule input-lines)]
     (reduce merge individual-rules))))

(defn parse-test-inputs-as-rule-map
  ([]
   (parse-test-inputs-as-rule-map ""))
  ([variant]
   (parse-inputs-as-rule-map (util/get-day-test-input *ns* variant))))

(defn target-color-reachable [rules visited-colors start-color target-color]
  (if
    (= (count visited-colors) (count rules))
    false
    (let [next-visited (conj visited-colors start-color)
          next-colors (filter #(not (contains? visited-colors %)) (keys (get rules start-color)))]
      (some
        true?
        (for [next-color next-colors]
          (or
             (= target-color next-color)
             (target-color-reachable rules next-visited next-color target-color)))))))

(defn count-reachable-colors [rules target-color]
  (reduce
    +
    (pmap
      #(if (target-color-reachable rules #{} % target-color) 1 0)
      (keys rules))))

(defn day7-part1 []
  (count-reachable-colors (parse-inputs-as-rule-map) "shiny gold"))

(defn count-total-reachable-bags [rules visited-colors start-color]
  (let [next-visited (conj visited-colors start-color)
        next-colors (get rules start-color)
        minus-visited (apply dissoc next-colors visited-colors)
        sum-nested-fn (fn [rule]
                        (let [[color num] rule
                              res (count-total-reachable-bags rules next-visited color)]
                          (+ num (* num res))))]
      (reduce + (pmap sum-nested-fn minus-visited))))

(defn day7-part2 []
  (count-total-reachable-bags (parse-inputs-as-rule-map) #{} "shiny gold"))
