(ns advent-of-code.day16
  (:gen-class)
  (:require [advent-of-code.util :as util]
            [clojure.string :as str]))

(defn parse-valid-ranges [input-line]
  (let [[_ rule-name r1-st r1-end r2-st r2-end] (re-matches #"^([^:]+): *(\d+)-(\d+) or (\d+)-(\d+)$" input-line)]
    {rule-name [[(Long. r1-st) (Long. r1-end)] [(Long. r2-st) (Long. r2-end)]]}))

(defn parse-ticket [input-line]
  (map #(Long. %) (str/split input-line #",")))

(defn parse-problem-input
  ([] (parse-problem-input (util/get-day-input *ns*)))
  ([input-res]
   (let [[rules-grp your-ticket-grp nearby-tickets-grp] (util/read-problem-input-split-by input-res #"\n\n")
         rules-lines                                    (str/split-lines rules-grp)
         your-ticket-lines                              (str/split-lines your-ticket-grp)
         nearby-tickets-lines                           (str/split-lines nearby-tickets-grp)
         all-rules                                      (reduce merge (map parse-valid-ranges rules-lines))
         your-ticket                                    (parse-ticket (first (drop 1 your-ticket-lines)))
         nearby-tickets                                 (map parse-ticket (rest nearby-tickets-lines))]
     [all-rules your-ticket nearby-tickets])))

(defn ticket-value-valid? [rules ticket-val]
  (first (filter (fn [[[r1-st r1-end] [r2-st r2-end]]]
                   (or (<= r1-st ticket-val r1-end) (<= r2-st ticket-val r2-end)))
           (vals rules))))

(defn find-invalid-ticket-vals [rules ticket]
  (for [ticket-val ticket]
    (if (nil? (ticket-value-valid? rules ticket-val)) ticket-val)))

(defn day16-part1
  ([]
   (day16-part1 (util/get-day-input *ns*)))
  ([input-res]
   (let [[rules _ nearby-tickets] (parse-problem-input input-res)]
     (reduce + (filter some? (apply concat (map (partial find-invalid-ticket-vals rules) nearby-tickets)))))))
