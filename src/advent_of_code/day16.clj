(ns advent-of-code.day16
  (:gen-class)
  (:require [advent-of-code.util :as util]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-rule [input-line]
  (let [[_ rule-name r1-st r1-end r2-st r2-end] (re-matches #"^([^:]+): *(\d+)-(\d+) or (\d+)-(\d+)$" input-line)]
    [rule-name [[(Long. r1-st) (Long. r1-end)] [(Long. r2-st) (Long. r2-end)]]]))

(defn parse-ticket [input-line]
  (map #(Long. %) (str/split input-line #",")))

(defn parse-problem-input
  ([] (parse-problem-input (util/get-day-input *ns*)))
  ([input-res]
   (let [[rules-grp your-ticket-grp nearby-tickets-grp] (util/read-problem-input-split-by input-res #"\n\n")
         rules-lines                                    (str/split-lines rules-grp)
         your-ticket-lines                              (str/split-lines your-ticket-grp)
         nearby-tickets-lines                           (str/split-lines nearby-tickets-grp)
         all-rules                                      (reduce conj [] (map parse-rule rules-lines))
         your-ticket                                    (parse-ticket (first (drop 1 your-ticket-lines)))
         nearby-tickets                                 (map parse-ticket (rest nearby-tickets-lines))]
     [all-rules your-ticket nearby-tickets])))

(defn ticket-value-valid-for-rule? [ticket-val [_ [[r1-st r1-end] [r2-st r2-end]]]]
  (or (<= r1-st ticket-val r1-end) (<= r2-st ticket-val r2-end)))

(defn ticket-value-valid? [rules ticket-val]
  (first (filter (partial ticket-value-valid-for-rule? ticket-val) rules)))

(defn find-invalid-ticket-vals [rules ticket]
  (for [ticket-val ticket]
    (if (nil? (ticket-value-valid? rules ticket-val)) ticket-val)))

(defn ticket-valid? [rules ticket]
  (not-any? some? (find-invalid-ticket-vals rules ticket)))

(defn day16-part1
  ([]
   (day16-part1 (util/get-day-input *ns*)))
  ([input-res]
   (let [[rules _ nearby-tickets] (parse-problem-input input-res)
         all-invalid-values       (apply concat
                                         (map (partial find-invalid-ticket-vals rules) nearby-tickets))]
     (reduce + (filter some? all-invalid-values)))))

(defn narrow-rules [all-rules possible-rule-by-pos valid-ticket]
  (merge-with set/intersection
              possible-rule-by-pos
              (apply merge (keep-indexed
                             (fn [t-idx val]
                               {t-idx (set (keep-indexed (fn [rule-idx rule]
                                                           (if (ticket-value-valid-for-rule? val rule) rule-idx))
                                             all-rules))})
                             valid-ticket))))

(defn find-unique-rules [possibilities covered-rule-indexes acc]
  (if (empty? possibilities) acc
      (let [uniques      (filter (fn [[_ vals]] (= 1 (count vals))) possibilities)
            uniques-flat (map (fn [[k v]]
                                (if (set? v)
                                    [k (first v)]
                                    [k v])) uniques)
            new-poss     (into {} uniques-flat)
            new-covered  (set (map last new-poss))
            recur-poss   (reduce-kv (fn [acc k v]
                                      (let [new-v (set/difference v new-covered)]
                                        (if (not-empty new-v)
                                          (assoc acc k new-v)
                                          (dissoc acc k))))
                           {} possibilities)]
        (find-unique-rules recur-poss (set/union covered-rule-indexes new-covered) (merge acc new-poss)))))

(defn day16-part2
  ([]
   (day16-part2 (util/get-day-input *ns*)))
  ([input-res]
   (let [[rules my-ticket nearby-tickets] (parse-problem-input input-res)
         valid-tickets                    (filter (partial ticket-valid? rules) (conj nearby-tickets my-ticket))
         start-rule-set                   (set (range 0 (count rules)))
         ticket-size                      (count (first valid-tickets))
         start-set                        (apply merge (for [i (range 0 ticket-size)] {i start-rule-set}))
         narrowed-possibilities           (reduce (partial narrow-rules rules) start-set valid-tickets)
         unique-possibilities             (find-unique-rules narrowed-possibilities #{} {})
         departure-fields                 (reduce-kv (fn [acc k v]
                                                       (let [rule (nth rules v)]
                                                         (if (str/starts-with? (first rule) "departure")
                                                           (conj acc k)
                                                           acc))) [] unique-possibilities)]
     (reduce * (map (partial nth my-ticket) departure-fields)))))

