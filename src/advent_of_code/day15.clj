(ns advent-of-code.day15
  (:gen-class)
  (:require [advent-of-code.util :as util]))

(defn problem-input-to-nums [input-res]
  (map #(Long. %) (util/read-problem-input-split-by input-res #",")))

(defn get-spoken-num
  ([starting-nums]
   (let [init-acc (apply
                    merge
                    (keep-indexed
                      (fn [idx val]
                        {val (inc idx)})
                      starting-nums))
         last-start-num (last starting-nums)
         init-round (inc (count starting-nums))]
     (get-spoken-num init-acc init-round last-start-num)))
  ([accumulator round-num prev-val]
   (let [last-seen (get accumulator prev-val)
         last-round-num (dec round-num)
         seen-before? (and (some? last-seen) (< last-seen last-round-num))
         this-round (if seen-before? (- last-round-num last-seen) 0)]
     (lazy-seq
       (cons
         this-round
         (get-spoken-num
           (assoc accumulator prev-val last-round-num)
           (inc round-num)
           this-round))))))

(defn day15-part1
  ([]
   (day15-part1
     (util/get-day-input *ns*) 2020))
  ([input-res through-round]
   (let [starting-nums (problem-input-to-nums input-res)
         init-rounds (count starting-nums)]
     (last
       (take
         (- through-round init-rounds)
         (get-spoken-num starting-nums))))))

(defn day15-part1-test []
  (day15-part1
    (util/get-day-test-input *ns*)
    2020))

(defn day15-part2
  ([]
   (day15-part2
     (util/get-day-input *ns*)))
  ([input-res]
   (day15-part1 input-res 30000000)))

(defn day15-part2-test [variant]
  (day15-part2
    (util/get-day-test-input *ns* variant)))