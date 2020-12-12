(ns advent-of-code.day10
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

(defn get-sorted-adapters [input-nums]
  (let [sorted-nums (sort input-nums)]
    ; add the item representing our adapter
    (concat sorted-nums [(+ 3 (last sorted-nums))])))

(defn reduce-by-gap-size [acc curr-num]
  (let [[gap-1s gap-2s gap-3s prev-num] acc
        diff (- curr-num prev-num)]
    (case diff
      1
      [(inc gap-1s) gap-2s gap-3s curr-num]
      2
      [gap-1s (inc gap-2s) gap-3s curr-num]
      3
      [gap-1s gap-2s (inc gap-3s) curr-num])))

(defn day10-part1
  ([]
   (day10-part1 "input_day10"))
  ([input-res]
   (let [sorted-nums (sort (parse-inputs-as-numbers input-res))
         gaps (reduce reduce-by-gap-size [0 0 0 0] sorted-nums)
         [gap-1s _ gap-3s _] gaps] ; increment the 3 gaps to account for this:
                                   ; "your device's built-in adapter is always 3 higher than the highest adapter"
     (* gap-1s (inc gap-3s)))))

(def count-possible-combos
  "Counts the number of valid ways to add a consecutive sequence of adapters whose joltage increases by 1"
  (memoize
    (fn [len-1s-run]
      (case len-1s-run
        0 1 ; if the length of the run of the 1 diffs is only 1 (or less), there is only one valid way to add them
        1 1
        2 2 ; if the length is 2, there are two valid ways (ex: [1 2 3] or [1 3])
        3 4 ; if the length is 3, there are four ways (ex: [1 2 3 4] [1 2 4] [1 3 4] [1 4])
        ; else, the run length is 4 or more, ex: [a b c d & rest]
        ; we know we have to include a (by definition), so we can then include either b, or c, or d (in which case the
        ; number of combinations is the number of ways those items can be included)
        ; we CAN'T include a hypothetical e here, since then the joltage difference from a to e would be 4
        (+ (count-possible-combos (- len-1s-run 1))      ; include b, and see how many ways there are to do that
           (count-possible-combos (- len-1s-run 2))      ; include c
           (count-possible-combos (- len-1s-run 3))))))) ; include d

(defn reduce-by-valid-combinations [acc curr-num]
  (let [combos (:combos acc)
        prev-num (:prev-num acc)
        len-1s-run (:len-1s-run acc)
        diff (- curr-num prev-num)]
    (case diff
      1 ; we start (or continue) a run of 1s
      {:combos combos
       :prev-num curr-num
       :len-1s-run (inc len-1s-run)}
      ; by the problem definition, a diff of 2 is impossible
      3 ; we must include this adapter, since the difference is 3, so now we need to count the number of ways
        ; to add any sequence of 1 diffs we just saw
      {:combos (* combos (count-possible-combos len-1s-run))
       :prev-num curr-num
       :len-1s-run 0})))

(defn day10-part2
  ([]
   (day10-part2 "input_day10"))
  ([input-res]
   (let [input-nums (parse-inputs-as-numbers input-res)
         all-nums (get-sorted-adapters input-nums)
         combo-acc (reduce reduce-by-valid-combinations {:combos 1 :prev-num 0 :len-1s-run 0} all-nums)]
     ; "your device's built-in adapter is always 3 higher than the highest adapter"
     (:combos combo-acc))))
