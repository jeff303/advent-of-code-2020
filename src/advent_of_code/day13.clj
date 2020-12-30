(ns advent-of-code.day13
  (:gen-class)
  (:require [advent-of-code.util :as util]
            [clojure.string :as st]
            [clojure.math.numeric-tower :as math]))

(defn load-problem-input [res-file]
  (let [lines (util/read-problem-input-as-lines res-file)
        [earliest-time bus-id-line] lines
        bus-id-strs (st/split bus-id-line #",")
        bus-ids (->> bus-id-strs
                     (map
                       #(if (= % "x")
                          nil
                          (Long. %)))
                     (vec))]
    {:earliest (Long. earliest-time) :bus-ids bus-ids}))

(defn calculate-gap [target num]
  (let [m (mod target num)]
    (- num m)))

(defn calculate-bus-gaps [input]
  (reduce
    #(assoc %1 (calculate-gap (:earliest input) %2) %2)
    (sorted-map)
    (filter some? (:bus-ids input))))

(defn make-schedule-check-fn [bus-ids]
  (let [bus-check-fns (keep-indexed
                        (fn [idx bus-id]
                          (if
                            (some? bus-id)
                            #(= 0 (mod (+ idx %) bus-id))))
                        bus-ids)]
    (fn [ts]
      (not-any?
        false?
        ((apply
           juxt
           bus-check-fns) ts)))))

(defn day13-part1
  ([]
   (day13-part1 (util/get-day-input *ns*)))
  ([res-file]
   (let [input (load-problem-input res-file)
         bus-gaps (calculate-bus-gaps input)
         [wait bus-id] (first bus-gaps)]
     (* wait bus-id))))

(defn day13-part1-test []
  (day13-part1 (util/get-day-test-input *ns*)))

; naive solution, way too slow
(defn day13-part2-slow
  ([]
   (day13-part2-slow (util/get-day-input *ns*)))
  ([res-file]
   (let [input (load-problem-input res-file)
         bus-ids (:bus-ids input)
         first-bus (first
                     (keep-indexed
                       (fn [idx bus-id]
                         (if (some? bus-id) [idx bus-id]))
                       bus-ids))
         [first-bus-idx first-bus-id] first-bus
         start-ts (- first-bus-id first-bus-idx)
         sched-check-fn (make-schedule-check-fn bus-ids)]
     (loop [ts start-ts iter 0]
       (cond
         (> iter 100000000000000)
         ; too many iterations; give up to prevent infinite loop
         nil
         (sched-check-fn ts)
         ts
         :default
         (recur (+ ts start-ts) (inc iter)))))))

; shamelessly copied from: https://rosettacode.org/wiki/Chinese_remainder_theorem#Clojure
(defn extended-gcd
  "The extended Euclidean algorithm
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs. "
  [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (math/abs b)
                     r0 (math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn chinese-remainder
  " Main routine to return the chinese remainder "
  [n a]
  (let [prod (apply * n)
        reducer (fn [sum [n_i a_i]]
                  (let [p (quot prod n_i)           ; p = prod / n_i
                        egcd (extended-gcd p n_i)   ; Extended gcd
                        inv_p (second egcd)]        ; Second item is the inverse
                    (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))] ; Replaces the Python for loop to sum
    ; (map vector n a) is same as
    ;                                             ; Python's version Zip (n, a)
    (mod sum-prod prod)))                             ; Result line

; Implementation note: I knew this was close to LCM, but with an added offset.  Googled for "LCM with offset", which
; eventually led to discussion of this exact problem, and the Chinese Remainder Theorem.  This version completes within
; the lifetime of the known universe.
(defn day13-part2
  ([]
   (day13-part2 (util/get-day-input *ns*)))
  ([res-file]
   (let [input (load-problem-input res-file)
         bus-ids (:bus-ids input)
         remainder-inputs (keep-indexed
                            (fn [idx bus-id]
                              (if (some? bus-id) [bus-id (- idx)]))
                            bus-ids)]
     (chinese-remainder
       (map first remainder-inputs)
       (map last remainder-inputs)))))

(defn day13-part2-test
  ([]
   (day13-part2-test ""))
  ([variant]
   (day13-part2 (util/get-day-test-input *ns* variant))))
