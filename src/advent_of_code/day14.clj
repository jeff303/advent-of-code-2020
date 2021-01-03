(ns advent-of-code.day14
  (:gen-class)
  (:require [advent-of-code.util :as util]
            [clojure.string :as st]
            [clojure.set :as s]
            [clojure.math.numeric-tower :as math]))

(defn input-line-to-instruction [input-line]
  (let [[_ inst mem-addr arg] (re-matches #"^(mask|mem\[([0-9]+)\]) *= *([^ ]+)$" input-line)]
    (if (= inst "mask")
      {:op :mask :arg arg}
      {:op :mem :addr (Long. mem-addr) :arg (Long. arg)})))

(defn get-bit-ops [bitmask-str]
  (keep-indexed
    (fn [idx ch]
      (let [bit-pos (- 35 idx)]
        (case ch
          \0 [:clear bit-pos]
          \1 [:set bit-pos]
          \X [:float bit-pos])))
    bitmask-str))

(def init-0s-mask
  (dec (math/expt 2 36)))

(def noop-bitmasks
  {:1s-mask 0 :0s-mask init-0s-mask :float-bits []})

(defn get-bitmasks
  ([bitmask-str]
   (get-bitmasks bitmask-str false))
  ([bitmask-str include-floating]
   (let [bit-ops (get-bit-ops bitmask-str)]
     (reduce
       (fn [acc item]
         (if
           (nil? item)
           acc
           (let [[op idx] item]
             (case op
               :clear (assoc acc :0s-mask (bit-clear (:0s-mask acc) idx)) ; keep a single bitmask for the "clear" ops
               :set (assoc acc :1s-mask (bit-set (:1s-mask acc) idx)) ; similarly for the "set" ops
               :float (assoc acc :float-bits (if include-floating (conj (:float-bits acc) idx))))))) ; but store floating bit positions in a set
       noop-bitmasks
       bit-ops))))

(defn apply-bitmasks [n bitmasks ignore-0s]
  (bit-or
    (if
      ignore-0s
      n
      (bit-and
        n
        (:0s-mask bitmasks)))
    (:1s-mask bitmasks)))

(defn update-memory-part1
  "For part 1, the bitmasks simply apply to the arg before storing it in memory"
  [memory bitmasks addr arg]
  (assoc memory addr (apply-bitmasks arg bitmasks false)))

(defn permute-all-floating-bits [n [first-pos & rest-pos]]
  (if
    (nil? first-pos)
    [n] ; base case; return the original number
    (mapcat ; set and clear this bit in all recursive results
      (fn [rec-n]
        [(bit-set rec-n first-pos)
         (bit-clear rec-n first-pos)])
      (permute-all-floating-bits n rest-pos))))

(defn update-memory-part2
  "For part 2, the bitmasks apply to the memory address, not the arg, and the floating positions permute all possible
  addresses at that bit"
  [memory bitmasks address arg]
  (let [start-addr (apply-bitmasks address bitmasks true) ; first, apply the 1s bitmask to the address itself (0s are ignored now)
        all-addrs (permute-all-floating-bits start-addr (:float-bits bitmasks))] ; then permute on all floating bits
    (reduce
      (fn [mem addr]
        (assoc mem addr arg))
      memory
      all-addrs)))

(defn run-program [instructions include-floating update-memory-fn]
  (loop [memory {} ; store memory as a map, since it may be sparse
         bitmasks noop-bitmasks
         insts instructions]
    (if
      (empty? insts)
      memory ; return memory at end of progaram
      (let [[first-inst & rest-insts] insts
            op (:op first-inst)]
        (case op
          :mask (recur ; recur with new bitmasks; this is almost the same for parts 1 and 2
                  memory
                  (get-bitmasks (:arg first-inst) include-floating) ; the only difference is the "floating" mask
                  rest-insts)
          :mem (recur ; this is where the difference in behavior between parts 1 and 2 comes into play
                 (update-memory-fn memory bitmasks (:addr first-inst) (:arg first-inst))
                 bitmasks
                 rest-insts))))))

(defn day14-part1
  ([]
   (day14-part1 (util/get-day-input *ns*)))
  ([input-res]
   (let [input-lines (util/read-problem-input-as-lines input-res)
         instructions (map input-line-to-instruction input-lines)
         program-mem (run-program instructions false update-memory-part1)]
     (reduce + (vals program-mem)))))

(defn day14-part1-test []
  (day14-part1 (util/get-day-test-input *ns*)))

(defn day14-part2
  ([]
   (day14-part2 (util/get-day-input *ns*)))
  ([input-res]
   (let [input-lines (util/read-problem-input-as-lines input-res)
         instructions (map input-line-to-instruction input-lines)
         program-mem (run-program instructions true update-memory-part2)]
     (reduce + (vals program-mem)))))

(defn day14-part2-test []
  (day14-part2 (util/get-day-test-input *ns* "_part2")))
