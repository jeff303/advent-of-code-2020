(ns advent-of-code.day12
  (:gen-class)
  (:require [advent-of-code.util :as util]
            [clojure.set :as s]
            [clojure.math.numeric-tower :as math]))

(defn char-to-dir [ch]
  (case ch
    \N :north
    \S :south
    \E :east
    \W :west
    \L :rotate-left
    \R :rotate-right
    \F :forward))

(defn input-line-to-instruction [input-line]
  (let [[_ dir amt] (re-matches #"^([NSEWLRF])(\d+)$" input-line)]
    [(char-to-dir (first dir)) (Integer. amt)]))

(defn parse-input-instructions
  ([]
   (parse-input-instructions (util/get-day-input *ns*)))
  ([input-res]
   (let [input-lines (util/read-problem-input-as-lines input-res)]
     (map input-line-to-instruction input-lines))))

(def dir-to-degree
  {:north 0
   :east 90
   :south 180
   :west 270})

(def degree-to-dir (s/map-invert dir-to-degree))

(defn normalize-right-turn
  ([amt]
   (normalize-right-turn amt 0))
  ([amt initial]
   (let [plus-360 (+ initial 360)
         adj (+ plus-360 amt)]
     (mod adj 360))))

(defn get-updated-dir [dir change]
  (let [deg (dir-to-degree dir)
        normalized-turn (normalize-right-turn change deg)]
    (degree-to-dir normalized-turn)))

(defn rotate-waypoint [amount acc]
  (let [waypoint (:waypoint acc)
        way-x (:x waypoint)
        way-y (:y waypoint)
        normalized-turn (normalize-right-turn amount)]
    (case normalized-turn
      0 acc ; nothing to change
      90 (assoc acc :waypoint {:x way-y :y (- way-x)}) ; 90-degree right turn (x becomes y, y becomes -x)
      180 (assoc acc :waypoint {:x (- way-x) :y (- way-y)}) ; 180-degree turn (x and y keep magnitude but flip signs)
      270 (assoc acc :waypoint {:x (- way-y) :y way-x})))) ; 270-degree right turn (x becomes -y, y becomes x)

(defn rotate-right [amount acc]
  (let [waypoint (:waypoint acc)]
    (if
      (some? waypoint)
      (rotate-waypoint amount acc) ; rotate the waypoint about the origin (part 2)
      (assoc acc :dir (get-updated-dir (:dir acc) amount))))) ; rotate the ship itself (part 1)

(defn move-towards-waypoint [amount acc]
  (let [curr-x (:x acc)
        curr-y (:y acc)
        waypoint (:waypoint acc)
        way-x (:x waypoint)
        way-y (:y waypoint)]
    (assoc acc :x (+ curr-x (* amount way-x))
               :y (+ curr-y (* amount way-y)))))

(defn move-ship-or-waypoint [has-waypoint acc dim amount]
  (let [curr-pos (if has-waypoint (dim (:waypoint acc)) (dim acc))]
    (assoc acc
      (if has-waypoint :waypoint dim)
      (if has-waypoint
        (assoc (:waypoint acc) dim (+ curr-pos amount))
        (+ curr-pos amount)))))

(defn move-in-direction [dir amount acc]
  (let [has-waypoint (some? (:waypoint acc))]
    (case dir
      :north (move-ship-or-waypoint has-waypoint acc :y (+ amount))
      :east (move-ship-or-waypoint has-waypoint acc :x (+ amount))
      :south (move-ship-or-waypoint has-waypoint acc :y (- amount))
      :west (move-ship-or-waypoint has-waypoint acc :x (- amount))
      :forward
      (if
        has-waypoint
        (move-towards-waypoint amount acc)
        (move-in-direction (:dir acc) amount acc)))))

(defn execute-instruction-xf [rf]
  (fn
    ([] (rf))
    ([result] (rf result))
    ([result [inst amt]]
     (case inst
       :rotate-left (rotate-right (- amt) result)
       :rotate-right (rotate-right amt result)
       (move-in-direction inst amt result)))))

(defn execute-instruction-rf [with-waypoint]
  (fn
    ([] (->
          {:x 0 :y 0 :dir :east}
          (cond-> with-waypoint (assoc :waypoint {:x 10 :y 1}))))
    ([result] result)))

(defn get-manhattan-distance [acc]
  (let [x (:x acc)
        y (:y acc)]
    (+ (math/abs x) (math/abs y))))

(defn day12-part1
  ([]
   (day12-part1 (util/get-day-input *ns*)))
  ([input-res]
   (let [instructions (parse-input-instructions input-res)
         final-acc (transduce execute-instruction-xf (execute-instruction-rf false) instructions)]
     (get-manhattan-distance final-acc))))

(defn day12-part1-test []
  (day12-part1 (util/get-day-test-input *ns*)))

(defn day12-part2
  ([]
   (day12-part2 (util/get-day-input *ns*)))
  ([input-res]
   (let [instructions (parse-input-instructions input-res)
         final-acc (transduce execute-instruction-xf (execute-instruction-rf true) instructions)]
     (get-manhattan-distance final-acc))))

(defn day12-part2-test []
  (day12-part2 (util/get-day-test-input *ns*)))
