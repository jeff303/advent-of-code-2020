(ns advent-of-code.day11
  (:gen-class)
  (:require [advent-of-code.util :as util]
            [clojure.string :as str]
            [clojure.core.reducers :as r]
            [clojure.set :as s]))

(defn seat-is [grid row col pred]
  (pred
    (nth
      (nth grid row)
      col)))

(defn- valid-seat? [grid max-row max-col [r c]]
  (and
    (some? r)
    (some? c)
    (<= 0 r max-row)
    (<= 0 c max-col)
    (seat-is grid r c some?)))

(defn get-adjacent-neighbors [grid max-row max-col row col]
  (filter
    (partial valid-seat? grid max-row max-col) ; space is actually a seat
    [[(dec row), (dec col)]    ; up & left
     [(dec row), col]          ; up
     [(dec row), (inc col)]    ; up & right
     [row, (inc col)]          ; right
     [(inc row), (inc col)]    ; down & right
     [(inc row), col]          ; down
     [(inc row), (dec col)]    ; down & left
     [row, (dec col)]]))       ; left

(defn go-until-seat [grid row col max-row max-col row-chg-fn col-chg-fn]
  (loop [r (row-chg-fn row)
         c (col-chg-fn col)]
    (let [in-bounds (and (<= 0 r max-row) (<= 0 c max-col))]
      (cond
        (not in-bounds)
        nil
        (and in-bounds (seat-is grid r c nil?))
        (recur
          (row-chg-fn r)
          (col-chg-fn c))
        :default
        [r c]))))

(defn get-line-of-sight-neighbors [grid max-row max-col row col]
  (let [travel-fn (partial go-until-seat grid row col max-row max-col)]
    (filter
      (partial valid-seat? grid max-row max-col)
      [(travel-fn dec dec)          ; up & left
       (travel-fn dec identity)     ; up
       (travel-fn dec inc)          ; up & right
       (travel-fn identity inc)     ; right
       (travel-fn inc inc)          ; down & right
       (travel-fn inc identity)     ; down
       (travel-fn inc dec)          ; down & left
       (travel-fn identity dec)]))) ; left

(defn count-seats [grid seat-pred [row col]]
  (-> grid
      (nth row)
      (nth col)
      (seat-pred)
      (if 1 0)))

(defn count-neighbor-seats* [grid get-neighbors-fn seat-pred max-row max-col row col]
  (reduce
    +
    (map
      #(count-seats grid seat-pred %)
      (get-neighbors-fn grid max-row max-col row col))))

(def count-neighbor-seats (memoize count-neighbor-seats*))

(defn input-line-to-grid-row [line]
  (into
    []
    (map #(case % \# :occupied \L :unoccupied \. nil) line)))

(defn get-input-grid [res-file]
  (let [lines (util/read-problem-input-as-lines res-file)]
    (into
      []
      (map input-line-to-grid-row lines))))

(defn row-to-str [row]
  (apply str (map #(case % :occupied \# :unoccupied \L \.) row)))

(defn print-grid [grid]
  (doall (map #(println (row-to-str %)) grid)))

(defn example-xf
  ;; init - returns initial value for accumulator, called when no init is given to transduce
  ([] [])
  ;; completion - returns the final result, take the final accumulated value, called once there are no more elements to process
  ([acc] acc)
  ;; step - do whatever you want on each element, returns accumulated state and takes accumulated state from before and new element
  ([acc e] (conj acc e)))

(defn process-point-xf [get-neighbors-fn vacate-threshold rf]
  (fn
    ([] (rf)) ; init case - call the 0-arity of reducing fn
    ([acc] (rf acc)) ; completion case - call the 1-arity of the reducing fn to finalize the result
    ([acc e] ; step case - process a grid point and accumulate grid update fns
     (let [[row col] e
           grid (:grid acc)
           max-row (dec (count grid))
           max-col (dec (count (nth grid 0)))
           ; make a function for updating a seat's occupied status in the grid
           update-seat-fn (fn [row col status]
                            #(do
                               (assoc! (nth % row) col status)
                               %))
           ; make a function for updating the check-next set
           update-check-next (fn [row col]
                               (let [impacted-neighbors
                                     (filter
                                       some?
                                       (get-neighbors-fn grid max-row max-col row col))]
                                 #(into % impacted-neighbors)))
           check-next (:check-next acc)
           update-fns (:update-fns acc)
           occupied-neighbors (count-neighbor-seats grid get-neighbors-fn #(= :occupied %) max-row max-col row col)
           vacate-fns (if
                        (and
                          (valid-seat? grid max-row max-col [row col])
                          (seat-is grid row col #(= % :occupied)))
                        [(update-seat-fn row col :unoccupied) (update-check-next row col)])
           [update-grid-fn update-check-fn]
           (cond
             (= occupied-neighbors 0) ; if 0 neighbors are occupied, occupy the seat
             (if
               (and
                 (valid-seat? grid max-row max-col [row col])
                 (seat-is grid row col #(= % :unoccupied)))
               [(update-seat-fn row col :occupied) (update-check-next row col)])
             (>= occupied-neighbors vacate-threshold) vacate-fns)
           new-update-fns (if (some? update-grid-fn) (conj update-fns update-grid-fn) update-fns)
           updated-occ (if (some? update-check-fn) (update-check-fn check-next) check-next)]
       {:round-num (inc (:round-num acc))
        :grid grid
        :check-next updated-occ
        :update-fns new-update-fns}))))

(defn run-round-rf [start-grid]
  (fn
    ([]
     {:round-num 0 :grid start-grid :check-next #{} :update-fns []})
    ([acc]
     (assoc acc :next-grid ((reduce comp (:update-fns acc)) (:grid acc))))))

(defn get-start-check-points [grid]
  (apply
    concat
    (keep-indexed
      (fn [row-num row]
        (vec
          (keep-indexed
            (fn [col-num cell]
              (if (= :unoccupied cell) [row-num col-num])) row)))
      grid)))

(defn make-transient [grid]
  (transient (mapv transient grid)))

(defn make-persistent [grid]
  (vec (map persistent! (persistent! grid))))

(defn run-until-stable [grid get-neighbors-fn vacate-threshold]
  (loop [cur-grid (make-transient grid)
         check-points (get-start-check-points grid)
         round-num 0]
    (let [round-res (transduce
                      (partial process-point-xf get-neighbors-fn vacate-threshold)
                      (run-round-rf cur-grid) check-points)
          has-changes (not (empty? (:update-fns round-res)))]
      (do
        ;(println "Running round " round-num)
        ;(print-grid cur-grid)
        (if
          has-changes
          ; keep running the simulation
          (recur (:next-grid round-res) (:check-next round-res) (inc round-num))
          ; return the round number and grid
          [round-num (make-persistent cur-grid)])))))

(defn day11-part1
  ([]
   (day11-part1 "input_day11"))
  ([input-res]
   (let [grid (get-input-grid input-res)
         [rounds final-grid] (run-until-stable grid get-adjacent-neighbors 4)
         final-counts (for [i (range 0 (count final-grid))
                            j (range 0 (count (first final-grid)))]
                        (count-seats final-grid #(= :occupied %) [i j]))
         final-occupied (reduce + final-counts)]
     final-occupied)))

(defn day11-part2
  ([]
   (day11-part2 "input_day11"))
  ([input-res]
   (let [grid (get-input-grid input-res)
         [rounds final-grid] (run-until-stable grid (memoize get-line-of-sight-neighbors) 5)
         final-counts (for [i (range 0 (count final-grid))
                            j (range 0 (count (first final-grid)))]
                        (count-seats final-grid #(= :occupied %) [i j]))
         final-occupied (reduce + final-counts)]
       final-occupied)))
