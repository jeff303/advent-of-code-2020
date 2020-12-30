(ns advent-of-code.util
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-problem-input-as-lines
  [resource-file]
  (with-open [rdr (-> resource-file io/resource io/reader)]
    (reduce conj [] (line-seq rdr))))

(defn read-problem-input-split-by
  [resource-file split-by]
  (with-open [rdr (-> resource-file io/resource io/reader)]
    (str/split (slurp rdr) split-by)))

(defn get-day [ns-sym]
  (-> ns-sym
      (resolve)
      (var-get)
      (ns-name)
      (name)
      (str/split #"\.")
      (last)))

(defmacro get-day-input [ns-sym]
  (let [day (get-day ns-sym)]
    (format "input/%s.txt" day)))

(defmacro get-day-test-input
  ([ns-sym]
   `(get-day-test-input ~ns-sym ""))
  ([ns-sym variant]
   (let [day (get-day ns-sym)]
     `(format "test_input/%s%s.txt" ~day ~variant))))
