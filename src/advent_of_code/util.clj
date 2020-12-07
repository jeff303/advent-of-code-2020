(ns advent-of-code.util
  (:require [clojure.java.io :as io]))

(defn read-problem-input
  [resource-file]
  (with-open [rdr (-> resource-file io/resource io/reader)]
    (reduce conj [] (line-seq rdr))))