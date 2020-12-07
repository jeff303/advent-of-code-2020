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
