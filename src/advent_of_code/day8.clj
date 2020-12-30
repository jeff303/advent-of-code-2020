(ns advent-of-code.day8
  (:gen-class)
  (:require [advent-of-code.util :as util]))

(defn input-line-to-program-inst [input-line]
  (subvec (re-matches #"^([a-z]{3}) (-|\+)(\d+)" input-line) 1))

(defn parse-inputs-to-program [res-file]
  (let [input-lines (util/read-problem-input-as-lines res-file)]
    (map input-line-to-program-inst input-lines)))

(defn parse-test-inputs-to-program
  ([]
   (parse-test-inputs-to-program ""))
  ([variant]
   (parse-inputs-to-program
     (util/get-day-test-input *ns* variant))))

(defn run-program [program acc pc seen-inst]
  (cond
    (>= pc (count program)) ; finished by executing the last instruction
    [acc true]
    (contains? seen-inst pc) ; infinite loop (already seen pc)
    [acc false]
    :default
    (let [inst (nth program pc)
          [op sign num-st] inst
          num (Integer. num-st)
          new-seen-inst (conj seen-inst pc)
          acc-fn (case sign "+" + "-" -)]
      (case op
        "nop"
        (run-program program acc (inc pc) new-seen-inst)
        "acc"
        (run-program program (acc-fn acc num) (inc pc) new-seen-inst)
        "jmp"
        (run-program program acc (acc-fn pc num) new-seen-inst)))))

(defn day8-part1 []
  (let [prog (into [] (parse-inputs-to-program (util/get-day-input *ns*)))]
    (first (run-program prog 0 0 #{}))))

(defn fix-program [program]
  (let [tweak-inst-fn (fn [pc inst]
                        (let [[op sign num-st] inst]
                          (case op
                            "nop"
                            [pc ["jmp" sign num-st]] ; flip acc to jmp
                            "jmp"
                            [pc ["nop" sign num-st]] ; flip jmp to acc
                            nil)))
        modified-instructions (keep-indexed tweak-inst-fn program)]
    (first
      (filter
        some?
        (for [mod-inst modified-instructions]
          (let [[mod-pc inst] mod-inst
                mod-prog (concat (take mod-pc program) [inst] (drop (inc mod-pc) program))
                [acc finished] (run-program mod-prog 0 0 #{})]
            (if finished acc)))))))

(defn day8-part2 []
  (let [prog (into [] (parse-inputs-to-program (util/get-day-input *ns*)))]
    (fix-program prog)))
