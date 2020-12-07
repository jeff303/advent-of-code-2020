(ns advent-of-code.day4
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.reducers :as red]
            [clojure.spec.alpha :as s]
            [clojure.walk :as w]))

(defn- parse-passport-entries-from-lines [lines]
  (for [line lines]
    (if
      (empty? line)
      nil
      (into
        {}
        (map
          #(clojure.string/split % #":")
          (clojure.string/split line #" "))))))

(defn combine-passport-entries [entries]
  (red/map
    #(red/reduce merge %)
    (red/filter
      #(some? (first %))
      (partition-by nil? entries))))

(defn parse-passport-entries-from-input []
  (let [input-text (slurp (io/resource "input_day4"))]
    (parse-passport-entries-from-lines (str/split-lines input-text))))

(defn- hacked-valid-passport [passport]
  (let [required-keys ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]]
    (= (count required-keys) (count (select-keys passport required-keys)))))

(defn day4-part1 []
  (let [passports (into [] (combine-passport-entries (parse-passport-entries-from-input)))]
    (count (into [] (red/filter hacked-valid-passport passports)))))

; birth year must be at least 1920 and at most 2002
(s/def ::byr #(<= 1920 (Integer. %) 2002))
; issue year must be at least 2010 and at most 2020
(s/def ::iyr #(<= 2010 (Integer. %) 2020))
; expiration year must be at least 2020 and at most 2030
(s/def ::eyr #(<= 2020 (Integer. %) 2030))

; hgt (Height) - a number followed by either cm or in:
; If cm, the number must be at least 150 and at most 193.
; If in, the number must be at least 59 and at most 76.
(defn- height-valid [height]
  (let [groups (re-matches #"^([0-9]*)(cm|in)$" height)]
    (if
      (= 3 (count groups))
      (let [[_ ht-str unit] groups
            ht (Integer. ht-str)]
        (case unit
          "cm" (<= 150 ht 193)
          "in" (<= 59 ht 76)
          :default false))
      false)))
(s/def ::hgt height-valid)

;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
(s/def ::hcl #(re-matches #"#[0-9a-f]{6}" %))

;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
(s/def ::ecl #(contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} %))

;pid (Passport ID) - a nine-digit number, including leading zeroes.
(s/def ::pid #(re-matches #"[0-9]{9}" %))

;cid (Country ID) - ignored, missing or not.

; overall passport, with the required keys
(s/def ::passport (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]))

(defn day4-part2 []
  (let [passports (into [] (-> (parse-passport-entries-from-input) combine-passport-entries))
        kwized-passports (red/map w/keywordize-keys passports)]
    (count (into [] (red/filter #(s/valid? ::passport %) kwized-passports)))))
