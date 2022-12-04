(ns aoc-2022.day04
  (:require [clojure.string :refer [split]])
  (:require [aoc-2022.core :refer [lines parse-int]]))

(defn parse-range
  [range-string]
  (let [[start end] (split range-string #"-")]
    {:start (parse-int start) :end (parse-int end)}))

(defn parse-line
  [line]
  (mapv parse-range (split line #",")))

(def input
  (->> "day04.txt"
       lines
       (map parse-line)))

(defn subset-range?
  [r1 r2]
  (or (and (>= (:start r1) (:start r2))
           (<= (:end r1) (:end r2)))
      (and (>= (:start r2) (:start r1))
           (<= (:end r2) (:end r1)))))

(defn overlaps?
  [r1 r2]
  (or (and (>= (:start r1) (:start r2))
           (<= (:start r1) (:end r2)))
      (and (>= (:start r2) (:start r1))
           (<= (:start r2) (:end r1)))))

(def part1
  (->> input
       (filter (fn [[r1 r2]] (subset-range? r1 r2)))
       count))

(def part2
  (->> input
       (filter (fn [[r1 r2]] (overlaps? r1 r2)))
       count))

(defn -main
  []
  (println part1)
  (println part2))
