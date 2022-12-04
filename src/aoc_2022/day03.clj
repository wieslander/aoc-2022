(ns aoc-2022.day03
  (:require [clojure.set :refer [intersection]])
  (:require [aoc-2022.core :refer [lines sum]]))

(defn compartments
  [line]
  (let [compartment-size (/ (count line) 2)]
    [(take compartment-size line)
     (drop compartment-size line)]))

(def input (lines "day03.txt"))

(defn common-item-type
  [[compartment1 compartment2]]
  (first (intersection (set compartment1)
                       (set compartment2))))

(defn badge
  [[elf1 elf2 elf3]]
  (first (intersection (set elf1)
                       (set elf2)
                       (set elf3))))

(defn item-type-priority
  [item-type]
  (let [is-upper (Character/isUpperCase item-type)
        base-priority (if is-upper 27 1)
        offset (if is-upper (int \A) (int \a))]
    (+ base-priority (- (int item-type) offset))))

(def part1
  (->> input
       (map compartments)
       (map common-item-type)
       (map item-type-priority)
       sum))

(def part2
  (->> input
       (partition 3)
       (map badge)
       (map item-type-priority)
       sum))

(defn -main
  []
  (println part1)
  (println part2))
