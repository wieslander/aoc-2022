(ns aoc-2022.day13
  (:require [aoc-2022.core :refer [lines parse-int sum]]))

(defn parse-list-int
  [s]
  (let [[numbers remaining] (split-with #(contains? (set "1234567890") %) s)]
    [(parse-int (apply str numbers)) remaining]))

(defn parse-list
  [s]
  (loop [l []
         [ch & remaining :as s] (rest s)]
    (case ch
      \] [l remaining]
      \, (recur l remaining)
      \[ (let [[nested-list remaining] (parse-list s)]
           (recur (conj l nested-list) remaining))
      (let [[value remaining] (parse-list-int s)]
        (recur (conj l value) remaining)))))

(defn parse-packet
  [line]
  (first (parse-list (seq line))))

(declare compare-values)

(defn compare-lists
  [left right]
  (let [[left-item & left-remainder] left
        [right-item & right-remainder] right]
    (cond
      (and (nil? left-item) (nil? right-item))
      0

      (and (nil? left-item) (not (nil? right-item)))
      -1

      (and (nil? right-item) (not (nil? left-item)))
      1

      :else
      (let [comparison (compare-values left-item right-item)]
        (if (zero? comparison)
          (recur left-remainder right-remainder)
          comparison)))))

(defn compare-ints
  [left right]
  (cond
    (< left right) -1
    (> left right) 1
    :else 0))

(defn compare-values
  [left right]
  (cond
    (and (int? left) (int? right))
    (compare-ints left right)

    (and (int? left) (not (int? right)))
    (recur [left] right)

    (and (not (int? left)) (int? right))
    (recur left [right])

    :else (compare-lists left right)))

(def input
  (->> "day13.txt"
       lines
       (filter seq)
       (map parse-packet)))

(def part1
  (->> input
       (partition 2)
       (map #(apply compare-values %))
       (keep-indexed #(if (= %2 -1) (inc %1) nil))
       sum))

(def part2
  (->> (conj input [[2]] [[6]])
       (sort compare-values)
       (keep-indexed #(if (or (= %2 [[2]]) (= %2 [[6]])) (inc %1) nil))
       (apply *)))

(defn -main
  []
  (println part1)
  (println part2))
