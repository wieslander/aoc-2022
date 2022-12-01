(ns aoc-2022.day01
  (:require [aoc-2022.core :refer [lines parse-int sum]]))

(def input
  (->> "day01.txt"
       lines
       (partition-by #(= "" %))
       (filter #(not= '("") %))
       (map #(map parse-int %))))

(def part1
  (->> input
       (map sum)
       (apply max)))

(def part2
  (->> input
       (map sum)
       (sort >)
       (take 3)
       sum))

(defn -main
  []
  (println part1)
  (println part2))
