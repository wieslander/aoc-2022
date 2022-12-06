(ns aoc-2022.day06
  (:require [clojure.java.io :refer [resource]]))

(def input
  (->> "day06.txt"
       resource
       slurp))

(defn start-of-packet-marker?
  [s]
  (= (count (set s))
     (count s)))

(defn find-marker-end
  [input marker-length]
  (if (start-of-packet-marker? (take marker-length input))
    marker-length
    (inc (find-marker-end (rest input) marker-length))))

(def part1 (find-marker-end input 4))

(def part2 (find-marker-end input 14))

(defn -main
  []
  (println part1)
  (println part2))
