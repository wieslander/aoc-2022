(ns aoc-2022.day06
  (:require [clojure.java.io :refer [resource]]))

(def input
  (->> "day06.txt"
       resource
       slurp))

(defn marker?
  [s]
  (= (count (set s))
     (count s)))

(defn find-marker-end
  [input marker-length]
  (let [windows (partition marker-length 1 input)]
    (loop [[window & remaining-windows] windows
           offset 0]
      (if (marker? window)
        (+ marker-length offset)
        (recur remaining-windows (inc offset))))))

(def part1 (find-marker-end input 4))

(def part2 (find-marker-end input 14))

(defn -main
  []
  (println part1)
  (println part2))
