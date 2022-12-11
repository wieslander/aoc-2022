(ns aoc-2022.day09
  (:require [clojure.string :as str])
  (:require [aoc-2022.core :refer [lines parse-int]]))

(defn parse-instruction
  [line]
  (let [[direction raw-steps] (str/split line #" ")]
    [direction (parse-int raw-steps)]))

(def input
  (->> "day09.txt"
       lines
       (map parse-instruction)))

(defn move-head
  [[x y] direction]
  (case direction
    "U" [x (dec y)]
    "D" [x (inc y)]
    "L" [(dec x) y]
    "R" [(inc x) y]))

(defn move-knot
  [[x y] [head-x head-y]]
  (let [dx (- head-x x)
        dy (- head-y y)]
    [x y]
    (cond
      (and (<= (abs dx) 1) (<= (abs dy) 1)) [x y]
      (= (abs dx) (abs dy) 2) [(- head-x (/ dx 2)) (- head-y (/ dy 2))]
      (= dx 2) [(dec head-x) head-y]
      (= dy 2) [head-x (dec head-y)]
      (= dx -2) [(inc head-x) head-y]
      (= dy -2) [head-x (inc head-y)])))

(defn move-tail
  [tail new-head]
  (let [[knot & remaining] tail]
    (if (nil? knot)
      tail
      (let [new-knot (move-knot knot new-head)]
        (cons new-knot (move-tail remaining new-knot))))))

(defn move-rope-once
  [direction rope]
  (let [[head & tail] rope
        new-head (move-head head direction)
        new-tail (move-tail tail new-head)]
    (cons new-head new-tail)))

(defn move-rope
  [backtrack-path [direction steps]]
  (loop [path backtrack-path
         step 0]
    (if (>= step steps)
      path
      (recur (cons (move-rope-once direction (first path)) path) (inc step)))))

(defn run-rope-simulation
  [rope-length instructions]
  (reduce move-rope (list (repeat rope-length [0 0])) instructions))

(def part1
  (->> input
       (run-rope-simulation 2)
       (map last)
       set
       count))

(def part2
  (->> input
       (run-rope-simulation 10)
       (map last)
       set
       count))

(defn -main
  []
  (println part1)
  (println part2))
