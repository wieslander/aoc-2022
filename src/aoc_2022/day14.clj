(ns aoc-2022.day14
  (:require [clojure.set :refer [union]])
  (:require [clojure.string :as str])
  (:require [aoc-2022.core :refer [lines parse-int]]))

(defn parse-coord
  [s]
  (mapv parse-int (str/split s #",")))

(defn parse-endpoints
  [line]
  (->> (str/split line #" -> ")
       (map parse-coord)))

(defn next-coord
  [coord end-coord]
  (let [delta (- end-coord coord)]
    (if (= delta 0)
      coord
      (+ coord (/ delta (abs delta))))))

(defn next-pos
  [pos end]
  (mapv next-coord pos end))

(defn rock-segment
  [start end]
  (loop [segment #{}
         pos start]
    (let [segment (conj segment pos)]
      (if (= pos end)
        segment
        (recur segment (next-pos pos end))))))

(defn rock-path
  [line]
  (->> line
       parse-endpoints
       (partition 2 1)
       (map #(apply rock-segment %))
       (apply union)))

(def input
  (->> "day14.txt"
       lines
       (map rock-path)
       (apply union)))

(defn free?
  [grid floor pos]
  (let [[_ y] pos]
    (and (not (contains? grid pos))
         (or (nil? floor) (< y floor)))))

(defn below
  [[x y]]
  [x (inc y)])

(defn below-left
  [[x y]]
  [(dec x) (inc y)])

(defn below-right
  [[x y]]
  [(inc x) (inc y)])

(defn find-bottom
  [grid]
  (apply max (map last grid)))

(defn find-floor
  [grid]
  (+ (find-bottom grid) 2))

(defn below?
  [[_ y] bottom]
  (> y bottom))

(defn drop-sand
  ([grid] (drop-sand grid nil))
  ([grid floor]
   (let [bottom (find-bottom grid)]
     (loop [grid grid
            sand [500 0]]
       (cond
         (not (free? grid floor sand)) grid
         (and (nil? floor) (below? sand bottom)) grid
         (free? grid floor (below sand)) (recur grid (below sand))
         (free? grid floor (below-left sand)) (recur grid (below-left sand))
         (free? grid floor (below-right sand)) (recur grid (below-right sand))
         :else (conj grid sand))))))

(def part1
  (->> (iterate drop-sand input)
       (partition 2 1)
       (take-while (fn [pair] (apply not= pair)))
       (map first)
       count))

(def part2
  (->> (iterate #(drop-sand % (find-floor input)) input)
       (partition 2 1)
       (take-while (fn [pair] (apply not= pair)))
       (map first)
       count))

(defn -main
  []
  (println part1)
  (println part2))
