(ns aoc-2022.day08
  (:require [clojure.string :as str])
  (:require [aoc-2022.core :refer [lines parse-int]]))

(def grid
  (->> "day08.txt"
       lines
       (map #(str/split % #""))
       (mapv #(mapv parse-int %))))

(defn height
  [grid]
  (count grid))

(defn width
  [grid]
  (count (get grid 0)))

(defn tree-height
  [x y]
  (-> grid
      (get y)
      (get x)))

(defn visible?
  [x y]
  (let [h (tree-height x y)]
    (or
     (->> (range x)
          (map #(tree-height % y))
          (every? #(< % h)))
     (->> (range (inc x) (width grid))
          (map #(tree-height % y))
          (every? #(< % h)))
     (->> (range y)
          (map #(tree-height x %))
          (every? #(< % h)))
     (->> (range (inc y) (height grid))
          (map #(tree-height x %))
          (every? #(< % h))))))

(defn get-visibilities
  []
  (for [x (range (width grid))
        y (range (height grid))]
    (visible? x y)))

(defn count-visible
  [max-height tree-coords]
  (->> tree-coords
       (map #(apply tree-height %))
       (split-with #(< % max-height))
       (apply (fn [visible blocking]
                (if (empty? blocking)
                  (count visible)
                  (inc (count visible)))))))

(defn get-viewing-distances
  [x y]
  (let [h (tree-height x y)]
    [(->> (range (dec x) -1 -1)
          (map #(vector % y))
          (count-visible h))
     (->> (range (inc x) (width grid))
          (map #(vector % y))
          (count-visible h))
     (->> (range (dec y) -1 -1)
          (map #(vector x %))
          (count-visible h))
     (->> (range (inc y) (width grid))
          (map #(vector x %))
          (count-visible h))]))

(defn get-scenic-score
  [x y]
  (apply * (get-viewing-distances x y)))

(defn get-scenic-scores
  []
  (for [x (range 1 (dec (width grid)))
        y (range 1 (dec (height grid)))]
    (get-scenic-score x y)))

(def part1
  (->> (get-visibilities)
       (filter identity)
       count))

(def part2
  (->> (get-scenic-scores)
       (apply max)))

(defn -main
  []
  (println part1)
  (println part2))
