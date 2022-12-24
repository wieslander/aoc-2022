(ns aoc-2022.day12
  (:require [aoc-2022.core :refer [lines manhattan-distance]])
  (:require [aoc-2022.search :refer [a-star]]))

(defn coord-map
  [vec-grid]
  (let [height (count vec-grid)
        width (count (get vec-grid 0))]
    (apply merge
           (for [x (range width)
                 y (range height)
                 :let [value (get (get vec-grid y) x)]]
             {[x y] value}))))

(def grid
  (->> "day12.txt"
       lines
       (mapv vec)
       coord-map))

(defn find-values
  [grid v]
  (->> (seq grid)
       (filter (fn [[_ value]] (= value v)))
       (map first)))

(defn find-value
  [grid v]
  (first (find-values grid v)))

(defn find-start
  [grid]
  (find-value grid \S))

(defn find-goal
  [grid]
  (find-value grid \E))

(defn find-starts
  [grid]
  (conj (find-values grid \a) (find-value grid \S)))

(defn get-elevation
  [grid pos]
  (let [value (get grid pos)]
    (case value
      nil nil
      \S (int \a)
      \E (int \z)
      (int value))))

(defn neighbors
  [grid [x y]]
  (let [offsets [[0 -1] [0 1] [-1 0] [1 0]]
        elevation (get-elevation grid [x y])]
    (->> offsets
         (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
         (filter (fn [neighbor]
                   (let [neighbor-elevation (get-elevation grid neighbor)]
                     (and (not (nil? neighbor-elevation))
                          (or (<= neighbor-elevation elevation)
                              (= neighbor-elevation (inc elevation)))))))
         (map (fn [pos] [pos 1])))))

(defn find-best-path
  [grid start goal]
  (a-star start
          (partial neighbors grid)
          (partial = goal)
          (partial manhattan-distance goal)))

(defn find-path-from-start
  [grid]
  (let [start (find-start grid)
        goal (find-goal grid)]
    (find-best-path grid start goal)))

(defn find-path-from-best-start
  [grid]
  (let [goal (find-goal grid)
        starts (find-starts grid)]
    (println starts)
    (->> starts
         (map #(find-best-path grid % goal))
         (filter seq)
         (sort-by count)
         first)))

(def part1
  (->> grid
       find-path-from-start
       count
       dec))

(def part2
  (->> grid
       find-path-from-best-start
       count
       dec))

(defn -main
  []
  (println part1)
  (println part2))
