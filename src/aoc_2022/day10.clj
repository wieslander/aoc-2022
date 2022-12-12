(ns aoc-2022.day10
  (:require [clojure.string :refer [split]])
  (:require [aoc-2022.core :refer [lines parse-int sum]]))

(defn parse-instruction
  [line]
  (let [[instruction arg] (split line #" ")]
    {:instruction (keyword instruction)
     :value (when (= instruction "addx") (parse-int arg))}))

(def input
  (->> "day10.txt"
       lines
       (map parse-instruction)))

(defn run-program
  [instructions]
  (loop [cycle 1
         x 1
         [{:keys [instruction value]} & remaining] instructions
         cycle-log []]
    (let [log-entry {:cycle cycle, :x x, :instruction instruction}]
      (case instruction
        nil cycle-log
        :noop (recur (inc cycle) x remaining (conj cycle-log log-entry))
        :addx (recur (+ cycle 2) (+ x value) remaining (conj cycle-log log-entry (assoc log-entry :cycle (inc cycle))))))))

(defn signal-strength
  [log-entry]
  (* (:cycle log-entry) (:x log-entry)))

(defn render-pixel
  [{:keys [cycle x]}]
  (let [column (dec (mod cycle 40))
        visible? (<= (abs (- column x)) 1)]
    (if visible? "#" ".")))

(defn render-row
  [cycle-log]
  (->> cycle-log
       (map render-pixel)
       (apply str)))

(def part1
  (->> input
       run-program
       (drop 19)
       (take-nth 40)
       (map signal-strength)
       sum))

(def part2
  (->> input
       run-program
       (partition 40)
       (map render-row)))

(defn -main
  []
  (println part1)
  (run! println part2))
