(ns aoc-2022.day05
  (:require [aoc-2022.core :refer [lines parse-int]])
  (:require [clojure.string :as str]))

(defn get-stack-count
  [lines]
  (->> lines
       (take-while #(not (str/blank? %)))
       last
       str/trim
       (#(last (str/split % #" ")))
       parse-int))

(defn parse-item
  [line stack-index]
  (let [item-pos (inc (* stack-index 4))]
    (when (> (count line) item-pos)
      (let [item (nth line item-pos)]
        (when (and (>= (int item) (int \A))
                   (<= (int item) (int \Z)))
          item)))))

(defn parse-items
  [line stack-count]
  (->> (range stack-count)
       (map #(parse-item line %))))

(defn push-item
  [stack item]
  (if (nil? item)
    stack
    (conj stack item)))

(defn parse-stacks
  [lines stack-count]
  (let [initial-stacks (vec (take stack-count (repeatedly list)))
        stack-lines (take-while #(not (nil? (str/index-of % "["))) lines)]
    (loop [[line & remaining-lines] (reverse stack-lines)
           stacks initial-stacks]
      (if (nil? line)
        stacks
        (let [items (parse-items line stack-count)]
          (recur remaining-lines (mapv push-item stacks items)))))))

(defn parse-instruction
  [line]
  (let [[_, item-count, _, source, _, dest] (str/split line #" ")]
    {:item-count (parse-int item-count)
     :source (parse-int source)
     :dest (parse-int dest)}))

(defn parse-instructions
  [lines]
  (->> lines
       (drop-while #(not (str/starts-with? % "move")))
       (map parse-instruction)))

(defn parse-input
  [lines]
  (let [stack-count (get-stack-count lines)
        stacks (parse-stacks lines stack-count)
        instructions (parse-instructions lines)]
    {:stacks stacks :instructions instructions}))

(def input
  (->> "day05.txt"
       lines
       parse-input))

(defn move
  [stacks instruction reverse?]
  (if (zero? (:item-count instruction))
    stacks
    (let [{:keys [source, dest, item-count]} instruction
          source-index (dec source)
          dest-index (dec dest)
          source-stack (get stacks source-index)
          dest-stack (get stacks dest-index)
          items (take item-count source-stack)
          ordered-items (if reverse? (reverse items) items)
          remaining (drop item-count source-stack)]
      (assoc stacks
             source-index remaining
             dest-index (concat ordered-items dest-stack)))))

(defn move-items
  [stacks [instruction & remaining-instructions] reverse?]
  (if (nil? instruction)
    stacks
    (recur (move stacks instruction reverse?)
           remaining-instructions
           reverse?)))

(defn solve
  [& {:keys [reverse?]}]
  (->> (move-items (:stacks input) (:instructions input) reverse?)
       (map first)
       (filter #(not (nil? %)))
       (apply str)))

(def part1 (solve {:reverse? true}))

(def part2 (solve {:reverse? false}))

(defn -main
  []
  (println part1)
  (println part2))
