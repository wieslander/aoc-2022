(ns aoc-2022.day11
  (:require [clojure.string :as str])
  (:require [aoc-2022.core :refer [lines parse-int]]))

(defn parse-index
  [index-line]
  (-> index-line
      (str/replace ":" "")
      (str/split #" ")
      last
      parse-int))

(defn parse-ints
  [strings]
  (mapv #(bigint (parse-int %)) strings))

(defn parse-items
  [line]
  (-> line
      (str/split #": ")
      last
      (str/split #", ")
      parse-ints))

(defn parse-operator
  [op]
  (case op
    "*" (fn [a b] (* a b))
    "+" (fn [a b] (+ a b))))

(defn parse-operand
  [old-value operand]
  (if (= operand "old")
    old-value
    (parse-int operand)))

(defn generate-operation
  [op left-operand right-operand]
  (fn [old]
    (let [operator (parse-operator op)
          operand (partial parse-operand old)
          result (operator (operand left-operand) (operand right-operand))]
      result)))

(defn parse-operation
  [line]
  (let [op-body (last (str/split line #"= "))
        [left-operand op right-operand] (str/split op-body #" ")]
    (generate-operation op left-operand right-operand)))

(defn parse-trailing-int
  [line]
  (-> line
      (str/split #" ")
      last
      parse-int))

(defn parse-monkey
  [monkey-lines]
  (let [[index-line items-line op-line test-line true-line false-line] monkey-lines]
    {:index (parse-index index-line)
     :initial-state {:items (parse-items items-line) :inspection-count 0}
     :operation (parse-operation op-line)
     :divisor (parse-trailing-int test-line)
     :true-monkey (parse-trailing-int true-line)
     :false-monkey (parse-trailing-int false-line)}))

(defn parse-monkeys
  [lines]
  (->> lines
       (filter (complement empty?))
       (partition 6)
       (map parse-monkey)))

(def input
  (->> "day11.txt"
       lines
       parse-monkeys))

(defn inspect
  [monkey-state]
  (assoc monkey-state
         :items (rest (:items monkey-state))
         :inspection-count (inc (:inspection-count monkey-state))))

(defn throw-item
  [worry-level target-monkey-state]
  (assoc target-monkey-state
         :items (conj (:items target-monkey-state) worry-level)))

(defn process-item
  [monkey state divide-by-three? mod-divisor]
  (let [monkey-index (:index monkey)
        monkey-state (get state monkey-index)
        item (first (:items monkey-state))
        pre-inspection-worry-level ((:operation monkey) item)
        worry-level (if divide-by-three?
                      (quot pre-inspection-worry-level 3)
                      (mod pre-inspection-worry-level mod-divisor))
        divisible? (zero? (mod worry-level (:divisor monkey)))
        target-index (if divisible? (:true-monkey monkey) (:false-monkey monkey))
        target-monkey-state (get state target-index)
        new-monkey-state (inspect monkey-state)
        new-target-monkey-state (throw-item worry-level target-monkey-state)]
    (assoc state
           monkey-index new-monkey-state
           target-index new-target-monkey-state)))

(defn monkey-turn
  [monkey initial-state divide-by-three? mod-divisor]
  (loop [state initial-state]
    (let [monkey-state (get state (:index monkey))]
      (if (empty? (:items monkey-state))
        state
        (recur (process-item monkey state divide-by-three? mod-divisor))))))

(defn monkey-round
  [monkeys initial-state divide-by-three? mod-divisor]
  (loop [[monkey & remaining-monkeys] monkeys
         state initial-state]
    (if (nil? monkey)
      state
      (recur remaining-monkeys (monkey-turn monkey state divide-by-three? mod-divisor)))))

(defn simulate-monkey-rounds
  [monkeys divide-by-three?]
  (let [mod-divisor (apply * (map :divisor monkeys))]
    (iterate #(monkey-round monkeys % divide-by-three? mod-divisor)
             (mapv :initial-state monkeys))))

(defn monkey-business
  [state]
  (->> state
       (map :inspection-count)
       (sort >)
       (take 2)
       (apply *)))

(def part1
  (-> input
      (simulate-monkey-rounds true)
      (nth 20)
      monkey-business))

(def part2
  (-> input
      (simulate-monkey-rounds false)
      (nth 10000)
      monkey-business))

(defn -main
  []
  (println part1)
  (println part2))
