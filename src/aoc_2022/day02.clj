(ns aoc-2022.day02
  (:require [aoc-2022.core :refer [lines sum]]
            [clojure.string :refer [split]]))

(defn parse-line
  [line]
  (mapv keyword (split line #" ")))

(def input
  (->> "day02.txt"
       lines
       (map parse-line)))

(def shapes [:rock :paper :scissors])

(def shapes-from-input
  {:A :rock
   :B :paper
   :C :scissors
   :X :rock
   :Y :paper
   :Z :scissors})

(def what-beats
  {:rock :paper
   :paper :scissors
   :scissors :rock})

(defn outcome
  [elf response]
  (cond
    (= elf response) :draw
    (= response (what-beats elf)) :win
    (= elf (what-beats response)) :loss))

(def shape-scores {:rock 1 :paper 2 :scissors 3})

(def outcome-scores
  {:win 6 :draw 3 :loss 0})

(def desired-outcomes {:X :loss :Y :draw :Z :win})

(defn find-response
  [elf desired-outcome]
  (->> shapes
       (filter #(= (outcome elf %)
                   desired-outcome))
       first))

(defn score
  [round]
  (let [elf (shapes-from-input (get round 0))
        response (shapes-from-input (get round 1))]
    (+ (shape-scores response)
       (outcome-scores (outcome elf response)))))

(defn score2
  [round]
  (let [elf (shapes-from-input (get round 0))
        desired-outcome (desired-outcomes (get round 1))
        response (find-response elf desired-outcome)]
    (+ (shape-scores response)
       (outcome-scores desired-outcome))))

(def part1
  (->> input
       (map score)
       sum))

(def part2
  (->> input
       (map score2)
       sum))

(defn -main
  []
  (println part1)
  (println part2))
