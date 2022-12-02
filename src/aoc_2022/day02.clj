(ns aoc-2022.day02
  (:require [aoc-2022.core :refer [lines sum]]
            [clojure.string :refer [split]]))

(defn parse-line
  [line]
  (split line #" "))

(def input
  (->> "day02.txt"
       lines
       (map parse-line)))

(def shapes
  {:A :rock
   :B :paper
   :C :scissors
   :X :rock
   :Y :paper
   :Z :scissors})

(def shape-scores {:rock 1 :paper 2 :scissors 3})

(def outcomes
  {[:rock :rock] :draw
   [:rock :paper] :win
   [:rock :scissors] :loss
   [:paper :rock] :loss
   [:paper :paper] :draw
   [:paper :scissors] :win
   [:scissors :rock] :win
   [:scissors :paper] :loss
   [:scissors :scissors] :draw
   :X :loss
   :Y :draw
   :Z :win})

(def outcome-scores
  {:win 6 :draw 3 :loss 0})

(defn response
  [opponent-shape outcome]
  (first (drop-while
          #(not= (outcomes [opponent-shape %]) outcome)
          [:rock :paper :scissors])))

(defn score
  [round]
  (let [opponent (keyword (get round 0))
        opponent-shape (opponent shapes)
        response (keyword (get round 1))
        response-shape (response shapes)
        outcome (outcomes [opponent-shape response-shape])]
    (+ (response-shape shape-scores)
       (outcome outcome-scores))))

(defn score2
  [round]
  (let [opponent (keyword (get round 0))
        opponent-shape (opponent shapes)
        desired-outcome (keyword (get round 1))
        outcome (outcomes desired-outcome)]
    (+ (shape-scores (response opponent-shape outcome))
       (outcome outcome-scores))))

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
