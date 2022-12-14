(ns aoc-2022.core
  (:require [clojure.java.io :refer [resource]])
  (:require [clojure.string :refer [split-lines]])
  (:gen-class))

(defn lines
  [name]
  (->> name
       (resource)
       (slurp)
       (split-lines)))

(defn parse-int
  [number-string]
  (Integer/parseInt number-string 10))

(defn sum
  [numbers]
  (reduce + numbers))

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))
