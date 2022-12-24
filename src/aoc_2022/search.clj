(ns aoc-2022.search
  (:require [clojure.data.priority-map :refer [priority-map-keyfn]])
  (:gen-class))

(defn backtrack
  [node visited]
  (loop [route (list)
         node node]
    (if (contains? visited node)
      (recur (cons node route) (visited node))
      route)))

(defn a-star
  [start next-states goal? h]
  (loop [visited {}
         frontier (priority-map-keyfn first start [(h start) 0 nil])]
    (when (seq frontier)
      (let [[current [_ current-score previous]] (peek frontier)
            visited (assoc visited current previous)]
        (if (goal? current)
          (backtrack current visited)
          (recur visited
                 (reduce (fn [frontier [neighbor edge-cost]]
                           (let [prev-score (get-in frontier [neighbor 1] Integer/MAX_VALUE)
                                 score (+ current-score edge-cost)]
                             (if (and (not (contains? visited neighbor))
                                      (< score prev-score))
                               (assoc frontier neighbor [(+ score (h neighbor)) score current])
                               frontier)))
                         (pop frontier)
                         (next-states current))))))))
