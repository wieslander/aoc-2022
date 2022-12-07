(ns aoc-2022.day07
  (:require [clojure.string :as str])
  (:require [aoc-2022.core :refer [lines parse-int sum]]))

(defn parse-directory-entry
  [line]
  (let [[info path] (str/split line #" ")]
    (if (= info "dir")
      [path {:type :directory, :path path}]
      [path {:type :file, :path path, :size (parse-int info)}])))

(defn parse-output
  [lines]
  (->> lines
       (map parse-directory-entry)
       (into {})))

(defn parse-command
  [lines]
  (let [[line & remaining-lines] lines
        [_, command, arg] (str/split line #" ")
        [output-lines remaining-lines]
        (split-with #(not (str/starts-with? % "$")) remaining-lines)
        parsed-output (parse-output output-lines)]
    [{:command (keyword command), :arg arg, :output parsed-output}
     remaining-lines]))

(defn parse-commands
  [lines]
  (loop [commands []
         lines lines]
    (if (empty? lines)
      commands
      (let [[command remaining-lines] (parse-command lines)]
        (recur (conj commands command) remaining-lines)))))

(defn set-children
  [tree subdirectories contents]
  (if (empty? subdirectories)
    (if (nil? (:contents tree))
      (assoc tree :contents contents)
      tree)
    (let [[next-directory & next-subdirectories] subdirectories
          next-tree (get (:contents tree) next-directory)]
      (assoc tree
             :contents (assoc (:contents tree)
                              next-directory (set-children next-tree next-subdirectories contents))))))

(defn build-tree
  [commands]
  (loop [tree {:type :directory, :path "/", :contents nil}
         [command & remaining-commands] commands
         directory-stack (list "/")]
    (if (nil? command)
      tree
      (case (:command command)
        :cd (case (:arg command)
              "/" (recur tree remaining-commands (list "/"))
              ".." (recur tree remaining-commands (rest directory-stack))
              (recur tree remaining-commands (conj directory-stack (:arg command))))
        :ls (recur (set-children tree (rest (reverse directory-stack)) (:output command))
                   remaining-commands directory-stack)))))

(defn assign-total-sizes
  [node]
  (let [{:keys [type contents]} node]
    (case type
      :file node
      :directory (let [new-contents (map assign-total-sizes (vals contents))]
                   (assoc node
                          :contents (into {} (map #(vector (:path %) %) new-contents))
                          :size (sum (map :size new-contents)))))))

(def filesystem
  (->> "day07.txt"
       lines
       parse-commands
       build-tree
       assign-total-sizes))

(defn traverse-directories
  [tree]
  (let [{:keys [contents]} tree
        directories (filter #(= (:type %) :directory) (vals contents))]
    (cons tree (apply concat (map traverse-directories directories)))))

(defn sum-small-directories
  [tree]
  (->> tree
       traverse-directories
       (map :size)
       (filter #(<= % 100000))
       sum))

(defn find-directory-to-delete
  [tree]
  (let [unused-space (- 70000000 (:size tree))
        min-directory-size (- 30000000 unused-space)]
    (->> tree
         traverse-directories
         (map :size)
         (filter #(>= % min-directory-size))
         (apply min))))

(def part1 (sum-small-directories filesystem))

(def part2 (find-directory-to-delete filesystem))

(defn -main
  []
  (println part1)
  (println part2))
