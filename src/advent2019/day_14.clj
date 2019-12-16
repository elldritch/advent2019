(ns advent2019.day-14
  (:require [clojure.string :as s]))

(defn parse-element [input]
  (let [[quantity ingredient] (-> (s/trim input)
                                  (s/split #" "))]
    {:quantity (Integer/parseInt quantity)
     :ingredient ingredient}))

(defn parse-element-list [input]
  (->> (s/split input #",")
       (mapv parse-element)))

(defn parse-equation [input]
  (let [[inputs output] (s/split input #"=>")]
    {:inputs (parse-element-list inputs)
     :output (parse-element output)}))

(defn parse-recipes [input]
  (->> input
       (s/split-lines)
       (mapv parse-equation)))

(defn load-file! [file] (parse-recipes (slurp file)))
