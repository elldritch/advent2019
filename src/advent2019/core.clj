(ns advent2019.core
  (:require [advent2019.01-tyranny-of-the-rocket-equation :refer [fuel-needed]])
  (:gen-class))

(defn -main
  [& args]
  (with-open [rdr (clojure.java.io/reader (first args))]
    (println (reduce + (map #(fuel-needed (Integer/parseInt %)) (line-seq rdr))))))
