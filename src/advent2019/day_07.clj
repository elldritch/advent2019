(ns advent2019.day-07
  (:require [advent2019.lib.intcode :refer [run-program load-program!]]))

(defn thruster-signal [program a b c d e]
  (reduce (fn [signal phase]
            (first (:outputs (run-program program [signal phase]))))
          0 [a b c d e]))

(defn permutations [xs]
  (if (= 1 (count xs))
    [[(first xs)]]
    (let [elements (set xs)]
      (apply concat
             (mapv
              (fn [x] (mapv #(into [x] %) (permutations (disj elements x))))
              elements)))))

(defn max-thruster-signal [program]
  (apply max-key :signal
         (map #(assoc {:phases %} :signal (apply (partial thruster-signal program) %))
              (permutations [0 1 2 3 4]))))

(defn solve! [file]
  (println "Max thruster signal:" (max-thruster-signal (load-program! file))))
