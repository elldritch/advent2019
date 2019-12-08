(ns advent2019.day-07
  (:require [advent2019.lib.intcode :refer [run-program
                                            load-program!
                                            resume-program-with-input]]))

(defn thruster-signal [program a b c d e]
  (reduce (fn [signal phase]
            (let [asks-for-phase (run-program program)
                  asks-for-signal (resume-program-with-input asks-for-phase phase)
                  has-output (resume-program-with-input asks-for-signal signal)]
              (:output has-output)))
          0 [a b c d e]))

(defn permutations [xs]
  (if (= 1 (count xs))
    [[(first xs)]]
    (let [elements (set xs)]
      (vec (mapcat
            (fn [x] (mapv #(into [x] %) (permutations (disj elements x))))
            elements)))))

(defn max-thruster-signal [program]
  (apply max-key :signal
         (map #(assoc {:phases %} :signal (apply (partial thruster-signal program) %))
              (permutations [0 1 2 3 4]))))

(defn solve! [file]
  (println "Max thruster signal:" (max-thruster-signal (load-program! file))))
