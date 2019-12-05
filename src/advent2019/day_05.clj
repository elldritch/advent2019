(ns advent2019.day-05
  (:require [advent2019.lib.intcode :refer [load-program! run-program]]))

(defn solve! [file]
  (let [outputs (:outputs (run-program (load-program! file) 1))]
    (println "Outputs:" outputs)
    (println "All tests zero?:" (= 0 (reduce + 0 (take (dec (count outputs)) outputs))))
    (println "Diagnostic code:" (last outputs))))
