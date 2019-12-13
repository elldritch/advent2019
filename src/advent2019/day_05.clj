(ns advent2019.day-05
  (:require [advent2019.lib.intcode :as intcode]))

(defn tests-ok? [outputs]
  (zero? (reduce + 0 (take (dec (count outputs)) outputs))))

(defn solve! [file]
  (let [program (intcode/load-program! file)
        ac-outputs (:outputs (intcode/gather-outputs (intcode/resume-program-with-input (intcode/run-program program) 1)))
        radiator-outputs (:outputs (intcode/gather-outputs (intcode/resume-program-with-input (intcode/run-program program) 5)))]
    (println "Air conditioner outputs:" ac-outputs)
    (println "All tests zero?:" (tests-ok? ac-outputs))
    (println "Diagnostic code:" (last ac-outputs))
    (println "Thermal radiator outputs:" radiator-outputs)
    (println "All tests zero?:" (tests-ok? radiator-outputs))
    (println "Diagnostic code:" (last radiator-outputs))))
