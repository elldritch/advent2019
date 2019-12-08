(ns advent2019.day-07
  (:require [advent2019.lib.intcode :refer [run-program]]))

(defn thruster-signal [program a b c d e]
  (reduce (fn [signal phase]
            (first (:outputs (run-program program [signal phase]))))
          0 [a b c d e]))
