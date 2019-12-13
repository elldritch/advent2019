(ns advent2019.day-09
  (:require [advent2019.lib.intcode :as intcode]))

(defn solve! [file]
  (let [program (intcode/load-program! file)
        continuation (intcode/run-program program)
        test-mode (intcode/resume-program-with-input continuation 1)
        boost-mode (intcode/resume-program-with-input continuation 2)]
    (println "BOOST test mode outputs:" (:outputs (intcode/gather-outputs test-mode)))
    (println "BOOST sensor boost mode outputs:" (:outputs (intcode/gather-outputs boost-mode)))))
