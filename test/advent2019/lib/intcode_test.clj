(ns advent2019.lib.intcode-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent2019.lib.intcode :refer [run-program]]))

(defn run [state expected]
  (is (= (:state (run-program state nil))) expected))

(deftest intcode
  (testing "addition and multiplication instructions"
    (run [1 9 10 3 2 3 11 0 99 30 40 50]
         [3500 9 10 70 2 3 11 0 99 30 40 50])
    (run [1 0 0 0 99]
         [2 0 0 0 99])
    (run [2 3 0 3 99]
         [2 3 0 6 99])
    (run [2 4 4 5 99 0]
         [2 4 4 5 99 9801])
    (run [1 1 1 4 99 5 6 0 99]
         [30 1 1 4 2 5 6 0 99]))
  (testing "parameter modes"
    (run [1002 4 3 4 33]
         [1002 4 3 4 99])
    (run [1101 100 -1 4 0]
         [1101 100 -1 4 99])))
