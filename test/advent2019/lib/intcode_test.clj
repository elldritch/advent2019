(ns advent2019.lib.intcode-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent2019.lib.intcode :refer [run-program]]))

(deftest intcode
  (testing "addition and multiplication instructions"
    (is (= (:state (run-program [1 9 10 3 2 3 11 0 99 30 40 50] nil))
           [3500 9 10 70 2 3 11 0 99 30 40 50]))
    (is (= (:state (run-program [1 0 0 0 99] nil))
           [2 0 0 0 99]))
    (is (= (:state (run-program [2 3 0 3 99] nil))
           [2 3 0 6 99]))
    (is (= (:state (run-program [2 4 4 5 99 0] nil))
           [2 4 4 5 99 9801]))
    (is (= (:state (run-program [1 1 1 4 99 5 6 0 99] nil))
           [30 1 1 4 2 5 6 0 99]))))
