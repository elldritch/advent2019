(ns advent2019.day-07-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent2019.day-07 :refer [thruster-signal max-thruster-signal]]))

(def test-program-1 [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0])

(def test-program-2 [3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23
                     23 4 23 99 0 0])

(def test-program-3 [3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33 1002 33
                     7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0])

(deftest amplifier-signal
  (testing "thruster signal calculation"
    (is (= 43210 (thruster-signal test-program-1 4 3 2 1 0)))
    (is (= 54321 (thruster-signal test-program-2 0 1 2 3 4)))
    (is (= 65210 (thruster-signal test-program-3 1 0 4 3 2))))
  (testing "thruster signal optimization"
    (is (= 43210 (:signal (max-thruster-signal test-program-1))))
    (is (= 54321 (:signal (max-thruster-signal test-program-2))))
    (is (= 65210 (:signal (max-thruster-signal test-program-3))))))
