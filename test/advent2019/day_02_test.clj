(ns advent2019.day-02-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent2019.day-02 :refer [interpret]]))

(deftest intcode
  (testing "interpreter"
    (is (= (interpret [1 9 10 3 2 3 11 0 99 30 40 50])
           [3500 9 10 70 2 3 11 0 99 30 40 50]))
    (is (= (interpret [1 0 0 0 99])
           [2 0 0 0 99]))
    (is (= (interpret [2 3 0 3 99])
           [2 3 0 6 99]))
    (is (= (interpret [2 4 4 5 99 0])
           [2 4 4 5 99 9801]))
    (is (= (interpret [1 1 1 4 99 5 6 0 99])
           [30 1 1 4 2 5 6 0 99]))))