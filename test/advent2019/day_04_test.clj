(ns advent2019.day-04-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent2019.day-04 :refer [meets-criteria?]]))

(deftest password-guessing
  (testing "passwords that meet all criteria"
    (is (meets-criteria? 111111))
    (is (not (meets-criteria? 223450)))
    (is (not (meets-criteria? 123789)))))