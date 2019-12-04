(ns advent2019.day-04-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent2019.day-04 :refer [meets-criteria? meets-extra-criteria?]]))

(deftest password-guessing
  (testing "passwords that meet all criteria"
    (is (meets-criteria? 111111))
    (is (not (meets-criteria? 223450)))
    (is (not (meets-criteria? 123789))))
  (testing "passwords meet stricter criteria"
    (is (meets-extra-criteria? 112233))
    (is (not (meets-extra-criteria? 123444)))
    (is (meets-extra-criteria? 111122))))
