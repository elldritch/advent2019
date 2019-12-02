(ns advent2019.01-tyranny-of-the-rocket-equation-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.01-tyranny-of-the-rocket-equation :refer [fuel-needed]]))

(deftest fuel
  (testing "Fuel needed for launch"
    (is (= (fuel-needed 12) 2))
    (is (= (fuel-needed 14) 2))
    (is (= (fuel-needed 1969) 654))
    (is (= (fuel-needed 100756) 33583))))
