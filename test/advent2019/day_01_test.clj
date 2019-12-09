(ns advent2019.day-01-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day-01 :refer [fuel-needed tsiolkovsky]]))

(deftest fuel
  (testing "fuel needed for module mass"
    (is (= (fuel-needed 12) 2))
    (is (= (fuel-needed 14) 2))
    (is (= (fuel-needed 1969) 654))
    (is (= (fuel-needed 100756) 33583)))
  (testing "tsiolkovsky fuel needed for launch"
    (is (= (tsiolkovsky 14) 2))
    (is (= (tsiolkovsky 1969) 966))
    (is (= (tsiolkovsky 100756) 50346))))
