(ns advent2019.day-06-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent2019.day-06 :refer [count-orbits
                                       orbits-to-graph
                                       parse-orbits
                                       minimum-orbital-transfers]]))

(def orbital-map (orbits-to-graph (parse-orbits "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")))

(deftest counting-orbits
  (testing "counting direct and indirect orbits"
    (is (= 42 (count-orbits orbital-map))))
  (testing "minimum orbital transfers"
    (is (= 4 (minimum-orbital-transfers orbital-map "K" "I")))))
