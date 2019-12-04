(ns advent2019.day-03-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent2019.day-03 :refer [parse min-intersection manhattan]]))

(def panel-1 "R8,U5,L5,D3\nU7,R6,D4,L4")

(def panel-2 "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83")

(def panel-3 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

(deftest intersections
  (testing "closest intersection"
    (let [[wire-1 wire-2] (parse panel-1)]
      (is (= (manhattan (min-intersection manhattan wire-1 wire-2)) 6)))
    (let [[wire-1 wire-2] (parse panel-2)]
      (is (= (manhattan (min-intersection manhattan wire-1 wire-2)) 159)))
    (let [[wire-1 wire-2] (parse panel-3)]
      (is (= (manhattan (min-intersection manhattan wire-1 wire-2)) 135))))
  (testing "intersection with fewest steps"
    (let [[wire-1 wire-2] (parse panel-1)]
      (is (= (:steps (min-intersection :steps wire-1 wire-2)) 30)))
    (let [[wire-1 wire-2] (parse panel-2)]
      (is (= (:steps (min-intersection :steps wire-1 wire-2)) 610)))
    (let [[wire-1 wire-2] (parse panel-3)]
      (is (= (:steps (min-intersection :steps wire-1 wire-2)) 410)))))
