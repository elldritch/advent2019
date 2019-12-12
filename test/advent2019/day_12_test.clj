(ns advent2019.day-12-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent2019.day-12 :refer [simulate
                                       total-energy]]))

(def test-moons
  (mapv #(assoc {:velocity {:x 0 :y 0 :z 0}} :position %)
       [{:x -1 :y 0 :z 2}
        {:x 2 :y -10 :z -7}
        {:x 4 :y -8 :z 8}
        {:x 3 :y 5 :z -1}]))

(def test-moons-2
  (mapv #(assoc {:velocity {:x 0 :y 0 :z 0}} :position %)
        [{:x -8 :y -10 :z 0}
         {:x 5 :y 5 :z 10}
         {:x 2 :y -7 :z 3}
         {:x 9 :y -8 :z -3}]))
