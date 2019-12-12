(ns advent2019.day-12-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent2019.day-12 :refer [simulate
                                       total-energy
                                       moon-period]]))

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

(deftest n-body-moons
  (testing "first example"
    (testing "simulation"
      (is (= (simulate test-moons 0)
             [{:position {:x -1 :y 0 :z 2} :velocity {:x 0 :y 0 :z 0}}
              {:position {:x 2 :y -10 :z -7} :velocity {:x 0 :y 0 :z 0}}
              {:position {:x 4 :y -8 :z 8} :velocity {:x 0 :y 0 :z 0}}
              {:position {:x 3 :y 5 :z -1} :velocity {:x 0 :y 0 :z 0}}]))
      (is (= (simulate test-moons 1)
             [{:position {:x 2 :y -1 :z 1} :velocity {:x 3 :y -1 :z -1}}
              {:position {:x 3 :y -7 :z -4} :velocity {:x 1 :y 3 :z 3}}
              {:position {:x 1 :y -7 :z 5} :velocity {:x -3 :y 1 :z -3}}
              {:position {:x 2 :y 2 :z 0} :velocity {:x -1 :y -3 :z 1}}]))
      (is (= (simulate test-moons 2)
             [{:position {:x 5 :y -3 :z -1} :velocity {:x 3 :y -2 :z -2}}
              {:position {:x 1 :y -2 :z 2} :velocity {:x -2 :y 5 :z 6}}
              {:position {:x 1 :y -4 :z -1} :velocity {:x 0 :y 3 :z -6}}
              {:position {:x 1 :y -4 :z 2} :velocity {:x -1 :y -6 :z 2}}]))
      (is (= (simulate test-moons 3)
             [{:position {:x 5 :y -6 :z -1} :velocity {:x 0 :y -3 :z 0}}
              {:position {:x 0 :y 0 :z 6} :velocity {:x -1 :y 2 :z 4}}
              {:position {:x 2 :y 1 :z -5} :velocity {:x 1 :y 5 :z -4}}
              {:position {:x 1 :y -8 :z 2} :velocity {:x 0 :y -4 :z 0}}]))
      (is (= (simulate test-moons 4)
             [{:position {:x 2 :y -8 :z 0} :velocity {:x -3 :y -2 :z 1}}
              {:position {:x 2 :y 1 :z 7} :velocity {:x 2 :y 1 :z 1}}
              {:position {:x 2 :y 3 :z -6} :velocity {:x 0 :y 2 :z -1}}
              {:position {:x 2 :y -9 :z 1} :velocity {:x 1 :y -1 :z -1}}]))
      (is (= (simulate test-moons 5)
             [{:position {:x -1 :y -9 :z 2} :velocity {:x -3 :y -1 :z 2}}
              {:position {:x 4 :y 1 :z 5} :velocity {:x 2 :y 0 :z -2}}
              {:position {:x 2 :y 2 :z -4} :velocity {:x 0 :y -1 :z 2}}
              {:position {:x 3 :y -7 :z -1} :velocity {:x 1 :y 2 :z -2}}]))
      (is (= (simulate test-moons 6)
             [{:position {:x -1 :y -7 :z 3} :velocity {:x 0 :y 2 :z 1}}
              {:position {:x 3 :y 0 :z 0} :velocity {:x -1 :y -1 :z -5}}
              {:position {:x 3 :y -2 :z 1} :velocity {:x 1 :y -4 :z 5}}
              {:position {:x 3 :y -4 :z -2} :velocity {:x 0 :y 3 :z -1}}]))
      (is (= (simulate test-moons 7)
             [{:position {:x 2 :y -2 :z 1} :velocity {:x 3 :y 5 :z -2}}
              {:position {:x 1 :y -4 :z -4} :velocity {:x -2 :y -4 :z -4}}
              {:position {:x 3 :y -7 :z 5} :velocity {:x 0 :y -5 :z 4}}
              {:position {:x 2 :y 0 :z 0} :velocity {:x -1 :y 4 :z 2}}]))
      (is (= (simulate test-moons 8)
             [{:position {:x 5 :y 2 :z -2} :velocity {:x 3 :y 4 :z -3}}
              {:position {:x 2 :y -7 :z -5} :velocity {:x 1 :y -3 :z -1}}
              {:position {:x 0 :y -9 :z 6} :velocity {:x -3 :y -2 :z 1}}
              {:position {:x 1 :y 1 :z 3} :velocity {:x -1 :y 1 :z 3}}]))
      (is (= (simulate test-moons 9)
             [{:position {:x 5 :y 3 :z -4} :velocity {:x 0 :y 1 :z -2}}
              {:position {:x 2 :y -9 :z -3} :velocity {:x 0 :y -2 :z 2}}
              {:position {:x 0 :y -8 :z 4} :velocity {:x 0 :y 1 :z -2}}
              {:position {:x 1 :y 1 :z 5} :velocity {:x 0 :y 0 :z 2}}]))
      (is (= (simulate test-moons 10)
             [{:position {:x 2 :y 1 :z -3} :velocity {:x -3 :y -2 :z 1}}
              {:position {:x 1 :y -8 :z 0} :velocity {:x -1 :y 1 :z 3}}
              {:position {:x 3 :y -6 :z 1} :velocity {:x 3 :y 2 :z -3}}
              {:position {:x 2 :y 0 :z 4} :velocity {:x 1 :y -1 :z -1}}]))
      (is (= (simulate test-moons 2770)
             [{:position {:x 2 :y -1 :z 1} :velocity {:x -3 :y 2 :z 2}}
              {:position {:x 3 :y -7 :z -4} :velocity {:x 2 :y -5 :z -6}}
              {:position {:x 1 :y -7 :z 5} :velocity {:x 0 :y -3 :z 6}}
              {:position {:x 2 :y 2 :z 0} :velocity {:x 1 :y 6 :z -2}}]))
      (is (= (simulate test-moons 2771)
             [{:position {:x -1 :y 0 :z 2} :velocity {:x -3 :y 1 :z 1}}
              {:position {:x 2 :y -10 :z -7} :velocity {:x -1 :y -3 :z -3}}
              {:position {:x 4 :y -8 :z 8} :velocity {:x 3 :y -1 :z 3}}
              {:position {:x 3 :y 5 :z -1} :velocity {:x 1 :y 3 :z -1}}]))
      (is (= (simulate test-moons 2772)
             [{:position {:x -1 :y 0 :z 2} :velocity {:x 0 :y 0 :z 0}}
              {:position {:x 2 :y -10 :z -7} :velocity {:x 0 :y 0 :z 0}}
              {:position {:x 4 :y -8 :z 8} :velocity {:x 0 :y 0 :z 0}}
              {:position {:x 3 :y 5 :z -1} :velocity {:x 0 :y 0 :z 0}}])))
    (testing "energy"
      (is (= (total-energy (simulate test-moons 10))
             179)))
    (testing "period"
      (is (= (moon-period test-moons)
             2772))))
  (testing "second example"
    (testing "simulation"
      (is (= (simulate test-moons-2 0)
             [{:position {:x -8 :y -10 :z 0} :velocity {:x 0 :y 0 :z 0}}
              {:position {:x 5 :y 5 :z 10} :velocity {:x 0 :y 0 :z 0}}
              {:position {:x 2 :y -7 :z 3} :velocity {:x 0 :y 0 :z 0}}
              {:position {:x 9 :y -8 :z -3} :velocity {:x 0 :y 0 :z 0}}]))
      (is (= (simulate test-moons-2 10)
             [{:position {:x -9 :y -10 :z 1} :velocity {:x -2 :y -2 :z -1}}
              {:position {:x 4 :y 10 :z 9} :velocity {:x -3 :y 7 :z -2}}
              {:position {:x 8 :y -10 :z -3} :velocity {:x 5 :y -1 :z -2}}
              {:position {:x 5 :y -10 :z 3} :velocity {:x 0 :y -4 :z 5}}]))
      (is (= (simulate test-moons-2 20)
             [{:position {:x -10 :y 3 :z -4} :velocity {:x -5 :y 2 :z 0}}
              {:position {:x 5 :y -25 :z 6} :velocity {:x 1 :y 1 :z -4}}
              {:position {:x 13 :y 1 :z 1} :velocity {:x 5 :y -2 :z 2}}
              {:position {:x 0 :y 1 :z 7} :velocity {:x -1 :y -1 :z 2}}]))
      (is (= (simulate test-moons-2 30)
             [{:position {:x 15 :y -6 :z -9} :velocity {:x -5 :y 4 :z 0}}
              {:position {:x -4 :y -11 :z 3} :velocity {:x -3 :y -10 :z 0}}
              {:position {:x 0 :y -1 :z 11} :velocity {:x 7 :y 4 :z 3}}
              {:position {:x -3 :y -2 :z 5} :velocity {:x 1 :y 2 :z -3}}]))
      (is (= (simulate test-moons-2 40)
             [{:position {:x 14 :y -12 :z -4} :velocity {:x 11 :y 3 :z 0}}
              {:position {:x -1 :y 18 :z 8} :velocity {:x -5 :y 2 :z 3}}
              {:position {:x -5 :y -14 :z 8} :velocity {:x 1 :y -2 :z 0}}
              {:position {:x 0 :y -12 :z -2} :velocity {:x -7 :y -3 :z -3}}]))
      (is (= (simulate test-moons-2 50)
             [{:position {:x -23 :y 4 :z 1} :velocity {:x -7 :y -1 :z 2}}
              {:position {:x 20 :y -31 :z 13} :velocity {:x 5 :y 3 :z 4}}
              {:position {:x -4 :y 6 :z 1} :velocity {:x -1 :y 1 :z -3}}
              {:position {:x 15 :y 1 :z -5} :velocity {:x 3 :y -3 :z -3}}]))
      (is (= (simulate test-moons-2 60)
             [{:position {:x 36 :y -10 :z 6} :velocity {:x 5 :y 0 :z 3}}
              {:position {:x -18 :y 10 :z 9} :velocity {:x -3 :y -7 :z 5}}
              {:position {:x 8 :y -12 :z -3} :velocity {:x -2 :y 1 :z -7}}
              {:position {:x -18 :y -8 :z -2} :velocity {:x 0 :y 6 :z -1}}]))
      (is (= (simulate test-moons-2 70)
             [{:position {:x -33 :y -6 :z 5} :velocity {:x -5 :y -4 :z 7}}
              {:position {:x 13 :y -9 :z 2} :velocity {:x -2 :y 11 :z 3}}
              {:position {:x 11 :y -8 :z 2} :velocity {:x 8 :y -6 :z -7}}
              {:position {:x 17 :y 3 :z 1} :velocity {:x -1 :y -1 :z -3}}]))
      (is (= (simulate test-moons-2 80)
             [{:position {:x 30 :y -8 :z 3} :velocity {:x 3 :y 3 :z 0}}
              {:position {:x -2 :y -4 :z 0} :velocity {:x 4 :y -13 :z 2}}
              {:position {:x -18 :y -7 :z 15} :velocity {:x -8 :y 2 :z -2}}
              {:position {:x -2 :y -1 :z -8} :velocity {:x 1 :y 8 :z 0}}]))
      (is (= (simulate test-moons-2 90)
             [{:position {:x -25 :y -1 :z 4} :velocity {:x 1 :y -3 :z 4}}
              {:position {:x 2 :y -9 :z 0} :velocity {:x -3 :y 13 :z -1}}
              {:position {:x 32 :y -8 :z 14} :velocity {:x 5 :y -4 :z 6}}
              {:position {:x -1 :y -2 :z -8} :velocity {:x -3 :y -6 :z -9}}]))
      (is (= (simulate test-moons-2 100)
             [{:position {:x 8 :y -12 :z -9} :velocity {:x -7 :y 3 :z 0}}
              {:position {:x 13 :y 16 :z -3} :velocity {:x 3 :y -11 :z -5}}
              {:position {:x -29 :y -11 :z -1} :velocity {:x -3 :y 7 :z 4}}
              {:position {:x 16 :y -13 :z 23} :velocity {:x 7 :y 1 :z 1}}])))
    (testing "energy"
      (is (= (total-energy (simulate test-moons-2 100))
             1940)))
    (testing "period"
      (is (= (moon-period test-moons-2)
             4686774924)))))
