(ns advent2019.day-10-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent2019.day-10 :refer [parse
                                       grid-to-coordinates
                                       visible-asteroids
                                       position-with-most-asteroids-visible
                                       vaporization-order]]))

(def map-1 ".#..#
.....
#####
....#
...##")

(def visible-1 ".7..7
.....
67775
....7
...87")

(def map-2 "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####")

(def map-3 "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.")

(def map-4 ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..")

(def map-5 ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")

(def map-vaporization ".#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....#...###..
..#.#.....#....##")

(defn expect [input x y visible]
  (let [position (position-with-most-asteroids-visible (parse input))]
    (and (= x (:x position))
         (= y (:y position))
         (= visible (:visible position)))))

(defn expect-order [input n x y] (= (nth input (dec n)) {:x x :y y}))

(deftest asteroid-monitoring
  (testing "asteroid full map visibility"
    (is (->> (grid-to-coordinates (:grid (parse visible-1)))
             (map #(assoc % :visible (count (visible-asteroids (parse map-1) %))))
             (map #(or (= \. (:at %)) (= (Character/digit (:at %) 10) (:visible %))))
             (reduce #(and %1 %2)))))
  (testing "asteroid monitoring station visibility"
    (is (expect map-2 5 8 33))
    (is (expect map-3 1 2 35))
    (is (expect map-4 6 3 41))
    (is (expect map-5 11 13 210)))
  (testing "asteroid vaporization order"
    (is (= (vaporization-order (parse map-vaporization))
           '({:x 8, :y 1}
             {:x 9, :y 0}
             {:x 9, :y 1}
             {:x 10, :y 0}
             {:x 9, :y 2}
             {:x 11, :y 1}
             {:x 12, :y 1}
             {:x 11, :y 2}
             {:x 15, :y 1}
             {:x 12, :y 2}
             {:x 13, :y 2}
             {:x 14, :y 2}
             {:x 15, :y 2}
             {:x 12, :y 3}
             {:x 16, :y 4}
             {:x 15, :y 4}
             {:x 10, :y 4}
             {:x 4, :y 4}
             {:x 2, :y 4}
             {:x 2, :y 3}
             {:x 0, :y 2}
             {:x 1, :y 2}
             {:x 0, :y 1}
             {:x 1, :y 1}
             {:x 5, :y 2}
             {:x 1, :y 0}
             {:x 5, :y 1}
             {:x 6, :y 1}
             {:x 6, :y 0}
             {:x 7, :y 0}
             {:x 8, :y 0}
             {:x 10, :y 1}
             {:x 14, :y 0}
             {:x 16, :y 1}
             {:x 13, :y 3}
             {:x 14, :y 3})))
    (let [vaporized (vaporization-order (parse map-5))]
      (is (expect-order vaporized 1 11 12))
      (is (expect-order vaporized 2 12 1))
      (is (expect-order vaporized 3 12 2))
      (is (expect-order vaporized 10 12 8))
      (is (expect-order vaporized 20 16 0))
      (is (expect-order vaporized 50 16 9))
      (is (expect-order vaporized 100 10 16))
      (is (expect-order vaporized 199 9 6))
      (is (expect-order vaporized 200 8 2))
      (is (expect-order vaporized 201 10 9))
      (is (expect-order vaporized 299 11 1)))))
