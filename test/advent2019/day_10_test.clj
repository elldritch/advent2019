(ns advent2019.day-10-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent2019.day-10 :refer [parse
                                       grid-to-coordinates
                                       visible-asteroids
                                       position-with-most-asteroids-visible]]))

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

(defn expect [region x y visible]
  (let [position (position-with-most-asteroids-visible (parse region))]
    (and (= x (:x position))
         (= y (:y position))
         (= visible (:visible position)))))

(deftest asteroid-monitoring
  (testing "asteroid visibility"
    (is (->> (grid-to-coordinates (parse visible-1))
             (map #(assoc % :visible (visible-asteroids (parse map-1) %)))
             (map #(or (= \. (:at %)) (= (Character/digit (:at %) 10) (:visible %))))
             (reduce #(and %1 %2))))
    (is (expect map-2 5 8 33))
    (is (expect map-3 1 2 35))
    (is (expect map-4 6 3 41))
    (is (expect map-5 11 13 210))))
