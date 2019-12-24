(ns advent2019.day-18-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent2019.day-18 :refer [input->maze
                                       shortest-path-waypoints
                                       waypoints-length]]))

(def maze-1 "#########
#b.A.@.a#
#########")

(def maze-large-1 "########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################")

(def maze-large-2 "########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################")

(def maze-large-3 "#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################")

(def maze-large-4 "########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################")

(defn waypoints [tc] (shortest-path-waypoints (input->maze tc)))

(deftest shortest-maze-path
  (testing "shortest path length"))
