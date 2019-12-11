(ns advent2019.day-11
  (:require [advent2019.lib.intcode :as intcode]
            [clojure.spec.alpha :as s]))

(defn color [grid point] (get grid point :black))

(defn paint [grid point color] (assoc grid point color))

(defn inputs-until-output [continuation input]
  (loop [cont continuation]
    ()))

(defn run-robot [program]
  (loop [grid {}
         position {:x 0 :y 0}
         continuation (intcode/run-program program)]
    ()))

(defn solve! [file] ())
