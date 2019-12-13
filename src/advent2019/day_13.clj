(ns advent2019.day-13
  (:require [advent2019.lib.intcode :as intcode]))

(defn tile-id-to-type [tile-id]
  (case tile-id
    0 :empty
    1 :wall
    2 :block
    3 :horizontal-paddle
    4 :ball))

(defn outputs-to-tile [x y tile-id]
  {:x x :y y :tile-id (tile-id-to-type tile-id)})

(defn tiles-on-exit [program]
  (let [tiles (:outputs (intcode/gather-outputs (intcode/run-program program)))]
    (map #(apply outputs-to-tile %) (partition 3 tiles))))

(defn display-tiles [tiles]
  (map (fn [row]
         (apply str (map #(case (:tile-id %)
                :empty " "
                :wall "W"
                :block "B"
                :horizontal-paddle "P"
                :ball "O") row)))
       (partition-by :y (sort-by :y (sort-by :x tiles)))))

(defn solve! [file]
  (let [program (intcode/load-program! file)
        tiles (tiles-on-exit program)]
    (println "Tiles on screen:" (count (filter #(= (:tile-id %) :block)
                                               tiles)))))
