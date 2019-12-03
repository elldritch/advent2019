(ns advent2019.day-03
  (:require [clojure.math.numeric-tower :refer [abs]]))

(defn parse [input]
  (map
   #(map (fn [[direction & distance]]
           {:direction (case direction
                         \L :left
                         \R :right
                         \U :up
                         \D :down)
            :distance (Integer/parseInt (apply str distance))}) %)
   (map #(clojure.string/split % #",")
        (clojure.string/split-lines input))))

(defn move [[x y] {:keys [direction distance]}]
  (case direction
    :up [x (+ y distance)]
    :down [x (- y distance)]
    :right [(+ x distance) y]
    :left [(- x distance) y]))

(defn reduce-directions [{lines :lines
                          [x y] :position}
                         {:keys [direction] :as step}]
  (let [[x' y' :as next-position] (move [x y] step)]
    (case direction
      :up {:position next-position
           :lines (cons {:vertical true :x x :bottom y :top y'} lines)}
      :down {:position next-position
             :lines (cons {:vertical true :x x
                           :bottom y' :top y}
                          lines)}
      :right {:position next-position
              :lines (cons {:horizontal true :y y
                            :left x :right x'}
                           lines)}
      :left {:position next-position
             :lines (cons {:horizontal true :y y
                           :left x' :right x}
                          lines)})))

(defn directions-to-lines [directions]
  ((reduce reduce-directions {:lines () :position [0 0]} directions) :lines))

(defn reduce-intersections [{lines :lines
                             [x y] :position
                             intersections :intersections}
                            {:keys [direction] :as step}]
  (let [[x' y' :as next-position] (move [x y] step)]
    (case direction
      :up {:lines lines
           :position next-position
           :intersections (concat
                           intersections
                           (->> lines
                                (filter :horizontal)
                                (filter (fn [line] (and (< (line :left) x) (> (line :right) x))))
                                (filter (fn [line] (and (> (line :y) y) (< (line :y) y'))))
                                (map (fn [line] {:x x :y (line :y)}))))}
      :down {:lines lines
             :position next-position
             :intersections (concat
                             intersections
                             (->> lines
                                  (filter :horizontal)
                                  (filter (fn [line] (and (< (line :left) x) (> (line :right) x))))
                                  (filter (fn [line] (and (> (line :y) y') (< (line :y) y))))
                                  (map (fn [line] {:x x :y (line :y)}))))}
      :right {:lines lines
              :position next-position
              :intersections (concat
                              intersections
                              (->> lines
                                   (filter :vertical)
                                   (filter (fn [line] (and (< (line :bottom) y) (> (line :top) y))))
                                   (filter (fn [line] (and (> (line :x) x) (< (line :x) x'))))
                                   (map (fn [line] {:x (line :x) :y y}))))}
      :left {:lines lines
             :position next-position
             :intersections (concat
                             intersections
                             (->> lines
                                  (filter :vertical)
                                  (filter (fn [line] (and (< (line :bottom) y) (> (line :top) y))))
                                  (filter (fn [line] (and (> (line :x) x') (< (line :x) x))))
                                  (map (fn [line] {:x (line :x) :y y}))))})))

(defn directions-to-line-intersections [lines directions]
  ((reduce reduce-intersections {:lines lines :position [0 0] :intersections ()} directions) :intersections))

(defn manhattan [{:keys [x y]}] (+ (abs x) (abs y)))

(defn closest-intersection [wire-1 wire-2]
  (let [wire-1-lines (directions-to-lines wire-1)
        intersections (directions-to-line-intersections wire-1-lines wire-2)]
    (apply min-key manhattan intersections)))

(defn load-file! [file] (parse (slurp file)))

(defn solve! [file]
  (let [[wire-1 wire-2] (load-file! file)
        closest (closest-intersection wire-1 wire-2)]
    (println "Closest intersection:" closest "Distance:" (manhattan closest))))
