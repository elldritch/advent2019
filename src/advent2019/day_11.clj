(ns advent2019.day-11
  (:require [advent2019.lib.intcode :as intcode]))

(defn color-at-point [grid point] (get grid point :black))

(defn paint [grid point color] (assoc grid point color))

(defn color-to-input [color]
  (case color
    :black 0
    :white 1))

(defn output-to-color [n]
  (case n
    1 :white
    0 :black))

(defn turn [facing turning]
  (case facing
    :north (case turning
             :left :west
             :right :east)
    :south (case turning
             :left :east
             :right :west)
    :east (case turning
            :left :north
            :right :south)
    :west (case turning
            :left :south
            :right :north)))

(defn move [position facing turning]
  (let [facing' (turn facing turning)
        [axis f] (case facing'
                   :north [:y inc]
                   :south [:y dec]
                   :east  [:x inc]
                   :west  [:x dec])]
    {:position (update position axis f)
     :facing facing'}))

(defn output-to-direction [n]
  (case n
    0 :left
    1 :right
    (throw (ex-info "unknown direction" {:n n}))))

(defn inputs-until-output [continuation input]
  (loop [cont continuation]
    (if (= :needs-input (:status cont))
      (recur (intcode/resume-program-with-input cont input))
      cont)))

(defn sensor-until-output [continuation grid position]
  (inputs-until-output continuation
                       (color-to-input (color-at-point grid position))))

(defn run-robot [program starting-grid]
  (loop [grid starting-grid
         position {:x 0 :y 0}
         facing :north
         continuation (sensor-until-output (intcode/run-program program)
                                           grid
                                           position)]
    (if (= :halted (:status continuation))
      grid
      (let [color-to-paint (:output continuation)
            grid' (paint grid position (output-to-color color-to-paint))
            consumed-continuation (sensor-until-output (intcode/resume-program continuation)
                                                       grid'
                                                       position)
            direction-to-turn (:output consumed-continuation)
            movement (move position facing (output-to-direction direction-to-turn))
            position' (:position movement)]
        (recur grid'
               position'
               (:facing movement)
               (sensor-until-output (intcode/resume-program consumed-continuation)
                                    grid'
                                    position'))))))

(defn extreme-map-key [cmp f m]
  (reduce-kv (fn [acc k _] (cmp (f k) acc)) 0 m))

(defn show-grid [grid]
  (let [min-x (extreme-map-key min :x grid)
        min-y (extreme-map-key min :y grid)
        max-x (extreme-map-key max :x grid)
        max-y (extreme-map-key max :y grid)]
    (clojure.string/join
     \newline
     (map (fn [y]
            (clojure.string/join
             ""
             (map (fn [x] (case (get grid {:x x :y y} :black)
                            :black " "
                            :white "X"))
                  (range min-x (inc max-x)))))
          (reverse (range min-y (inc max-y)))))))

(defn solve! [file]
  (let [program (intcode/load-program! file)]
    (println "Total squares painted:" (count (run-robot program {})))
    (println "Registration identifier:\n")
    (println (show-grid (run-robot program {{:x 0 :y 0} :white})))))
