(ns advent2019.day-17
  (:require [advent2019.lib.intcode :as intcode]
            [advent2019.lib.grid :as g]))

(defn show [image]
  (reduce (fn [s px] (str s (char px))) "" image))

(defn image->grid [ascii]
  (:grid (reduce (fn [{:keys [grid x y]} c]
                   (if (= c \newline)
                     {:grid grid
                      :x 0
                      :y (inc y)}
                     {:grid (g/grid-assoc grid x y c)
                      :x (inc x)
                      :y y}))
                 {:grid (g/grid)
                  :x 0
                  :y 0}
                 ascii)))

(defn intersection? [grid x y]
  (and (= (g/grid-get grid x y) \#)
       (= (g/grid-get grid (inc x) y) \#)
       (= (g/grid-get grid (dec x) y) \#)
       (= (g/grid-get grid x (inc y)) \#)
       (= (g/grid-get grid x (dec y)) \#)))

(defn intersections [grid]
  (filter #(intersection? grid (:x %) (:y %))
          (g/values grid)))

(defn solve! [file]
  (let [image (:outputs (->> file
                             (intcode/load-program!)
                             (intcode/run-program)
                             (intcode/gather-outputs)))
        grid (image->grid (show image))]
    (println "Sum of alignment parameters:"
             (reduce + (map #(* (:x %) (:y %))
                            (intersections grid))))))
