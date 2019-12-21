(ns advent2019.day-17
  (:require [advent2019.lib.intcode :as intcode]
            [advent2019.lib.grid :as g]
            [clojure.string :as str]))

(defn show [image]
  (str/trim (reduce (fn [s px] (str s (char px))) "" image)))

(defn image->grid [ascii]
  (:grid (reduce (fn [{:keys [grid x y]} c]
                   (if (= c \newline)
                     {:grid grid
                      :x 0
                      :y (dec y)}
                     {:grid (g/grid-assoc grid x y c)
                      :x (inc x)
                      :y y}))
                 {:grid (g/grid)
                  :x 0
                  :y (count (filter #{\newline} ascii))}
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

(defn next-turn [grid position direction]
  (first (filter (comp #{\#}
                       #(g/grid-get grid (g/adjacent position direction %)))
                 [:left :right])))

(defn travel-down-segment [grid start direction]
  (loop [position start]
    (let [position' (g/adjacent position direction)]
      (if (= \# (g/grid-get grid
                            (:x position')
                            (:y position')))
        (recur position')
        {:start start
         :end position
         :direction direction
         :distance (g/distance position start)}))))

(defn find-robot [grid]
  (let [position (first (filter (comp #{\^ \< \> \v} :value) (g/values grid)))]
    {:position (dissoc position :value)
     :direction (case (:value position)
                  \^ :north
                  \< :west
                  \> :east
                  \v :south)}))

(defn path [grid]
  (let [start (find-robot grid)]
    (loop [position (:position start)
           direction (:direction start)
           travelled []]
      (let [segment (travel-down-segment grid position direction)
            relative (next-turn grid (:end segment) direction)
            travelled' (conj travelled {:travel (:distance segment)
                                        :turn relative})]
        (if (nil? relative)
          travelled'
          (recur (:end segment)
                 (g/turn direction relative)
                 travelled'))))))

(defn path->ascii [path]
  (->> path
       (mapcat #(vector (:travel %) (:turn %)))
       (filter #(not (or (#{0} %) (nil? %))))
       (map #(case %
               :right "R"
               :left "L"
               (str %)))
       (str/join ",")))

(defn solve! [file]
  (let [image (:outputs (->> file
                             (intcode/load-program!)
                             (intcode/run-program)
                             (intcode/gather-outputs)))
        grid (image->grid (show image))]
    (println "Sum of alignment parameters:"
             (reduce + (map #(* (:x %) (:y %))
                            (intersections grid))))
    (println "Scaffold image:")
    (println (show image))
    (println (path grid))))
