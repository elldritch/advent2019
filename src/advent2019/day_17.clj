(ns advent2019.day-17
  (:require [advent2019.lib.intcode :as intcode]
            [advent2019.lib.grid :as g]
            [clojure.string :as str]))

(defn outputs->image [image]
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
            travelled' (conj travelled {:distance (:distance segment)
                                        :turn relative})]
        (if (nil? relative)
          travelled'
          (recur (:end segment)
                 (g/turn direction relative)
                 travelled'))))))

(defn path->ascii [path]
  (->> path
       (mapcat #(vector (:distance %) (:turn %)))
       (filter #(not (or (#{0} %) (nil? %))))
       (map #(case %
               :right "R"
               :left "L"
               (str %)))
       (str/join ",")))

(defn input-program [continuation program]
  (intcode/provide-inputs
   continuation
   (->> program
        (map #(case %
                :a \A
                :b \B
                :c \C
                :left \L
                :right \R
                (str %)))
        (str/join ",")
        (#(str % \newline))
        (map int)
        (apply list))))

; These were determined from manual examination.
; A: L,12,L,12,R,12,
; A: L,12,L,12,R,12,
; B: L,8,L,8,R,12,L,8,L,8,
; C: L,10,R,8,R,12,
; C: L,10,R,8,R,12,
; A: L,12,L,12,R,12,
; B: L,8,L,8,R,12,L,8,L,8,
; C: L,10,R,8,R,12,
; A: L,12,L,12,R,12,
; B: L,8,L,8,R,12,L,8,L,8
(defn input-main [continuation]
  (input-program continuation [:a :a :b :c :c :a :b :c :a]))

(defn input-a [continuation]
  (input-program continuation [:left 12 :left 12 :right 12]))

(defn input-b [continuation]
  (input-program continuation [:left 8 :left 8 :right 12 :left 8 :left 8]))

(defn input-c [continuation]
  (input-program continuation [:left 10 :right 8 :right 12]))

(defn solve! [file]
  (let [image (:outputs (->> file
                             (intcode/load-program!)
                             (intcode/run-program)
                             (intcode/gather-outputs)))
        grid (image->grid (outputs->image image))]
    (println "Sum of alignment parameters:"
             (reduce + (map #(* (:x %) (:y %))
                            (intersections grid))))
    (println "Path through scaffold:" (path->ascii (path grid)))))
