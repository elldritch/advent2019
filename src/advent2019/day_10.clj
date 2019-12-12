(ns advent2019.day-10
  (:require [clojure.set]))

(defn get-in-grid [grid x y] ((grid y) x))

(defn grid-to-coordinates [grid]
  (apply concat (map-indexed #(map-indexed (fn [x point]
                                             {:x x :y %1 :at point})
                                           %2)
                             grid)))

(defn asteroids [grid]
  (map #(dissoc % :at) (filter #(= \# (:at %)) (grid-to-coordinates grid))))

(defn parse [lines]
  (let [grid (vec (map #(vec (char-array %)) (clojure.string/split-lines lines)))
        asteroid-list (vec (asteroids grid))]
    {:grid grid
     :asteroids asteroid-list}))

(defn slope [a b]
  (if (= (:x a) (:x b))
    ; This is an awful hack to represent vertical lines with infinite slope. We
    ; would cause a divide-by-zero trying to use / here.
    (clojure.lang.Ratio. (biginteger 1) (biginteger 0))
    (clojure.lang.Numbers/toRatio (/ (- (:y b) (:y a)) (- (:x b) (:x a))))))

(defn between [a x b] (or (<= a x b) (<= b x a)))

(defn ray [point dx dy]
  (iterate (fn [{:keys [x y]}] {:x (+ dx x) :y (+ dy y)}) point))

(defn points-between [a b points]
  (rest (drop-last (take-while #(and (between (:x a) (:x %) (:x b))
                                     (between (:y a) (:y %) (:y b)))
                               points))))

(defn lattice-points-between [a b]
  (let [slope' (slope a b)
        dy (int (numerator slope'))
        dx (int (denominator slope'))
        points (ray a dx dy)
        points' (ray a (- dx) (- dy))]
    (concat (points-between a b points) (points-between a b points'))))

(defn visible? [region a b]
  (if (and (= (:x a) (:x b))
           (= (:y a) (:y b)))
    false
    (reduce #(and %1 (not= \# (get-in-grid (:grid region) (:x %2) (:y %2))))
            true
            (lattice-points-between a b))))

(defn visible-asteroids [region point]
  (filter #(visible? region point %) (:asteroids region)))

(defn position-with-most-asteroids-visible [region]
  (->> (:asteroids region)
       (map #(assoc % :visible (count (visible-asteroids region %))))
       (apply max-key :visible)))

(defn angle-clockwise-from-up [origin point]
  (let [dx (- (:x point) (:x origin))
        dy (- (- (:y point) (:y origin)))
        at2 (java.lang.Math/atan2 dx dy)]
    (if (neg? at2) (+ at2 (* 2 java.lang.Math/PI)) at2)))

(defn vaporize-asteroids [region vaporized]
  {:grid (reduce (fn [grid a]
                   (update grid (:y a) #(assoc (if (nil? %) {} %) (:x a) \.)))
                 (:grid region)
                 vaporized)
   :asteroids (clojure.set/difference (set (:asteroids region)) (set vaporized))})

(defn vaporization-order [region]
  (let [origin (dissoc (position-with-most-asteroids-visible region) :visible)]
    (loop [remaining (vaporize-asteroids region [origin])
           acc ()]
      (let [vaporized-in-layer (filter (set (visible-asteroids remaining origin))
                                       (sort-by #(angle-clockwise-from-up origin %) (:asteroids remaining)))
            remaining' (vaporize-asteroids remaining vaporized-in-layer)
            vaporized (concat acc vaporized-in-layer)]
        (if (empty? (:asteroids remaining'))
          vaporized
          (recur remaining' vaporized))))))

(defn solve! [file]
  (let [region (parse (slurp file))
        vaporized (vaporization-order region)
        bet (nth vaporized 199)]
    (println "Position with most asteroids visible:" (position-with-most-asteroids-visible region))
    (println "200th asteroid to be vaporized:" bet)
    (println "Part 2 answer:" (+ (* 100 (:x bet)) (:y bet)))))
