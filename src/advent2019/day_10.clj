(ns advent2019.day-10)

(defn parse [lines]
  (clojure.string/split-lines lines))

(defn load-file! [file]
  (parse (slurp file)))

(defn get-in-grid [grid x y] (get (grid y) x))

(defn grid-to-coordinates [grid]
  (apply concat (map-indexed #(map-indexed (fn [x point]
                                             {:x x :y %1 :at point})
                                           %2)
                             grid)))

(defn asteroids [grid]
  (map #(dissoc % :at) (filter #(= \# (:at %)) (grid-to-coordinates grid))))

(defn slope [a b]
  ; This is an awful hack to represent vertical lines with infinite slope. We
  ; would cause a divide-by-zero trying to use / here.
  (if (= (:x a) (:x b))
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

(defn visible? [grid a b]
  (if (and (= (:x a) (:x b))
           (= (:y a) (:y b)))
    false
    (reduce #(and %1 (not= \# (get-in-grid grid (:x %2) (:y %2))))
            true
            (lattice-points-between a b))))

; here's the problem: this needs a grid when i'm providing it an asteroid list.
(defn visible-asteroids [grid point]
  (filter #(visible? grid point %) (asteroids grid)))

(defn position-with-most-asteroids-visible [grid]
  (->> (grid-to-coordinates grid)
       (filter #(= (:at %) \#))
       (map #(dissoc % :at))
       (map #(assoc % :visible (count (visible-asteroids grid %))))
       (apply max-key :visible)))


; Find all asteroids visible from the monitoring station.

; Sort visible asteroids in clockwise order. (Use atan2?)

; Add asteroids in this order to the vaporisation list.

; Remove vaporised asteroids (i.e. first layer) from the grid.

; Repeat.

(defn angle-clockwise-from-up [origin point]
  (let [dx (- (:x point) (:x origin))
        dy (- (- (:y point) (:y origin)))
        at2 (java.lang.Math/atan2 dx dy)]
    (if (neg? at2) (+ at2 (* 2 java.lang.Math/PI)) at2)))

(defn vaporization-order [grid]
  (let [origin (position-with-most-asteroids-visible grid)]
    (loop [remaining (set (asteroids grid))
           acc ()]
      (let [vaporized-in-layer (filter (set (visible-asteroids remaining origin))
                                       (sort-by #(angle-clockwise-from-up origin %) remaining))
            remaining' (clojure.set/difference remaining (set vaporized-in-layer))
            vaporized (concat acc vaporized-in-layer)]
        (if (empty? remaining')
          vaporized
          (recur remaining' vaporized))))))

(defn solve! [file]
  (println "Position with most asteroids visible:" (position-with-most-asteroids-visible (load-file! file))))
