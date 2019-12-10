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
  (filter #(= \# (:at %)) (grid-to-coordinates grid)))

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

(defn visible-asteroids [grid point]
  (reduce + 0 (map #(if (visible? grid point %) 1 0) (asteroids grid))))

(defn position-with-most-asteroids-visible [grid]
  (->> (grid-to-coordinates grid)
       (filter #(= (:at %) \#))
       (map #(assoc % :visible (visible-asteroids grid %)))
       (apply max-key :visible)))

(defn solve! [file]
  (println "Position with most asteroids visible:" (position-with-most-asteroids-visible (load-file! file))))
