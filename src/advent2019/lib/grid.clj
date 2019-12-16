(ns advent2019.lib.grid
  "Provides utilities for working with square grid graphs."
  (:require [clojure.spec.alpha :as s]
            [clojure.math.numeric-tower :as math]))

(s/def ::x int?)

(s/def ::y int?)

(s/def ::value any?)

(s/def ::point (s/keys :req-un [::x ::y]
                       :opt-un [::value]))

(s/def ::row (s/map-of ::x ::value))

(s/def ::grid (s/map-of ::y ::row))

(defn grid
  "Constructs a grid."
  ; TODO: for improved performance when calling [[values]], this should be a
  ; sorted-map. Unfortunately, assoc-in and update-in create hash-maps.
  [] {})

(defn grid-get
  "Returns the value at the point.

  If the point does not have a value, `not-found` is returned. If `not-found`
  is not set, nil is returned."
  ([grid x y] (grid-get grid x y nil))
  ([grid x y not-found] (get-in grid [y x] not-found)))

(s/fdef grid-get
  :args (s/alt :get (s/cat :grid ::grid
                           :x ::x
                           :y ::y)
               :get-with-not-found (s/cat :grid ::grid
                                          :x ::x
                                          :y ::y
                                          :not-found ::value))
  :ret ::value)

(defn grid-update
  "Updates a value at a point in the grid."
  [grid x y f] (update-in grid [y x] f))

(s/fdef grid-update
  :args (s/cat :grid ::grid
               :x ::x
               :y ::y
               :f (s/fspec :args (s/cat :value ::value)
                           :ret ::value))
  :ret ::grid)

(defn grid-assoc
  "Sets a value at a point in the grid."
  [grid x y value] (assoc-in grid [y x] value))

(s/fdef grid-assoc
  :args (s/cat :grid ::grid
               :x ::x
               :y ::y
               :value ::value)
  :ret ::grid)

(defn grid-dissoc
  "Unsets a value at a point in the grid."
  [grid x y]
  (let [point-removed (update-in grid [y] dissoc x)]
    ; Remove empty rows.
    (if (empty? (point-removed y))
      (dissoc point-removed y)
      point-removed)))

(defn values
  "Returns a collection of all grid points that have non-nil values in
  sparse row-major order."
  [grid]
  (sort-by :y (sort-by :x (mapcat
                           (fn [[y row]] (map
                                          (fn [[x value]] {:x x :y y :value value})
                                          row))
                           grid))))

; (defn rectangle
;   "Returns a collection of all grid points within a bounding rectangle in
;   row-major order.

;   If `top-left` and `bottom-right` points are provided, they define the
;   bounding rectangle. Otherwise, the bounding rectangle is chosen to contain
;   all grid points with non-nil values."
;   ([grid]
;    ())
;   ([grid top-left bottom-right]
;    ()))

(defn- extreme-x [grid f]
  (apply f (map (fn [[_ row]] (apply f (keys row))) grid)))

(defn min-x
  "The lowest x coordinate of a point in the grid with a non-nil value."
  [grid] (extreme-x grid min))

(defn max-x
  "The largest x coordinate of a point in the grid with a non-nil value."
  [grid] (extreme-x grid max))

(defn- extreme-y [grid f]
  (apply f (keys grid)))

(defn min-y
  "The lowest y coordinate of a point in the grid with a non-nil value."
  [grid] (extreme-y grid min))

(defn max-y
  "The largest y coordinate of a point in the grid with a non-nil value."
  [grid] (extreme-y grid max))

(defn- extreme-point [grid fx fy]
  (let [x (fx grid)
        y (fy grid)]
    {:x x
     :y y
     :value (grid-get grid x y)}))

(defn top-left
  "The point furthest down and to the right that is not below or to the right
  of any points in the grid with a non-nil value.

  This point may itself have a nil value."
  [grid] (extreme-point grid min-x min-y))

(defn bottom-right
  "The point furthest up and to the left that is not above or to the left of
  any points in the grid with a non-nil value.

  This point may itself have a nil value."
  [grid] (extreme-point grid max-x max-y))

(defn distance
  "The manhattan distance between two points."
  [[x y] [x' y']]
  (+ (math/abs (- x x'))
     (math/abs (- y y'))))
