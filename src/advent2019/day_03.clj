(ns advent2019.day-03
  (:require [clojure.math.numeric-tower :refer [abs]]))

(defn parse [input]
  (map #(map (fn [[direction & length]]
               {:direction (case direction \L :left \R :right \U :up \D :down)
                :length (Integer/parseInt (apply str length))}) %)
       (map #(clojure.string/split % #",") (clojure.string/split-lines input))))

(defn move [[x y] {:keys [direction length]}]
  (case direction
    :up [x (+ y length)]
    :down [x (- y length)]
    :right [(+ x length) y]
    :left [(- x length) y]))

(defn acc-path-to-line [{lines :lines
                         [x y] :position
                         steps :steps}
                        {:keys [direction length] :as path}]
  (let [[x' y'] (move [x y] path)]
    {:position [x' y']
     :steps (+ steps length)
     :lines (case direction
              :up (cons {:vertical true :x x
                         :bottom y :top y'
                         :direction :up :steps steps} lines)
              :down (cons {:vertical true :x x
                           :bottom y' :top y
                           :direction :down :steps steps} lines)
              :right (cons {:horizontal true :y y
                            :left x :right x'
                            :direction :right :steps steps} lines)
              :left (cons {:horizontal true :y y
                           :left x' :right x
                           :direction :left :steps steps} lines))}))

(defn paths-to-lines [paths]
  ((reduce acc-path-to-line {:lines () :position [0 0] :steps 0} paths) :lines))

(defn between [low x high] (or (and (< low x) (< x high))
                               (and (< high x) (< x low))))

(defn distance [a b] (abs (- a b)))

(defn steps-to-intersection-of-line-from-position [{:keys [direction steps] :as line} [x y]]
  (case direction
    :up (+ steps (distance y (line :bottom)) (distance x (line :x)))
    :down (+ steps (distance y (line :top)) (distance x (line :x)))
    :right (+ steps (distance x (line :left)) (distance y (line :y)))
    :left (+ steps (distance x (line :right)) (distance y (line :y)))))

(defn line-intersects-path-from-position? [line {:keys [direction] :as path} [x y]]
  (let [[x' y'] (move [x y] path)]
    (cond
      (or (= direction :up)
          (= direction :down)) (and (line :horizontal)
                                      (between (line :left) x (line :right))
                                      (between y (line :y) y'))
      (or (= direction :right)
          (= direction :left)) (and (line :vertical)
                                      (between (line :bottom) y (line :top))
                                      (between x (line :x) x')))))

(defn line-intersection-with-path-from-position [line {:keys [direction]} [x y] path-steps]
  (cond
    (or (= direction :up)
        (= direction :down)) {:x x :y (line :y)
                                :steps (+ path-steps (steps-to-intersection-of-line-from-position line [x y]))}
    (or (= direction :right)
        (= direction :left)) {:x (line :x) :y y
                                :steps (+ path-steps (steps-to-intersection-of-line-from-position line [x y]))}))

(defn acc-intersections-of-path [{:keys [lines position steps intersections]} path]
  {:lines lines
   :position (move position path)
   :steps (+ steps (path :length))
   :intersections (concat intersections
                          (->> lines
                               (filter #(line-intersects-path-from-position? % path position))
                               (map #(line-intersection-with-path-from-position % path position steps))))})

(defn intersections-of-lines-and-paths [lines paths]
  ((reduce acc-intersections-of-path {:lines lines :position [0 0] :steps 0 :intersections ()} paths) :intersections))

(defn manhattan [{:keys [x y]}] (+ (abs x) (abs y)))

(defn min-intersection [f wire-1 wire-2]
  (let [wire-1-lines (paths-to-lines wire-1)
        intersections (intersections-of-lines-and-paths wire-1-lines wire-2)]
    (apply min-key f intersections)))

(defn load-file! [file] (parse (slurp file)))

(defn solve! [file]
  (let [[wire-1 wire-2] (load-file! file)
        closest (min-intersection manhattan wire-1 wire-2)
        fewest-steps (min-intersection :steps wire-1 wire-2)]
    (println "Closest intersection:" closest "Distance:" (manhattan closest))
    (println "Fewest step intersection:" fewest-steps "Steps:" (:steps fewest-steps))))
