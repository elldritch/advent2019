(ns advent2019.day-12
  (:require [clojure.math.numeric-tower :refer [abs lcm]]))

(defn load-file! [file]
  (->> file
       (slurp)
       (clojure.string/split-lines)
       (mapv #(->> (re-matcher #"<x=(-?[0-9]+?), y=(-?[0-9]+?), z=(-?[0-9]+?)>" %)
                   (re-find)
                   (rest)
                   (map (fn [n] (Integer/parseInt n)))
                   (zipmap [:x :y :z])
                   (assoc {:velocity {:x 0 :y 0 :z 0}} :position)))))

(defn closer [v axis a b]
  (+ (axis v)
     (cond
       (< (axis (:position a)) (axis (:position b))) 1
       (< (axis (:position b)) (axis (:position a))) -1
       :else 0)))

(defn apply-gravity [moons]
  (vec (map-indexed
        (fn [i m]
          {:position (:position m)
           :velocity (reduce-kv
                      (fn [v j m']
                        (if (= i j)
                          v
                          {:x (closer v :x m m')
                           :y (closer v :y m m')
                           :z (closer v :z m m')}))
                      (:velocity m)
                      moons)})
        moons)))

(defn apply-velocity [moons]
  (mapv (fn [{:keys [velocity position]}]
          {:velocity velocity
           :position (merge-with + velocity position)})
        moons))

(defn time-step [moons]
  (->> moons
       (apply-gravity)
       (apply-velocity)))

(defn energyv [v] (reduce-kv (fn [e _ v'] (+ (abs v') e)) 0 v))

(defn energy [moon]
  (* (energyv (:position moon))
     (energyv (:velocity moon))))

(defn total-energy [moons]
  (->> moons
       (map energy)
       (reduce + 0)))

(defn simulate [initial-moons steps]
  (last (take (inc steps) (iterate time-step initial-moons))))

(defn fixpoint [f k x]
  (loop [i 0
         seen {}
         current x]
    (if (contains? seen (k current))
      {:first-seen (seen (k current))
       :repeat-seen i}
      (recur (inc i)
             (assoc seen (k current) i)
             (f current)))))

(defn moon-axis [axis]
  #(concat (map axis (map :position %))
           (map axis (map :velocity %))))

(defn moon-period [moons]
  (let [fix-x (fixpoint time-step (moon-axis :x) moons)
        fix-y (fixpoint time-step (moon-axis :y) moons)
        fix-z (fixpoint time-step (moon-axis :z) moons)
        ; TODO: in general, we also need to take initial offsets into account
        ; to compute when the cycles align, but it looks like these cycles all
        ; start at state zero.
        period-x (- (:repeat-seen fix-x) (:first-seen fix-x))
        period-y (- (:repeat-seen fix-y) (:first-seen fix-y))
        period-z (- (:repeat-seen fix-z) (:first-seen fix-z))]
    (lcm (lcm period-x period-y) period-z)))

(defn solve! [file]
  (let [moons (load-file! file)]
    (println "Total energy after 1000 steps:" (total-energy (simulate moons 1000)))
    (println "Steps before repeating a previous state:" (moon-period moons))))
