(ns advent2019.day-18
  (:require [loom.graph :as g]
            [loom.alg :as alg]
            [advent2019.lib.grid :as grid]
            [clojure.string :as str]
            [clojure.set :as s]))

(defn add-bidirectional-edge [graph a b] (g/add-edges graph [a b] [b a]))

(defn input->maze [input]
  (->> input
       (str/split-lines)
       (map-indexed
        (fn [y row]
          (map-indexed
           (fn [x c]
             {:x x
              :y y
              :value c})
           row)))
       (apply concat)
       (filter (comp not #{\#} :value))
       (reduce
        (fn [{:keys [graph keys doors entrance]} node]
          (let [value (:value node)
                position (dissoc node :value)
                graph' (g/add-nodes graph position)]
            {:graph  (->> (filter #(g/has-node? graph' %) (grid/adjacent position))
                          (reduce #(add-bidirectional-edge %1 position %2) graph'))
             :keys (conj keys (when (Character/isLowerCase value) [value position]))
             :doors (conj doors (when (Character/isUpperCase value) [value position]))
             :entrance (if (= value \@) position entrance)}))
        {:graph (g/digraph)
         :keys {}
         :doors {}
         :entrance nil})
       ((fn [maze]
          (assoc maze :graph (->> maze
                                  (:doors)
                                  (vals)
                                  (mapcat #(concat (g/out-edges (:graph maze) %)
                                                   (g/in-edges (:graph maze) %)))
                                  (g/remove-edges* (:graph maze))))))))

(defn with-door [maze door]
  (let [position ((:doors maze) door)
        graph (:graph maze)]
    (assoc maze :graph (->> (grid/adjacent position)
                            (filter #(g/has-node? graph %))
                            (reduce #(add-bidirectional-edge %1 position %2)
                                    graph)))))

(defn with-doors [maze doors] (reduce with-door maze doors))

(defn path-length [path] (dec (count path)))

(defn paths [maze from collected-keys]
  (let [unlocked-doors (->> collected-keys
                            (map #(Character/toUpperCase %))
                            (filter (set (keys (:doors maze))))
                            (set))
        unlocked-maze (with-doors maze unlocked-doors)
        remaining-keys (s/difference (set (keys (:keys maze))) collected-keys)]
    (->> remaining-keys
         (map #(let [key-position ((:keys unlocked-maze) %)
                     sp (alg/shortest-path (:graph unlocked-maze) from key-position)]
                 {:key %
                  :position key-position
                  :reachable (not (nil? sp))
                  :distance (path-length sp)}))
         (filter :reachable)
         (map #(dissoc % :reachable)))))

(def paths' (memoize paths))

(defn waypoints-length [waypoints] (reduce + 0 (map :distance waypoints)))

(defn shortest-path-waypoints' [maze from waypoints]
  (let [collected-keys (set (map :key waypoints))
        remaining-keys (s/difference (set (keys (:keys maze))) collected-keys)]
    (if (empty? remaining-keys)
      waypoints
      (apply min-key
             waypoints-length
             (map #(shortest-path-waypoints' maze (:position %) (conj waypoints %))
                  (paths' maze from collected-keys))))))

(defn shortest-path-waypoints [maze] (shortest-path-waypoints' maze (:entrance maze) []))

(defn solve! [file]
  (let [maze (input->maze (slurp file))]
    (println (shortest-path-waypoints maze))))
