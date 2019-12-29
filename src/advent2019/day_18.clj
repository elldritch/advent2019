(ns advent2019.day-18
  (:require [loom.graph :as g]
            [loom.alg :as alg]
            [advent2019.lib.grid :as grid]
            [clojure.string :as str]
            [clojure.set :as set]))

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
                position (dissoc node :value)]
            {:graph (g/add-nodes graph position)
             :keys (conj keys (when (Character/isLowerCase value) [value position]))
             :doors (conj doors (when (Character/isUpperCase value) [value position]))
             :entrance (if (= value \@) position entrance)}))
        {:graph (g/graph)
         :keys {}
         :doors {}
         :entrance nil})
       ((fn [maze]
          (update
           maze
           :graph
           (fn [graph]
             (let [is-not-door? (comp not (set (vals (:doors maze))))]
               (->> (g/nodes graph)
                    (filter is-not-door?)
                    (reduce
                     (fn [graph' node]
                       (->> (grid/adjacent node)
                            (filter is-not-door?)
                            (filter #(g/has-node? graph' %))
                            (map #(vector node %))
                            (g/add-edges* graph')))
                     graph)))))))))

(defn with-door [maze door]
  (let [position ((:doors maze) door)]
    (assoc maze :graph (->> (grid/adjacent position)
                            (filter #(g/has-node? (:graph maze) %))
                            (map #(vector position %))
                            (g/add-edges* (:graph maze))))))

(defn with-doors [maze doors] (reduce with-door maze doors))

(defn path-length [path] (if (empty? path) nil (dec (count path))))

(defn remaining-keys [maze waypoints]
  (set/difference (set (keys (:keys maze)))
                  (set waypoints)))

(defn has-all-keys? [maze waypoints] (empty? (remaining-keys maze waypoints)))

(defn unlock-maze-doors [maze waypoints]
  (with-doors maze (->> waypoints
                        (map #(Character/toUpperCase %))
                        (filter (set (keys (:doors maze))))
                        (set))))

(defn next-waypoints [maze waypoints]
  (let [position (if (empty? waypoints)
                   (:entrance maze)
                   ((:keys maze) (peek waypoints)))
        remaining (remaining-keys maze waypoints)
        maze' (unlock-maze-doors maze waypoints)]
    (if (empty? remaining)
      []
      (->> remaining
           (map (fn [k] {:key k :position ((:keys maze') k)}))
           (map #(assoc %
                        :distance
                        (path-length (alg/shortest-path (:graph maze')
                                                        position
                                                        (:position %)))))
           (filter (comp not nil? :distance))))))

(defn shortest-waypoints [maze]
  (loop [queue [{:distance 0
                 :waypoints []}]
         shortest nil]
    (if (empty? queue)
      shortest
      (let [{:keys [distance waypoints]} (peek queue)]
        (if (has-all-keys? maze waypoints)
          (if (or (nil? shortest) (< distance (:distance shortest)))
            (recur (pop queue) {:distance distance :waypoints waypoints})
            (recur (pop queue) shortest))
          (if (and (not (nil? shortest)) (> distance (:distance shortest)))
            (recur (pop queue) shortest)
            (recur (->> (next-waypoints maze waypoints)
                        (map (fn [wp]
                               {:distance (+ distance (:distance wp))
                                :waypoints (conj waypoints (:key wp))}))
                        (into (pop queue)))
                   shortest)))))))

(defn solve! [file]
  (let [maze (input->maze (slurp file))]
    (println (shortest-waypoints maze))))
