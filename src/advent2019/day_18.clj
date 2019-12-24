(ns advent2019.day-18
  (:require [loom.graph :as g]
            [loom.alg :as alg]
            [advent2019.lib.grid :as grid]
            [clojure.string :as str]
            [clojure.set :as s]))

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
            {:graph  (->> (filter #(g/has-node? graph' %)
                                  (grid/adjacent position))
                          (map #(vector position %))
                          (g/add-edges* graph'))
             :keys (conj keys (when (Character/isLowerCase value) [value position]))
             :doors (conj doors (when (Character/isUpperCase value) [value position]))
             :entrance (if (= value \@) position entrance)}))
        {:graph (g/graph)
         :keys {}
         :doors {}
         :entrance nil})
       ((fn [maze]
          (assoc maze :graph (->> maze
                                  (:doors)
                                  (vals)
                                  (mapcat #(g/out-edges (:graph maze) %))
                                  (g/remove-edges* (:graph maze))))))))

(defn with-door [maze door]
  (let [position ((:doors maze) door)]
    (assoc maze :graph (->> (grid/adjacent position)
                            (map #(vector position %))
                            (g/add-edges* (:graph maze))))))

(defn with-doors [maze doors] (reduce with-door maze doors))

(defn paths [maze from with-keys]
  (let [unlocked-doors (set (map #(Character/toUpperCase %) with-keys))
        unlocked-maze (with-doors maze unlocked-doors)
        remaining-keys (s/difference (set (keys (:keys maze))) with-keys)]
    (->> remaining-keys
         (map #(let [key-position ((:keys unlocked-maze) %)]
                 {:key %
                  :position key-position
                  :shortest-path (alg/shortest-path (:graph unlocked-maze)
                                                    from
                                                    key-position)}))
         (filter #(not (nil? (:shortest-path %))))
         (map #(update % :shortest-path count)))))

(def paths' (memoize paths))

(defn shortest-path-through-maze' [maze from with-keys path-length-so-far]
  (let [remaining-keys (s/difference (set (keys (:keys maze))) with-keys)
        result (map #(shortest-path-through-maze'
                         maze
                         (:position %)
                         (conj with-keys (:key %))
                         (+ path-length-so-far (:shortest-path %)))
                       (paths' maze from with-keys))]
    (if (empty? remaining-keys)
      path-length-so-far
      (apply min result))))

(defn solve! [file]
  (let [maze (input->maze (slurp file))]
    (println (shortest-path-through-maze' maze (:entrance maze) #{} 0))))
