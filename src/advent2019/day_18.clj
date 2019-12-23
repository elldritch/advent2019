(ns advent2019.day-18
  (:require [loom.graph :as g]
            [loom.alg :as alg]
            [advent2019.lib.grid :as grid]
            [clojure.string :as str]))

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
       ((fn [maze] (assoc maze :graph (->> maze
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

(defn solve! [file]
  (println (input->maze (slurp file))))
