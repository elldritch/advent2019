(ns advent2019.day-06
  (:require [clojure.string :refer [split]]))

(defn parse-orbits [input]
  (vec (map #(split % #"\)") (split input #"\n"))))

(defn load-orbits! [file]
  (parse-orbits (slurp file)))

(defn orbits-to-graph [orbits]
  (reduce (fn [graph [primary satellite]]
            (update graph primary #(conj (if (nil? %) [] %) satellite)))
          {} orbits))

(defn- count-orbits' [graph primary ancestors]
  (let [satellites (graph primary)]
    (if (empty? satellites)
      ancestors
      (+ ancestors
         (reduce + 0 (map #(count-orbits' graph % (inc ancestors)) satellites))))))

(defn count-orbits [graph] (count-orbits' graph "COM" 0))

(defn graph-to-primaries [graph]
  (reduce-kv (fn [primaries primary satellites]
               (reduce #(assoc %1 %2 primary) primaries satellites))
             {} graph))

(defn minimum-orbital-transfers [graph start end]
  (let [primaries (graph-to-primaries graph)]
    (loop [{:keys [distance from] current :body} {:body start
                                                  :distance 0
                                                  :from nil}
           queue []]
      (let [satellites (graph current)
            primary (primaries current)
            adjacent (disj (set (conj satellites primary)) from)
            queue' (into (mapv #(assoc {:distance (inc distance)
                                        :from current}
                                       :body %)
                               adjacent) queue)]
        (cond
          (= current end) distance
          (empty? queue') :unreachable
          :else (recur
                 (peek queue')
                 (pop queue')))))))

(defn solve! [file]
  (let [graph (orbits-to-graph (load-orbits! file))]
    (println "Orbits count:" (count-orbits graph))
    (println "Minimum orbital transfers:" (- (minimum-orbital-transfers graph "YOU" "SAN") 2))))
