(ns advent2019.day-06
  (:require [clojure.string :refer [split]]))

(defn parse-orbits [input]
  (vec (map #(split % #"\)") (split input #"\n"))))

(defn load-orbits! [file]
  (parse-orbits (slurp file)))

(defn orbits-to-graph [orbits]
  (reduce (fn [graph [primary satellite]]
            (assoc graph primary (conj (get graph primary []) satellite)))
          {} orbits))

(defn- count-orbits' [graph primary ancestors]
  (let [satellites (get graph primary)]
    (if (empty? satellites)
      {:orbits (inc ancestors)}
      (reduce + 0 (map :orbits (map #(count-orbits' graph % (inc ancestors)) satellites))))))

(defn count-orbits [graph] (count-orbits' graph "COM" 0))

(defn solve! [file]
  (throw (Exception. "not yet unimplemented")))
