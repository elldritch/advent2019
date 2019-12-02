(ns advent2019.day-01)

(defn fuel-needed [mass] (- (int (Math/floor (/ mass 3))) 2))

(defn tsiolkovsky [mass]
  (loop [fuel 0
         delta (fuel-needed mass)]
    (if (<= delta 0)
      fuel
      (recur (+ fuel delta) (fuel-needed delta)))))

(defn- fuel-sum [compute-fuel module-masses]
  (reduce + (map compute-fuel (map #(Integer/parseInt %) module-masses))))

(defn solve! [input-file]
  (with-open [rdr (clojure.java.io/reader input-file)]
    (let [masses (line-seq rdr)]
      (println "Fuel needed for module masses alone:" (fuel-sum fuel-needed masses))
      (println "Total fuel needed:" (fuel-sum tsiolkovsky masses)))))