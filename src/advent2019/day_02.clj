(ns advent2019.day-02
  (:require [advent2019.lib.intcode :refer [load-program! run-program]]))

(defn interpret-program [noun verb program]
  (let [program' (assoc program 1 noun)
        program'' (assoc program' 2 verb)]
    (first (:state (run-program program'')))))

(defn int-pairs
  ([] (int-pairs 0))
  ([n]
   (lazy-seq
    (concat
     (mapcat (fn [x] (map #(vector x %) (range n))) (range n))
     (int-pairs (inc n))))))

(defn inputs-for [output program]
  (first
   (filter
    #(= (% :output) output)
    (map (fn [[noun verb]]
           {:noun noun
            :verb verb
            :output (interpret-program noun verb program)})
         (int-pairs)))))

(defn solve! [file]
  (let [program (load-program! file)
        output (interpret-program 12 02 program)
        {:keys [noun verb]} (inputs-for 19690720 program)]
    (println "Value at position 0:" output)
    (println "Inputs for 19690720:"
             "noun:" noun
             "verb:" verb)
    (println "100 * noun + verb:" (+ verb (* noun 100)))))
