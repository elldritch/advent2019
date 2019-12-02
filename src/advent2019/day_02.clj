(ns advent2019.day-02)

(defn op [state pc f]
  (let [input-1-pos (nth state (+ pc 1))
        input-2-pos (nth state (+ pc 2))
        result-pos (nth state (+ pc 3))
        input-1 (nth state input-1-pos)
        input-2 (nth state input-2-pos)]
    {:state (assoc state result-pos (f input-1 input-2))
     :pc (+ pc 4)}))

(defn interpret-step [state pc]
  (let [instruction (nth state pc)]
    (case instruction
      1 (op state pc +)
      2 (op state pc *)
      99 {:state state :pc pc :done true}
      (throw (Exception. "unknown opcode")))))

(defn interpret [initial-state]
  (loop [state initial-state
         pc 0]
    (let [{state' :state pc' :pc done :done} (interpret-step state pc)]
      (if done
        state
        (recur state' pc')))))

(defn interpret-program [noun verb program]
  (let [program' (assoc program 1 noun)
        program'' (assoc program' 2 verb)]
    (first (interpret program''))))

(defn int-pairs
  ([] (int-pairs 0))
  ([n]
   (lazy-seq (concat
    (apply concat (map (fn [x] (map #(vector x %) (range n))) (range n)))
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

(defn load-program! [file]
  (vec (map #(Integer/parseInt %)
            (clojure.string/split (slurp file) #","))))

(defn solve! [file]
  (let [program (load-program! file)
        output (interpret-program 12 02 program)
        {:keys [noun verb]} (inputs-for 19690720 program)]
    (println "Value at position 0:" output)
    (println "Inputs for 19690720:"
             "noun:" noun
             "verb:" verb)
    (println "100 * noun + verb:" (+ verb (* noun 100)))))
