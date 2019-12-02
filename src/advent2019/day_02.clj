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

(defn solve! [file]
  (let [program (vec (map #(Integer/parseInt %)
                          (clojure.string/split (slurp file) #",")))
        program' (assoc program 1 12)
        program'' (assoc program' 2 2)]
    (println "Value at position 0:"
             (first (interpret program'')))))