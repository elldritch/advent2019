(ns advent2019.lib.intcode
  (:require [advent2019.lib :refer [digits]]))

(defn load-program! [file]
  (vec (map #(Integer/parseInt %)
            (clojure.string/split (slurp file) #","))))

(defn parameter-mode [instruction-digits parameter-number]
  (case (if (> (+ 2 parameter-number) (count instruction-digits))
          0
          (nth instruction-digits (- (count instruction-digits) (+ 2 parameter-number))))
    0 :position
    1 :immediate
    (throw (ex-info "unknown parameter mode"
                    {:instruction instruction-digits :parameter-number parameter-number}))))

(defn next-instruction [state pc]
  (let [instruction (nth state pc)
        d (digits instruction)]
    (case (mod instruction 100)
      1 {:op :add
         :a (nth state (+ pc 1)) :a-mode (parameter-mode d 1)
         :b (nth state (+ pc 2)) :b-mode (parameter-mode d 2)
         :store-in (nth state (+ pc 3))}
      2 {:op :multiply
         :a (nth state (+ pc 1)) :a-mode (parameter-mode d 1)
         :b (nth state (+ pc 2)) :b-mode (parameter-mode d 2)
         :store-in (nth state (+ pc 3))}
      3 {:op :input
         :store-in (nth state (+ pc 1))}
      4 {:op :output
         :output (nth state (+ pc 1))
         :output-mode (parameter-mode d 1)}
      99 {:op :halt}
      (throw (ex-info "unknown instruction"
                      {:instruction instruction
                       :pc pc
                       :state state})))))

(defn get-parameter [state parameter mode]
  (case mode
    :position (nth state parameter)
    :immediate parameter))

(defn arithmetic [state {:keys [a a-mode b b-mode store-in]} f]
  (let [a-value (get-parameter state a a-mode)
        b-value (get-parameter state b b-mode)]
    (assoc state store-in (f a-value b-value))))

(defn run-program [program input]
  (loop [state program
         input input
         outputs []
         pc 0]
    (let [instruction (next-instruction state pc)]
      (case (:op instruction)
        :add (recur (arithmetic state instruction +)
                    input
                    outputs
                    (+ pc 4))
        :multiply (recur (arithmetic state instruction *)
                         input
                         outputs
                         (+ pc 4))
        :input (recur (assoc state (instruction :store-in) input)
                      input
                      outputs
                      (+ pc 2))
        :output (recur state
                       input
                       (conj outputs (get-parameter state
                                       (instruction :output)
                                       (instruction :output-mode)))
                       (+ pc 2))
        :halt {:state state :input input :outputs outputs :pc pc}))))
