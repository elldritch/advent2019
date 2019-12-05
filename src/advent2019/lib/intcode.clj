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

(defn parameter [instruction state pc n]
  (let [parameter-value (nth state (+ pc n))]
    (case (parameter-mode (digits instruction) n)
      :position (nth state parameter-value)
      :immediate parameter-value)))

(defn next-instruction [state pc]
  (let [instruction (nth state pc)
        parameter' (partial parameter instruction state pc)
        slot (fn [n] (nth state (+ pc n)))]
    (case (mod instruction 100)
      1 {:op :add
         :a (parameter' 1)
         :b (parameter' 2)
         :store-in (slot 3)}
      2 {:op :multiply
         :a (parameter' 1)
         :b (parameter' 2)
         :store-in (slot 3)}
      3 {:op :input
         :store-in (slot 3)}
      4 {:op :output
         :output (parameter' 1)}
      99 {:op :halt}
      (throw (ex-info "unknown instruction"
                      {:instruction instruction
                       :pc pc
                       :state state})))))

(defn arithmetic [state {:keys [a b store-in]} f]
  (assoc state store-in (f a b)))

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
                       (conj outputs (instruction :output))
                       (+ pc 2))
        :halt {:state state :input input :outputs outputs :pc pc}))))
