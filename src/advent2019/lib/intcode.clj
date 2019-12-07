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
         :store-in (slot 1)}
      4 {:op :output
         :output (parameter' 1)}
      5 {:op :jump-if-true
         :test (parameter' 1)
         :jump-to (parameter' 2)}
      6 {:op :jump-if-false
         :test (parameter' 1)
         :jump-to (parameter' 2)}
      7 {:op :less-than
         :a (parameter' 1)
         :b (parameter' 2)
         :store-in (slot 3)}
      8 {:op :equals
         :a (parameter' 1)
         :b (parameter' 2)
         :store-in (slot 3)}
      99 {:op :halt}
      (throw (ex-info "unknown instruction"
                      {:instruction instruction
                       :pc pc
                       :state state})))))

(defn run-program [program inputs]
  (loop [state program
         input inputs
         outputs []
         pc 0]
    (let [i (next-instruction state pc)]
      (case (:op i)
        :add (recur (assoc state (i :store-in) (+ (i :a) (i :b)))
                    input
                    outputs
                    (+ pc 4))
        :multiply (recur (assoc state (i :store-in) (* (i :a) (i :b)))
                         input
                         outputs
                         (+ pc 4))
        :input (recur (assoc state (i :store-in) (peek input))
                      (pop input)
                      outputs
                      (+ pc 2))
        :output (recur state
                       input
                       (conj outputs (i :output))
                       (+ pc 2))
        :jump-if-true (recur state
                             input
                             outputs
                             (if (not= 0 (i :test)) (i :jump-to) (+ pc 3)))
        :jump-if-false (recur state
                              input
                              outputs
                              (if (zero? (i :test)) (i :jump-to) (+ pc 3)))
        :less-than (recur (assoc state (i :store-in) (if (< (i :a) (i :b)) 1 0))
                          input
                          outputs
                          (+ pc 4))
        :equals (recur (assoc state (i :store-in) (if (= (i :a) (i :b)) 1 0))
                       input
                       outputs
                       (+ pc 4))
        :halt {:state state :input input :outputs outputs :pc pc}))))
