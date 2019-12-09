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

(defn run-program-from-pc [program from-pc]
  (loop [state program
         pc from-pc]
    (let [i (next-instruction state pc)]
      (case (:op i)
        :add (recur (assoc state (:store-in i) (+ (:a i) (:b i)))
                    (+ pc 4))
        :multiply (recur (assoc state (:store-in i) (* (:a i) (:b i)))
                         (+ pc 4))
        :input {:status :needs-input
                :store-in (:store-in i)
                :state state
                :pc (+ pc 2)}
        :output {:status :has-output
                 :output (:output i)
                 :state state
                 :pc (+ pc 2)}
        :jump-if-true (recur state
                             (if (not= 0 (:test i)) (:jump-to i) (+ pc 3)))
        :jump-if-false (recur state
                              (if (zero? (:test i)) (:jump-to i) (+ pc 3)))
        :less-than (recur (assoc state (:store-in i) (if (< (:a i) (:b i)) 1 0))
                          (+ pc 4))
        :equals (recur (assoc state (:store-in i) (if (= (:a i) (:b i)) 1 0))
                       (+ pc 4))
        :halt {:status :halted
               :state state
               :pc pc}))))

(defn resume-program [continuation]
  (if (= (:status continuation) :has-output)
    (run-program-from-pc (:state continuation) (:pc continuation))
    (throw (ex-info "illegal program resumption" continuation))))

(defn resume-program-with-input [continuation input]
  (if (= (:status continuation) :needs-input)
    (run-program-from-pc (assoc (:state continuation) (:store-in continuation) input)
                         (:pc continuation))
    (throw (ex-info "illegal program input" continuation))))

(defn run-program [program] (run-program-from-pc program 0))

(defn gather-outputs [continuation]
  (loop [continuation' continuation
         outputs []]
    (if (= (:status continuation') :has-output)
      (recur (resume-program continuation') (conj outputs (:output continuation')))
      {:continuation continuation'
       :outputs outputs})))
