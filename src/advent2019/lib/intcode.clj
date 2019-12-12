(ns advent2019.lib.intcode)

(defn load-program! [file]
  (vec (map #(Long/parseLong %)
            (clojure.string/split (slurp file) #","))))

(defn opcode [instruction] (mod instruction 100))

(defn parameter-mode [instruction parameter-number]
  (case (mod (quot instruction (int (Math/pow 10 (+ 1 parameter-number)))) 10)
    0 :position
    1 :immediate
    2 :relative
    (throw (ex-info "unknown parameter mode"
                    {:instruction instruction :parameter-number parameter-number}))))

(defn read-parameter [instruction state pc relative-base n]
  (let [parameter-value (get state (+ pc n) 0)]
    (case (parameter-mode instruction n)
      :position (get state parameter-value 0)
      :immediate parameter-value
      :relative (get state (+ parameter-value relative-base) 0))))

(defn write-parameter [instruction state pc relative-base n]
  (let [parameter-value (get state (+ pc n) 0)]
    (case (parameter-mode instruction n)
      :position parameter-value
      :immediate (ex-info "write parameters may never be in immediate mode"
                          {:instruction instruction})
      :relative (+ parameter-value relative-base))))

(defn next-instruction [state pc relative-base]
  (let [instruction (get state pc 0)
        read-parameter' (partial read-parameter instruction state pc relative-base)
        write-parameter' (partial write-parameter instruction state pc relative-base)]
    (case (opcode instruction)
      1 {:op :add
         :a (read-parameter' 1)
         :b (read-parameter' 2)
         :store-in (write-parameter' 3)}
      2 {:op :multiply
         :a (read-parameter' 1)
         :b (read-parameter' 2)
         :store-in (write-parameter' 3)}
      3 {:op :input
         :store-in (write-parameter' 1)}
      4 {:op :output
         :output (read-parameter' 1)}
      5 {:op :jump-if-true
         :test (read-parameter' 1)
         :jump-to (read-parameter' 2)}
      6 {:op :jump-if-false
         :test (read-parameter' 1)
         :jump-to (read-parameter' 2)}
      7 {:op :less-than
         :a (read-parameter' 1)
         :b (read-parameter' 2)
         :store-in (write-parameter' 3)}
      8 {:op :equals
         :a (read-parameter' 1)
         :b (read-parameter' 2)
         :store-in (write-parameter' 3)}
      9 {:op :adjust-relative-base
         :adjust-by (read-parameter' 1)}
      99 {:op :halt}
      (throw (ex-info "unknown instruction"
                      {:instruction instruction
                       :pc pc
                       :state state})))))

(defn run-continuation [continuation]
  (loop [state (:state continuation)
         pc (:pc continuation)
         relative-base (:relative-base continuation)]
    (let [i (next-instruction state pc relative-base)]
      (case (:op i)
        :add (recur (assoc state (:store-in i) (+ (:a i) (:b i)))
                    (+ pc 4)
                    relative-base)
        :multiply (recur (assoc state (:store-in i) (* (:a i) (:b i)))
                         (+ pc 4)
                         relative-base)
        :input {:status :needs-input
                :store-in (:store-in i)
                :state state
                :pc (+ pc 2)
                :relative-base relative-base}
        :output {:status :has-output
                 :output (:output i)
                 :state state
                 :pc (+ pc 2)
                 :relative-base relative-base}
        :jump-if-true (recur state
                             (if (not= 0 (:test i)) (:jump-to i) (+ pc 3))
                             relative-base)
        :jump-if-false (recur state
                              (if (zero? (:test i)) (:jump-to i) (+ pc 3))
                              relative-base)
        :less-than (recur (assoc state (:store-in i) (if (< (:a i) (:b i)) 1 0))
                          (+ pc 4)
                          relative-base)
        :equals (recur (assoc state (:store-in i) (if (= (:a i) (:b i)) 1 0))
                       (+ pc 4)
                       relative-base)
        :adjust-relative-base (recur state
                                     (+ pc 2)
                                     (+ relative-base (:adjust-by i)))
        :halt {:status :halted
               :state state
               :pc pc
               :relative-base relative-base}))))

(defn resume-program [continuation]
  (if (= (:status continuation) :has-output)
    (run-continuation continuation)
    (throw (ex-info "illegal program resumption" continuation))))

(defn resume-program-with-input [continuation input]
  (let [{:keys [status state store-in]} continuation]
    (if (= status :needs-input)
      (run-continuation (assoc continuation :state (assoc state store-in input)))
      (throw (ex-info "illegal program input" continuation)))))

(defn run-program [program]
  (run-continuation {:state (zipmap (range) program)
                     :pc 0
                     :relative-base 0}))

(defn gather-outputs [continuation]
  (loop [continuation' continuation
         outputs []]
    (if (= (:status continuation') :has-output)
      (recur (resume-program continuation') (conj outputs (:output continuation')))
      {:continuation continuation'
       :outputs outputs})))
