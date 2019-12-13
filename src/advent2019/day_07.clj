(ns advent2019.day-07
  (:require [advent2019.lib.intcode :as intcode]))

(defn setup-amplifier [program phase]
  {:continuation (intcode/resume-program-with-input (intcode/run-program program) phase)
   :signal nil
   :done false})

(defn run-amplifier [amplifier signal]
  (let [output (intcode/resume-program-with-input (:continuation amplifier) signal)
        continuation (intcode/resume-program output)]
    {:continuation continuation
     :signal (:output output)
     :done (= (:status continuation) :halted)}))

(defn thruster-signal [program a b c d e]
  (reduce (fn [signal phase]
            (let [amp (setup-amplifier program phase)
                  amp' (run-amplifier amp signal)]
              (:signal amp')))
          0 [a b c d e]))

(defn feedback-thruster-signal [program a b c d e]
  (let [setup (partial setup-amplifier program)]
    (loop [amplifiers {:a (setup a)
                       :b (setup b)
                       :c (setup c)
                       :d (setup d)
                       :e (setup e)}
           feedback-signal 0]
      (let [a' (run-amplifier (:a amplifiers) feedback-signal)
            b' (run-amplifier (:b amplifiers) (:signal a'))
            c' (run-amplifier (:c amplifiers) (:signal b'))
            d' (run-amplifier (:d amplifiers) (:signal c'))
            e' (run-amplifier (:e amplifiers) (:signal d'))]
        (if (:done e')
          (:signal e')
          (recur {:a a'
                  :b b'
                  :c c'
                  :d d'
                  :e e'}
                 (:signal e')))))))

(defn permutations [xs]
  (if (= 1 (count xs))
    [[(first xs)]]
    (let [elements (set xs)]
      (vec (mapcat
            (fn [x] (mapv #(into [x] %) (permutations (disj elements x))))
            elements)))))

(defn max-signal [amplify phases]
  (apply max-key :signal
         (map #(assoc {:phases %} :signal (apply amplify %)) phases)))

(defn max-thruster-signal [program]
  (max-signal (partial thruster-signal program)
              (permutations [0 1 2 3 4])))

(defn max-feedback-thruster-signal [program]
  (max-signal (partial feedback-thruster-signal program)
              (permutations [5 6 7 8 9])))

(defn solve! [file]
  (let [amplifier-program (intcode/load-program! file)]
    (println "Max thruster signal:" (max-thruster-signal amplifier-program))
    (println "Max feedback thruster signal:" (max-feedback-thruster-signal amplifier-program))))
