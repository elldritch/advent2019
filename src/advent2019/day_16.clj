(ns advent2019.day-16)

(def base-pattern [0 1 0 -1])

(defn pattern [base position]
  (rest (cycle (mapcat #(repeat position %) base))))

(defn phase [digits]
  (mapv (fn [position]
          (let [output (reduce + (map * digits (pattern base-pattern position)))]
            (mod (if (pos? output) output (- output)) 10)))
        (range 1 (inc (count digits)))))

(defn phases [digits phase-count] (nth (iterate phase digits) phase-count))

(defn undigits [digits] (reduce #(+ (* 10 %1) %2) 0 digits))

(defn phase'
  "phase' is a highly optimized phase computation hack that only works for
  computing FFT values after a high offset. It builds the FFT from the last
  element of the input by adding modulo 10 at each value.

  This works because, at high offset, the pattern multipliers will all be 1s
  until they become all 0s."
  [digits offset phase-count]
  (let [signal-length (* 10000 (count digits))
        tail-length (- signal-length offset)]
    (loop [i 0
           phase-digits digits]
      (let [reversed (take tail-length (cycle (reverse phase-digits)))]
        (if (= i phase-count)
          phase-digits
          (recur (inc i)
                 (reduce (fn [xs x]
                           ; Modulo is distributive.
                           ; Since -1 is never part of the pattern, no need to
                           ; worry about the enegative case here.
                           (conj xs (mod (+ x (if (empty? xs) 0 (first xs))) 10)))
                         '()
                         reversed)))))))

(defn decode
  "decode is a highly optimized hack that only works for the specific puzzle
  inputs. In particular, it only works when the offset > 3/4 * (signal length).

  When offset > 3/4 * (signal length), the FFT pattern is all leading zeroes and
  trailing ones. We use this to dramatically simplify the calculation. See
  phase' for details."
  [digits]
  (let [offset (undigits (take 7 digits))]
    (assert (> offset (* (/ 3 4) (* 10000 (count digits)))))
    (let [final (phase' digits offset 100)]
      (take 8 final))))

(defn parse [input] (map #(Character/digit % 10) input))

(defn load-file! [file] (parse (slurp file)))

(defn solve! [file]
  (let [digits (load-file! file)]
    (println "First 8 digits after 100 phases of FFT:" (take 8 (phases digits 100)))
    (println "Decoded message:" (decode digits))))
