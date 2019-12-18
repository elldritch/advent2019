(ns advent2019.day-16)

(def base-pattern [0 1 0 -1])

(defn pattern [base position]
  (rest (apply concat (repeat (mapcat #(repeat position %) base)))))

(defn phase [digits]
  (mapv (fn [position]
         (let [output (reduce + (map * digits (pattern base-pattern position)))]
           (mod (if (pos? output) output (- output)) 10)))
       (range 1 (inc (count digits)))))

(defn phases [digits phase-count] (nth (iterate phase digits) phase-count))

(defn undigits [digits] (reduce #(+ (* 10 %1) %2) 0 digits))

(defn decode [digits]
  (let [offset (undigits (take 7 digits))
        signal (apply concat (repeat 10000 digits))
        final (phases signal 100)]
    (take 8 (nthrest final offset))))

(defn parse [input] (map #(Character/digit % 10) input))

(defn load-file! [file] (parse (slurp file)))

(defn solve! [file]
  (let [digits (load-file! file)]
    (println "First 8 digits after 100 phases of FFT:" (take 8 (phases digits 100)))
    (println "Decoded message:" (decode digits))))
