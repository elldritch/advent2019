(ns advent2019.day-08)

(defn load-file! [file] (map #(Character/digit % 10) (slurp file)))

(defn layers [image width height]
  (partition (* width height) image))

(defn checksum [image width height]
  (let [layers' (layers image width height)
        fewest-zeroes (apply min-key #(count (filter zero? %)) layers')
        freqs (frequencies fewest-zeroes)]
    (* (get freqs 1) (get freqs 2))))

(defn decode [image width height]
  (let [base (repeat (* width height) 2)]
    (reduce #(map (fn [top bottom] (if (= 2 top) bottom top)) %1 %2)
            base
            (layers image width height))))

(defn image-print [image width]
  (doall (map println (partition width image)))
  nil)

(defn solve! [file]
  (let [data (load-file! file)
        width 25
        height 6]
    (println "Checksum:" (checksum data width height))
    (println "Image:")
    (image-print (decode data width height) width)))
