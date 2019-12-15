(ns advent2019.day-13
  (:require [advent2019.lib.intcode :as intcode]))

(defn parse-tile-id [tile-id]
  (case tile-id
    0 :empty
    1 :wall
    2 :block
    3 :horizontal-paddle
    4 :ball))

(defn parse-triplet [x y z]
  (if (and (= x -1) (= y 0))
    {:type :score
     :value z}
    {:type (parse-tile-id z)
     :x x
     :y y}))

(defn tick [continuation]
  (let [frame (intcode/gather-outputs continuation)
        triplets (->> (:outputs frame)
                      (partition 3)
                      (map #(apply parse-triplet %))
                      (group-by #(= (:type %) :score)))]
    {:continuation (:continuation frame)
     :tiles (get triplets false)
     :score (:value (get triplets true {:value 0}))}))

(defn display [frame]
  (format
   "Score: %s\n\n%s"
   (:score frame)
   (clojure.string/join
    \newline
    (map (fn [row]
           (apply str (map #(case (:type %)
                              :empty " "
                              :wall "W"
                              :block "B"
                              :horizontal-paddle "P"
                              :ball "O") row)))
         (partition-by :y (sort-by :y (sort-by :x (:tiles frame))))))))

(defn play! [program]
  (loop [continuation (intcode/run-program (assoc program 0 2))]
    (let [frame (tick continuation)
          cont (:continuation frame)
          status (:status cont)]
      (println (display frame))
      (println status)
      (println (:tiles frame))
      (if (= status :halted)
        (:score frame)
        (do
          (print "Next joystick input: ")
          (flush)
          (recur (intcode/resume-program-with-input cont (Integer/parseInt (read-line)))))))))

(defn solve! [file]
  (let [program (intcode/load-program! file)
        tiles (:tiles (tick (intcode/run-program program)))]
    (println "Block tiles:" (count (filter #(= (:type %) :block)
                                           tiles)))
    (play! program)))
