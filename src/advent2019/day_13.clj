(ns advent2019.day-13
  (:require [advent2019.lib.intcode :as intcode]
            [advent2019.lib.grid :as grid]
            [clojure.string :as str]))

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

(defn reduce-frame [frame triplet]
  (let [tile (apply parse-triplet triplet)]
    (if (= (:type tile) :score)
      (assoc frame :score (:value tile))
      (update frame :tiles #(grid/grid-assoc %
                                             (:x tile)
                                             (:y tile)
                                             (:type tile))))))

(defn tick [continuation tiles score]
  (let [computed (intcode/gather-outputs continuation)
        frame (->> (:outputs computed)
                   (partition 3)
                   (reduce reduce-frame {:tiles tiles
                                         :score score}))]
    {:continuation (:continuation computed)
     :tiles (:tiles frame)
     :score (:score frame)}))

(defn show [frame]
  (format
   "Score: %s\n\n%s"
   (:score frame)
   (str/join
    \newline
    (map (fn [row]
           (apply str (map #(case (:value %)
                              :empty " "
                              :wall "W"
                              :block "B"
                              :horizontal-paddle "P"
                              :ball "O"
                              (throw (ex-info "unknown tile type"
                                              {:tile %})))
                           row)))
         (partition-by :y (grid/values (:tiles frame)))))))

(defn play [program]
  (loop [continuation (intcode/run-program (assoc program 0 2))
         tiles (grid/grid)
         score 0]
    (let [frame (tick continuation tiles score)
          continue (:continuation frame)
          status (:status continue)
          tile-list (grid/values (:tiles frame))
          paddle (first (filter #(= (:value %) :horizontal-paddle) tile-list))
          ball (first (filter #(= (:value %) :ball) tile-list))]
      (if (= status :halted)
        (:score frame)
        (recur (intcode/resume-program-with-input continue
                                                  (cond
                                                    (< (:x paddle) (:x ball)) 1
                                                    (< (:x ball) (:x paddle)) -1
                                                    :else 0))
               (:tiles frame)
               (:score frame))))))

(defn solve! [file]
  (let [program (intcode/load-program! file)
        tiles (:tiles (tick (intcode/run-program program) (grid/grid) 0))]
    (println "Block tiles:" (count (filter #(= (:type %) :block)
                                           tiles)))
    (println "Score on completion:" (play program))))
