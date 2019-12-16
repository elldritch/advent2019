(ns advent2019.day-14
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(defn parse-element [input]
  (let [[quantity ingredient] (-> (str/trim input)
                                  (str/split #" "))]
    {ingredient (Integer/parseInt quantity)}))

(defn parse-element-list [input]
  (->> (str/split input #",")
       (map parse-element)
       (apply merge)))

(defn parse-equation [input]
  (let [[inputs output] (str/split input #"=>")
        output' (parse-element output)]
    {:inputs (parse-element-list inputs)
     :output {:ingredient (first (keys output'))
              :quantity (first (vals output'))}}))

(defn parse-recipes [input]
  (->> input
       (str/split-lines)
       (mapv parse-equation)))

(defn load-file! [file] (parse-recipes (slurp file)))

(defn find-recipe [recipes ingredient]
  (first (filter #(= (get-in % [:output :ingredient]) ingredient) recipes)))

(defn smallest-multiple-of-x-above-y [x y]
  (let [multiplier (int (math/ceil (/ y x)))]
    {:multiple (* x multiplier)
     :multiplier multiplier}))

(defn scale-recipe [recipe output-quantity]
  (let [multiplier (:multiplier (smallest-multiple-of-x-above-y (get-in recipe [:output :quantity])
                                                                output-quantity))]
    {:inputs (reduce-kv (fn [m k v] (assoc m k (* v multiplier))) {} (:inputs recipe))
     :output (update (:output recipe) :quantity (partial * multiplier))}))

(defn recipe-for [recipes ingredient amount]
  (let [recipe (find-recipe recipes ingredient)
        scaled (scale-recipe recipe amount)
        scaled-amount (get-in scaled [:output :quantity])]
    {:inputs (:inputs scaled)
     :output (assoc (:output scaled) :leftover (max (- scaled-amount amount) 0))}))

; mapcat over inputs, get their scaled recipes
; put extra output on outputs side to be consumed by recipes
; iterate until fixpoint
; ORE as input ingredient is handled as special case
; 
; maybe this needs dynamic programming? reduce over (ways to create an X, ore needed)

(defn reduce-input [{:keys [leftovers inputs]} ingredient quantity]
  ())

(defn ore-needed-for-fuel [recipes]
  (loop [inputs (:inputs (recipe-for recipes "FUEL" 1))
         leftovers []]
    (println inputs)
    (println (reduce-kv reduce-input
                     {:leftovers leftovers
                      :inputs []}
                     inputs))
    (println leftovers)))

(defn solve! [file]
  (let [recipes (load-file! file)]
    ()))
