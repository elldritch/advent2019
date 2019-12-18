(ns advent2019.day-14
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]
            [loom.graph :as g]
            [loom.alg :as alg]))

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
       (mapv parse-equation)
       (reduce (fn [recipes {:keys [inputs]
                             {:keys [ingredient quantity]} :output}]
                 (assoc recipes
                        ingredient
                        {:inputs inputs
                         :quantity quantity}))
               {})))

(defn load-file! [file] (parse-recipes (slurp file)))

(defn smallest-multiple-above [x y]
  (let [multiplier (long (math/ceil (/ y x)))]
    {:multiple (* x multiplier)
     :multiplier multiplier}))

(defn scale-recipe [recipes ingredient amount]
  (let [recipe (recipes ingredient)
        quantity (:quantity recipe)
        multiplier (:multiplier (smallest-multiple-above quantity
                                                         amount))]
    {:inputs (reduce-kv (fn [m k v] (assoc m k (* v multiplier))) {} (:inputs recipe))
     :quantity (* multiplier quantity)}))

(defn reactions-graph [recipes target]
  (loop [graph (g/add-nodes (g/weighted-digraph) target)
         nodes [target]]
    (if (empty? nodes)
      graph
      (let [reduction
            (reduce (fn [acc node]
                      (let [inputs (:inputs (recipes node))
                            successors (keys inputs)]
                        {:graph (-> (:graph acc)
                                    (g/add-nodes* successors)
                                    (g/add-edges* (map #(vector node % (inputs %))
                                                       successors)))
                         :successors (into (:successors acc) successors)}))
                    {:graph graph
                     :successors #{}}
                    nodes)]
        (recur (:graph reduction)
               (:successors reduction))))))

(defn reaction-quantities [recipes ingredient amount]
  (let [rxg (reactions-graph recipes ingredient)]
    (reduce (fn [quantities ingredient]
              (assoc quantities
                     ingredient
                     (reduce (fn [acc predecessor]
                               (+ (get-in (scale-recipe recipes predecessor (quantities predecessor))
                                          [:inputs ingredient])
                                  acc))
                             0
                             (g/predecessors rxg ingredient))))
            {ingredient amount}
            (rest (alg/topsort rxg)))))

(defn min-ore [recipes ingredient amount]
  ((reaction-quantities recipes ingredient amount) "ORE"))

(defn halfway [high low]
  (+ (quot (- high low) 2) low))

(defn max-fuel [recipes ore]
  (loop [high (inc ore)
         low 0]
    (let [needle (halfway high low)
          needed (min-ore recipes "FUEL" needle)]
      (cond
        (<= high (inc low)) low
        (< ore needed) (recur needle low)
        (> ore needed) (recur high needle)
        :else (throw (ex-info "impossible" {:high high
                                            :low low
                                            :needle needle}))))))

(defn solve! [file]
  (let [recipes (load-file! file)]
    (println "Minimum ore to produce 1 fuel:" (min-ore recipes "FUEL" 1))
    (println "Maximum fuel produced by 1T ore:" (max-fuel recipes 1000000000000))))
