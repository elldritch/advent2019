(ns advent2019.day-04
  (:require [advent2019.lib :refer [digits]]))

(defn parse [text]
  (vec (map #(Integer/parseInt %) (clojure.string/split text #"-"))))

(defn two-adjacent-digits-same? [n]
  (:ok (reduce (fn [last d] (if (= last d) (reduced {:ok true}) d))
               nil
               (digits n))))

(defn non-decreasing-digits? [n]
  (not (:fail (reduce (fn [last d] (if (or (nil? last) (<= last d)) d (reduced {:fail true})))
                      nil
                      (digits n)))))

(defn meets-criteria? [n]
  (and
   (two-adjacent-digits-same? n)
   (non-decreasing-digits? n)))

(defn filter-inclusive-range [f low high]
  (filter f (range low (inc high))))

(def password-candidates-within-range
  (partial filter-inclusive-range meets-criteria?))

(defn two-adjacent-digits-same-not-part-of-larger-run? [n]
  (let [result (reduce (fn [{:keys [last run]} d]
                         (if (not= last d)
                           (if (= run 2)
                             (reduced {:ok true})
                             {:last d :run 1})
                           {:last d :run (inc run)}))
                       {:last nil :run 0} (digits n))]
    (or (:ok result) (= (result :run) 2))))

(defn meets-extra-criteria? [n]
  (and
   (meets-criteria? n)
   (two-adjacent-digits-same-not-part-of-larger-run? n)))

(def stricter-password-candidates-within-range
  (partial filter-inclusive-range meets-extra-criteria?))

(defn solve! [file]
  (let [input-range (parse (slurp file))
        passwords (apply password-candidates-within-range input-range)
        stricter-passwords (apply stricter-password-candidates-within-range input-range)]
    (println "Password examples:" (take 5 passwords))
    (println "Password count:" (count passwords))
    (println "Stricter password examples:" (take 5 stricter-passwords))
    (println "Stricter password count:" (count stricter-passwords))))
