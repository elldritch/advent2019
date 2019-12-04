(ns advent2019.day-04)

(defn parse [text]
  (vec (map #(Integer/parseInt %) (clojure.string/split text #"-"))))

(defn digits [n]
  (if (pos? n)
    (conj (digits (quot n 10)) (mod n 10))
    []))

(defn two-adjacent-digits-same? [n]
  (:ok (reduce (fn [last d]
                 (if (or (nil? last) (not= last d))
                   d
                   (reduced {:ok true})))
               nil (digits n))))

(defn non-decreasing-digits? [n]
  (:ok (reduce (fn [{:keys [last ok]} d]
                 (if (or (nil? last) (<= last d))
                   {:last d :ok ok}
                   (reduced {:ok false})))
               {:last nil :ok true} (digits n))))

(defn meets-criteria? [n]
  (and
   (two-adjacent-digits-same? n)
   (non-decreasing-digits? n)))

(defn password-candidates-within-range [low high]
  (filter meets-criteria? (range low (inc high))))

(defn solve! [file]
  (let [passwords (apply password-candidates-within-range (parse (slurp file)))]
    (println "Password examples:" (take 5 passwords))
    (println "Password count:" (count passwords))))