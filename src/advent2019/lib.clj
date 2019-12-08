(ns advent2019.lib)

(defn digits [n]
  (if (pos? n)
    (conj (digits (quot n 10)) (mod n 10))
    []))

(defn abs [n] (if (pos? n) n (* -1 n)))
