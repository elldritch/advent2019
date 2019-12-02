(ns advent2019.01-tyranny-of-the-rocket-equation)

(defn fuel-needed [mass] (- (int (Math/floor (/ mass 3))) 2))