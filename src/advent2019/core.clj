(ns advent2019.core
  (:require [advent2019.day-01 :as day1]
            [advent2019.day-02 :as day2]
            [advent2019.day-03 :as day3]
            [advent2019.day-04 :as day4]
            [advent2019.day-05 :as day5]
            [advent2019.day-06 :as day6]
            [advent2019.day-07 :as day7]
            [advent2019.day-08 :as day8]
            [advent2019.day-09 :as day9]
            [advent2019.day-10 :as day10]
            [advent2019.day-11 :as day11]
            [advent2019.day-12 :as day12]
            [advent2019.day-13 :as day13]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

; Program runner

(def options
  [["-f" "--input-file FILE" "Path to file containing puzzle input"
    :id :file]
   ["-p" "--puzzle PUZZLE_DAY" "Day of puzzle"
    :id :puzzle
    :parse-fn #(Integer/parseInt %)]
   ["-h", "--help"]])

(defn errors-msg [errors] (clojure.string/join \newline errors))

(defn usage-msg [options-summary]
  (clojure.string/join \newline
                       [""
                        "Usage:"
                        ""
                        options-summary]))

(defn -main
  [& args]
  (let [{{:keys [file puzzle]} :options
         errors :errors
         summary :summary} (parse-opts args options)]
    (cond
      errors (do
               (println (str (errors-msg errors) \newline (usage-msg summary)))
               (System/exit 1))
      (nil? file) (do
                    (println "Error: input file must be specified.")
                    (System/exit 1))
      (nil? puzzle) (do
                      (println "Error: puzzle number must be specified.")
                      (System/exit 1))
      :else
      (case puzzle
        1 (day1/solve! file)
        2 (day2/solve! file)
        3 (day3/solve! file)
        4 (day4/solve! file)
        5 (day5/solve! file)
        6 (day6/solve! file)
        7 (day7/solve! file)
        8 (day8/solve! file)
        9 (day9/solve! file)
        10 (day10/solve! file)
        11 (day11/solve! file)
        12 (day12/solve! file)
        13 (day13/solve! file)
        (do
          (println "Error: invalid puzzle number")
          (System/exit 1))))))
