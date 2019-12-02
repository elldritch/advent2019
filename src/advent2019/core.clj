(ns advent2019.core
  (:require [advent2019.day-01 :as day1]
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
        (do
          (println "Error: invalid puzzle number")
          (System/exit 1))))))
