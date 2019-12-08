(defproject advent2019 "0.1.0-SNAPSHOT"
  :description "Advent of Code 2019 solutions"
  :url "https://github.com/liftM/advent2019"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/tools.cli "0.4.2"]]
  :main ^:skip-aot advent2019.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
