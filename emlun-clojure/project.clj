(defproject adventofcode-2018 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Unlicense"
            :url "http://unlicense.org"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]]
  :main ^:skip-aot adventofcode.2018.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
