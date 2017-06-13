(defproject clojure-blocking-async "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Public Domain"
            :url "http://unlicense.org"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.3.443"]
                 [criterium "0.4.4"]
                 [clj-http "3.6.1"]]
  :main ^:skip-aot clojure-blocking-async.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
