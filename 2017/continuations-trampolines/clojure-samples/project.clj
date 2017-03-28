(defproject clojure-samples "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Public Domain"
            :url "http://unlicense.org"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot clojure-samples.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
