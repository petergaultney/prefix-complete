(defproject autocomplete-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :main ^:skip-aot autocomplete-clj.core
  ;;  :jvm-opts ["-XX:MaxJavaStackTraceDepth=-1"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
