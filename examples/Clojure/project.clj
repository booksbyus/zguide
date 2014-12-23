(defproject zguide "1.0.0-SNAPSHOT"
  :source-paths ["."]
  :description "0MQ zguide in Clojure"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.zeromq/cljzmq "0.1.4"]
                 [org.clojure/core.match "0.3.0-alpha4"]]
  :profiles {:jeromq {:dependencies [[org.zeromq/jeromq "0.3.4"]]
                      :exclusions [jzmq]}}
  :jvm-opts ["-Djava.library.path=/usr/lib/:/usr/local/lib"]
  :aliases {"ex" ["run" "-m"]
            "jex" ["with-profile" "jeromq" "run" "-m"]})
