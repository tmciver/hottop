(defproject hottop/hottop "0.1.4" 
  :min-lein-version "2.0.0"
  :profiles {:dev
             {:dependencies
              [[ring-mock "0.1.2"]]}}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [ring/ring-core "1.0.0-RC1"]
                 [clout "1.0.1"]]
  :description "HoTToP is a resource-oriented web application framework.")
