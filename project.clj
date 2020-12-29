(defproject advent-of-code-2020 "0.1.0-SNAPSHOT"
  :description "Jeff's 2020 Advent of Code project"
  :url "http://github.com/jeff303/advent-of-code-2020"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [com.taoensso/tufte "2.2.0"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :dev {:resource-paths ["test/resources"]}}
  :plugins [[lein-exec "0.3.7"]])
