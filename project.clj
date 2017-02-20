(defproject hayek "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-git-deps "0.0.1-SNAPSHOT"]]
  :git-dependencies [["https://github.com/lunkdjedi/clj-ta-lib"]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj-ta-lib/clj-ta-lib "0.0.1"]
                 [com.hypirion/clj-xchart "0.2.0"]
                 [clj-time "0.13.0"]]
  :main ^:skip-aot hayek.core
  :repositories {"local" ~(str (.toURI (java.io.File. "maven_repository")))}
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
