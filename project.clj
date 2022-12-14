(defproject banks2ledger "1.3.0"
  :description "Banks' CSV to ledger converter with probabilistic payment matching"
  :url "https://github.com/laurio/banks2ledger"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/data.csv "1.0.1"]]
  :main ^:skip-aot banks2ledger.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
