#!/usr/bin/env bb

(require '[clojure.test :as t]
         '[babashka.classpath :as cp]
         '[babashka.fs :as fs]
         '[babashka.process :refer [shell]])


(cp/add-classpath "src:test")


(require 'banks2ledger.bayesian-test
         'banks2ledger.csv-parser-test
         'banks2ledger.ledger-parser-test
         'banks2ledger.integration-test)


(def test-results
  (t/run-tests 'banks2ledger.bayesian-test
               'banks2ledger.csv-parser-test
               'banks2ledger.ledger-parser-test
               'banks2ledger.integration-test))


(defn check-bb
  []
  (print "testing bb.csv... ")
  (let [{:keys [exit out]}
        (shell {:out :string}
               "bb" "src/banks2ledger/banks2ledger.clj"
               "--ledger-file" "test/data/ledger.dat"
               "--csv-file" "test/data/bb.csv"
               "--csv-skip-header-lines" "3"
               "--csv-skip-trailer-lines" "2"
               "--date-format" "yyyy/MM/dd"
               "--ref-col" "3"
               "--amount-col" "4"
               "--descr-col" "%9!%1 %6 %7 %8"
               "--account" "Assets:BB Account"
               "--currency" "HUF")]
    (if (and (zero? exit)
             (= out (slurp "test/data/bb.ref-out")))
      (println "OK")
      (do
        (fs/delete-if-exists "test/data/bb.out")
        (spit "test/data/bb.out" out)
        (println "FAIL, inspect bb.out!")
        (System/exit exit)))))


(defn check-ica
  []
  (print "testing ica.csv... ")
  (let [{:keys [exit out]}
        (shell {:out :string}
               "bb" "src/banks2ledger/banks2ledger.clj"
               "--ledger-file" "test/data/ledger.dat"
               "--csv-file" "test/data/ica.csv"
               "--csv-field-separator" ";"
               "--csv-skip-header-lines" "1"
               "--amount-col" "4"
               "--descr-col" "%1"
               "--account" "Assets:ICA Account"
               "--amount-decimal-separator" ","
               "--amount-grouping-separator" " ")]
    (if (and (zero? exit)
             (= out (slurp "test/data/ica.ref-out")))
      (println "OK")
      (do
        (fs/delete-if-exists "test/data/ica.out")
        (spit "test/data/ica.out" out)
        (println "FAIL, inspect ica.out!")
        (System/exit exit)))))


(defn check-seb
  []
  (print "testing seb.csv... ")
  (let [{:keys [exit out]}
        (shell {:out :string}
               "bb" "src/banks2ledger/banks2ledger.clj"
               "--ledger-file" "test/data/ledger.dat"
               "--csv-file" "test/data/seb.csv"
               "--csv-skip-header-lines" "5"
               "--ref-col" "2"
               "--amount-col" "4"
               "--descr-col" "%3"
               "--hooks-file" "test/data/hooks.clj"
               "--account" "Assets:SEB Account")]
    (if (and (zero? exit)
             (= out (slurp "test/data/seb.ref-out")))
      (println "OK")
      (do
        (fs/delete-if-exists "test/data/seb.out")
        (spit "test/data/seb.out" out)
        (println "FAIL, inspect seb.out!")
        (System/exit exit)))))


(let [{:keys [fail error]} test-results
      issues-count (+ fail error)]
  (if (pos-int? issues-count)
    (System/exit issues-count)
    (do
      (println "\nNow running end-to-end tests:")
      (check-bb)
      (check-ica)
      (check-seb))))
