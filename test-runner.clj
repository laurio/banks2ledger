#!/usr/bin/env bb

(require '[clojure.test :as t]
         '[babashka.classpath :as cp])


(cp/add-classpath "src:test")

(require 'banks2ledger.banks2ledger-test)


(def test-results
  (t/run-tests 'banks2ledger.banks2ledger-test))


(def failures-and-errors
  (let [{:keys [fail error]} test-results]
    (+ fail error)))


(System/exit failures-and-errors)
