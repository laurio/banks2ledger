(ns banks2ledger.bayesian-test
  "Tests for Bayesian inference and tokenization logic."
  (:require
    [banks2ledger.bayesian :refer [best-accounts n-occur p-belong
                                   tokenize toktab-inc toktab-update]]
    [clojure.test :refer [deftest is testing]]))


(deftest test-toktab-inc
  (testing "toktab-inc"
    (is (= (toktab-inc {} ["Account" "tok1"])
           {"Account" {"tok1" 1}}))
    (is (= (toktab-inc {"Account" {"tok1" 1}} ["Account" "tok1"])
           {"Account" {"tok1" 2}}))
    (is (= (toktab-inc {"Account" {"tok1" 2}} ["Account" "tok2"])
           {"Account" {"tok1" 2, "tok2" 1}}))))


(deftest test-toktab-update
  (testing "toktab-update"
    (is (= (toktab-update {} {:accs ["Acc1" "Acc2"] :toks ["tok1" "tok2"]})
           {"Acc1" {"tok1" 1, "tok2" 1}, "Acc2" {"tok1" 1, "tok2" 1}}))
    (is (= (toktab-update {"Acc1" {"tok1" 1, "tok2" 1}
                           "Acc2" {"tok1" 1, "tok2" 1}}
                          {:accs ["Acc1" "Acc3"] :toks ["tok1" "tok3"]})
           {"Acc1" {"tok1" 2, "tok2" 1, "tok3" 1}
            "Acc2" {"tok1" 1, "tok2" 1}
            "Acc3" {"tok1" 1, "tok3" 1}}))))


(deftest test-tokenize
  (testing "tokenize"
    (is (= (tokenize "Forgalmi jutalék 01N00/702595/ számláról")
           ["FORGALMI" "JUTALÉK" "01N00" "702595" "SZÁMLÁRÓL"]))
    (is (= (tokenize "Vásárlás LIBRI ARKAD /N 20060923")
           ["VÁSÁRLÁS" "LIBRI" "ARKAD" "N" "YYYYMMDD"]))
    (is (= (tokenize "APOTEK HJART/16-03-21")
           ["APOTEK" "HJART" "YY-MM-DD"]))
    (is (= (tokenize "COOP KONSUM /16-03-17")
           ["COOP" "KONSUM" "YY-MM-DD"]))))


(deftest test-tokenize-edge-cases
  (testing "tokenize handles nil and blank inputs"
    (is (= (tokenize nil) []) "nil input should return empty vector")
    (is (= (tokenize "") []) "empty string should return empty vector")
    (is (= (tokenize "   ") []) "whitespace-only should return empty vector")
    (is (= (tokenize "  \t\n  ") []) "mixed whitespace should return empty vector")))


(deftest test-n-occur
  (testing "n-occur"
    (is (= (n-occur {"Acc1" {"tok1" 2, "tok2" 1, "tok3" 1}} "tok1" "Acc1") 2))
    (is (= (n-occur {"Acc1" {"tok1" 2, "tok2" 1, "tok3" 1}} "tok2" "Acc1") 1))
    (is (= (n-occur {"Acc1" {"tok1" 2, "tok2" 1, "tok3" 1}} "tok4" "Acc1") 0))
    (is (= (n-occur {"Acc1" {"tok1" 2, "tok2" 1, "tok3" 1}} "tok" "Acc2") 0))))


(deftest test-p-belong
  (testing "p-belong"
    (is (= 0.5
           (p-belong {"Acc1" {"tok1" 2, "tok2" 1}
                      "Acc2" {"tok1" 2, "tok2" 1}} "tok1" "Acc1")))
    (is (= 0.5
           (p-belong {"Acc1" {"tok1" 1, "tok2" 2}
                      "Acc2" {"tok1" 1, "tok2" 5}} "tok1" "Acc1")))
    (is (= 0.25
           (p-belong {"Acc1" {"tok1" 1, "tok2" 2}
                      "Acc2" {"tok1" 2, "tok2" 5}
                      "Acc3" {"tok1" 1, "tok2" 8}} "tok1" "Acc1")))))


(deftest test-best-accounts
  (testing "best-accounts"
    (is (= [[0.625 "Acc4"]
            [0.25 "Acc2"]
            [0.125 "Acc1"]]
           (best-accounts {"Acc1" {"tok1" 1, "tok2" 2}
                           "Acc2" {"tok1" 2, "tok2" 8}
                           "Acc3" {"tok2" 8}
                           "Acc4" {"tok1" 5, "tok2" 5}} "tok1")))))
