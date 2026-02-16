(ns banks2ledger.bayesian-test
  "Tests for Bayesian inference and tokenization logic."
  (:require
    [banks2ledger.bayesian :refer [bayes* best-accounts decide-account n-occur
                                   p-belong tokenize toktab-inc toktab-update]]
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


(deftest test-bayes*
  (testing "normal probabilities"
    (let [result (bayes* [0.6 0.8 0.9])]
      (is (< 0.9 result 1.0) "combined high probabilities should be very high")))
  (testing "all-zero probabilities"
    (is (= 0.0 (bayes* [0.0 0.0])) "all-zero probs yield 0.0"))
  (testing "all-one probabilities"
    (is (= 1.0 (bayes* [1.0 1.0])) "all-one probs yield 1.0"))
  (testing "mixed zero and one probabilities (NaN bug case)"
    (is (= 0.0 (bayes* [0.0 1.0])) "mixed 0.0 and 1.0 should return 0.0, not NaN")
    (is (= 0.0 (bayes* [1.0 0.0])) "mixed 1.0 and 0.0 should return 0.0, not NaN"))
  (testing "empty input"
    (is (= 0.0 (bayes* [])) "empty input should return 0.0"))
  (testing "single value"
    (is (= 0.6 (bayes* [0.6])) "single value should pass through unchanged")))


(deftest test-decide-account
  (let [acc-maps {"Expenses:Food"    {"GROCERY" 5, "STORE" 3}
                  "Expenses:Transport" {"BUS" 4, "TRAIN" 2}
                  "Income:Salary"    {"EMPLOYER" 10}}]
    (testing "clear winner returns the top account"
      (is (= "Expenses:Food"
             (decide-account acc-maps "GROCERY STORE" "Income:Salary" {}))))
    (testing "single-result match returns that account"
      (is (= "Income:Salary"
             (decide-account acc-maps "EMPLOYER" "Expenses:Food" {}))))
    (testing "empty results return Unknown"
      (is (= "Unknown"
             (decide-account acc-maps "NONEXISTENT" "Income:Salary" {}))))
    (testing "tied top-2 probabilities return Unknown"
      (let [tied-maps {"Expenses:A" {"TOK" 3}
                       "Expenses:B" {"TOK" 3}}]
        (is (= "Unknown"
               (decide-account tied-maps "TOK" "Income:X" {})))))))
