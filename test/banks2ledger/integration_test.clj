(ns banks2ledger.integration-test
  "Integration tests for CLI options, hooks, and end-to-end functionality."
  (:require
    [banks2ledger.banks2ledger :refer [cli-options valid-descr-col-spec?]]
    [banks2ledger.csv-parser :refer [format-value]]
    [banks2ledger.ledger-parser :refer [print-ledger-entry!]]
    [clojure.string :as string]
    [clojure.test :refer [deftest is testing]]
    [clojure.tools.cli :as tools-cli]))


;; String containing the platform-dependent newline character, to verify
;; that all tested output is produced with this newline encoding.
(def NL (with-out-str (println)))


;; Tests for descr-col spec validation

(deftest test-valid-descr-col-spec
  (testing "valid-descr-col-spec? accepts valid specs"
    (is (valid-descr-col-spec? "%0") "Single column reference")
    (is (valid-descr-col-spec? "%1") "Different column reference")
    (is (valid-descr-col-spec? "%123") "Large column number")
    (is (valid-descr-col-spec? "%0 %1") "Multiple columns with space")
    (is (valid-descr-col-spec? "%0 %1 %2") "Three columns")
    (is (valid-descr-col-spec? "%1!%2") "Alternative specs")
    (is (valid-descr-col-spec? "%4!%1 %2 %3!%7") "Complex alternatives")
    (is (valid-descr-col-spec? "prefix %0 suffix") "Column with literal text")
    (is (valid-descr-col-spec? "%0-%1") "Columns with delimiter")))


(deftest test-invalid-descr-col-spec
  (testing "valid-descr-col-spec? rejects invalid specs"
    (is (not (valid-descr-col-spec? "")) "Empty string")
    (is (not (valid-descr-col-spec? "   ")) "Whitespace only")
    (is (not (valid-descr-col-spec? "no refs")) "No column references")
    (is (not (valid-descr-col-spec? "%-5")) "Negative column index")
    (is (not (valid-descr-col-spec? "%abc")) "Non-numeric column reference")
    (is (not (valid-descr-col-spec? "%")) "Incomplete column reference")
    (is (not (valid-descr-col-spec? "just text")) "Text without column refs")))


;; Tests for --debug option validation

(deftest test-debug-option-valid-values
  (testing "--debug option accepts valid boolean strings"
    (let [result-true (tools-cli/parse-opts ["--debug" "true"] cli-options)
          result-false (tools-cli/parse-opts ["--debug" "false"] cli-options)]
      (is (nil? (:errors result-true)) "No errors for 'true'")
      (is (true? (get-in result-true [:options :debug])) "Parsed as boolean true")
      (is (nil? (:errors result-false)) "No errors for 'false'")
      (is (false? (get-in result-false [:options :debug])) "Parsed as boolean false"))))


(deftest test-debug-option-invalid-values
  (testing "--debug option rejects invalid values"
    (let [test-cases ["maybe" "yes" "no" "1" "0" "TRUE" "FALSE" "" "xyz"]]
      (doseq [invalid-value test-cases]
        (let [result (tools-cli/parse-opts ["--debug" invalid-value] cli-options)]
          (is (seq (:errors result))
              (str "Should reject invalid value: " (pr-str invalid-value)))
          (is (some #(re-find #"Must be 'true' or 'false'" %) (:errors result))
              (str "Error message for " (pr-str invalid-value))))))))


;; Example hook formatters for testing
;; See also: https://tomscii.sig7.se/2018/08/Custom-transactions-in-banks2ledger

(defn round
  [^Double x]
  (double (Math/round x)))


(defn simple-salary-hook-formatter
  [entry]
  (let [amount (:amount entry)
        currency (:currency entry)
        year (subs (:date entry) 0 4)
        verifs [{:comment "Pay stub data"}
                {:account  (format "Tax:%s:GrossIncome" year)
                 :amount   "-00,000.00"
                 :currency currency}
                {:account  (format "Tax:%s:IncomeTax" year)
                 :amount   "0,000.00"
                 :currency currency}
                {:account (format "Tax:%s:NetIncome" year)}
                {:comment "Distribution of net income"}
                {:account  (:account entry)
                 :amount   amount
                 :currency currency}
                {:account  "Income:Salary"
                 :amount   (str "-" amount)
                 :currency currency}]]
    (print-ledger-entry! (conj entry [:verifs verifs]))))


(defn advanced-salary-hook-formatter
  [entry]
  (let [gross-salary 38500.0
        spp-contrib (round (* 0.05 gross-salary))
        recv-amount (-> (:amount entry)
                        (string/replace "," "")
                        parse-double)
        net-salary (+ recv-amount spp-contrib)
        income-tax (- gross-salary net-salary)
        currency (:currency entry)
        year (subs (:date entry) 0 4)
        verifs [{:comment "Pay stub data"}
                {:account  (format "Tax:%s:GrossIncome" year)
                 :amount   (format-value (- gross-salary))
                 :currency currency}
                {:account  (format "Tax:%s:IncomeTax" year)
                 :amount   (format-value income-tax)
                 :currency currency}
                {:account (format "Tax:%s:NetIncome" year)}
                {:comment "Distribution of net income"}
                {:account  "Income:Salary"
                 :amount   (format-value (- net-salary))
                 :currency currency}
                {:account  "Equity:SPP:Collect"
                 :amount   (format-value spp-contrib)
                 :currency currency}
                {:account  (:account entry)
                 :amount   (:amount entry)
                 :currency currency}]]
    (print-ledger-entry! (conj entry [:verifs verifs]))))


;; Verify the hook formatters defined above

(deftest test-print-ledger-entry-via-hook-formatters
  (testing "print-ledger-entry-via-hook-formatters"
    (is (= (with-out-str
             (simple-salary-hook-formatter
               {:date     "2017/07/25"
                :descr    "LÖN"
                :ref      "TXN123456789"
                :account  "Assets:Bank:Account"
                :amount   "29,290.00"
                :currency "SEK"}))
           (str "2017/07/25 (TXN123456789) LÖN" NL
                "    ; Pay stub data" NL
                "    Tax:2017:GrossIncome                  SEK -00,000.00" NL
                "    Tax:2017:IncomeTax                    SEK 0,000.00" NL
                "    Tax:2017:NetIncome" NL
                "    ; Distribution of net income" NL
                "    Assets:Bank:Account                   SEK 29,290.00" NL
                "    Income:Salary                         SEK -29,290.00" NL NL)))
    (is (= (with-out-str
             (advanced-salary-hook-formatter
               {:date     "2018/07/25"
                :descr    "LÖN"
                :ref      "TXN123456789"
                :account  "Assets:Bank:Account"
                :amount   "27,365.00"
                :currency "SEK"}))
           (str "2018/07/25 (TXN123456789) LÖN" NL
                "    ; Pay stub data" NL
                "    Tax:2018:GrossIncome                  SEK -38,500.00" NL
                "    Tax:2018:IncomeTax                    SEK 9,210.00" NL
                "    Tax:2018:NetIncome" NL
                "    ; Distribution of net income" NL
                "    Income:Salary                         SEK -29,290.00" NL
                "    Equity:SPP:Collect                    SEK 1,925.00" NL
                "    Assets:Bank:Account                   SEK 27,365.00" NL NL)))))
