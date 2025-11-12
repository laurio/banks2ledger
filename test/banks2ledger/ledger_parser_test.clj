(ns banks2ledger.ledger-parser-test
  "Tests for ledger file parsing and output formatting."
  (:require
    [banks2ledger.ledger-parser :refer [parse-ledger-entry print-ledger-entry!
                                        split-ledger-entry]]
    [clojure.test :refer [deftest is testing]]))


;; String containing the platform-dependent newline character, to verify
;; that all tested output is produced with this newline encoding.
(def NL (with-out-str (println)))


(deftest test-split-ledger-entry
  (testing "split-ledger-entry"
    (is (= (split-ledger-entry
             (str "2016/03/22 ICA NARA KAR/16-03-21\n"
                  "    Expenses:Groceries:ICA                SEK 314.32\n"
                  "    Assets:Bank account\n\n"))
           ["2016/03/22 ICA NARA KAR/16-03-21"
            "Expenses:Groceries:ICA                SEK 314.32"
            "Assets:Bank account"]))
    ;; same input, but with CRLF line endings:
    (is (= (split-ledger-entry
             (str "2016/03/22 ICA NARA KAR/16-03-21\r\n"
                  "    Expenses:Groceries:ICA                SEK 314.32\r\n"
                  "    Assets:Bank account\r\n\r\n"))
           ["2016/03/22 ICA NARA KAR/16-03-21"
            "Expenses:Groceries:ICA                SEK 314.32"
            "Assets:Bank account"]))
    (is (= (split-ledger-entry
             (str "2016/02/16 Lindra Second Hand, Kärrtorp | Baby stuff\n"
                  "    Expenses:Clothing:Baby                 SEK 60.00\n"
                  "    Assets:Bank account\n"))
           ["2016/02/16 Lindra Second Hand, Kärrtorp | Baby stuff"
            "Expenses:Clothing:Baby                 SEK 60.00"
            "Assets:Bank account"]))
    (is (= (split-ledger-entry
             (str "; this is a global comment that is not applied to a specific transaction\n"
                  "; it can start with any of the five characters ; # | * %\n"
                  "    ; it is also valid in any column, not just at the start of the line\n"
                  "# according to ledger-cli documentation,\n"
                  "| the following characters are also\n"
                  "* valid comment characters, if used at the\n"
                  "% beginning of the line: # | * %\n"
                  "2018/01/22 (1234567890) CLAS OHLSON /18-01-19 | Verktyg & material\n"
                  "    ; NYCKELBRICKA 6-PA              19.90\n"
                  "    ; PINCETTSATS 4-PAC              59.90\n"
                  "    ; SKYDDGLASÖGON KL              179.00\n"
                  "    ; BLOCKNYCKEL 24MM               69.90\n"
                  "    ; ELTEJP 20MM SVART              29.90\n"
                  "    Expenses:Supplies                     SEK 358.60\n"
                  "    Assets:Bank account\n"))
           ["2018/01/22 (1234567890) CLAS OHLSON /18-01-19 | Verktyg & material"
            "Expenses:Supplies                     SEK 358.60"
            "Assets:Bank account"]))
    (is (= (split-ledger-entry
             (str "; this is a global comment that is not applied to a specific transaction\n"
                  "; it can start with any of the five characters ; # | * %\n"
                  "    ; it is also valid in any column, not just at the start of the line\n"
                  "# according to ledger-cli documentation,\n"
                  "| the following characters are also\n"
                  "* valid comment characters, if used at the\n"
                  "% beginning of the line: # | * %\n"
                  "\t; There\n"
                  "  \t; Are          \n"
                  "    ; Only Comments         \n"
                  "    ; And Whitespace in this block!\n"))
           []))))


(deftest test-parse-ledger-entry
  (testing "parse-ledger-entry"
    (is (= (parse-ledger-entry
             ["2016/03/22 ICA NARA KAR/16-03-21"
              "Expenses:Groceries:ICA                SEK 314.32"
              "Assets:Bank account"])
           {:date "2016/03/22"
            :toks ["ICA" "NARA" "KAR" "YY-MM-DD"]
            :accs ["Expenses:Groceries:ICA" "Assets:Bank account"]}))
    (is (= (parse-ledger-entry
             ["2016/02/16 Lindra Second Hand, Kärrtorp | Baby stuff"
              "Expenses:Clothing:Baby                 SEK 60.00"
              "Assets:Bank account"])
           {:date "2016/02/16"
            :toks ["LINDRA" "SECOND" "HAND" "KÄRRTORP"]
            :accs ["Expenses:Clothing:Baby" "Assets:Bank account"]}))
    (is (= (parse-ledger-entry
             ["2018/01/22 (1234567890) CLAS OHLSON /18-01-19 | Verktyg & material"
              "Expenses:Supplies                     SEK 358.60"
              "Assets:Bank account"])
           {:date "2018/01/22"
            :toks ["(1234567890)" "CLAS" "OHLSON" "YY-MM-DD"]
            :accs ["Expenses:Supplies" "Assets:Bank account"]}))))


(deftest test-print-ledger-entry
  (testing "print-ledger-entry"
    (is (= (with-out-str
             (print-ledger-entry!
               {:date   "2018-07-21"
                :descr  "Custom Shop Extra"
                :verifs [{:comment "This is a comment"}
                         {:account  "Expenses:Random"
                          :amount   "123.45"
                          :currency "SEK"}
                         {:account "Assets:Pocket"}]}))
           (str "2018-07-21 Custom Shop Extra" NL
                "    ; This is a comment" NL
                "    Expenses:Random                       SEK 123.45" NL
                "    Assets:Pocket" NL NL)))
    (is (= (with-out-str
             (print-ledger-entry!
               {:date   "2018-07-21"
                :ref    "1234567890"
                :descr  "Custom Shop Extra"
                :verifs [{:comment "This is a comment"}
                         {:account  "Expenses:Random"
                          :amount   "123.45"
                          :currency "SEK"}
                         {:account "Assets:Pocket"}]}))
           (str "2018-07-21 (1234567890) Custom Shop Extra" NL
                "    ; This is a comment" NL
                "    Expenses:Random                       SEK 123.45" NL
                "    Assets:Pocket" NL NL)))))
