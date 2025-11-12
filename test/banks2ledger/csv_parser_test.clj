(ns banks2ledger.csv-parser-test
  "Tests for CSV parsing and data conversion."
  (:require
    [banks2ledger.csv-parser :refer [all-indices clip-string convert-amount
                                     convert-date format-colspec format-value
                                     get-col parse-csv-entry split-by-indices
                                     unquote-string]]
    [clojure.test :refer [deftest is testing]])
  (:import
    (clojure.lang
      ExceptionInfo)
    (java.util
      Locale)))


(deftest test-clip-string
  (testing "clip-string"
    (is (= (clip-string "|" "abcdefg") "abcdefg"))
    (is (= (clip-string "|" "abcdef|ghij") "abcdef"))
    (is (= (clip-string "  " " abcd  efg") " abcd"))))


(deftest test-convert-date
  (testing "convert-date"
    (is (= (convert-date "2016-04-24" {:date-format "yyyy-MM-dd"})
           "2016/04/24"))
    (is (= (convert-date "04/24/16" {:date-format "MM/dd/yy"})
           "2016/04/24"))
    (is (= (convert-date "2016/04/24" {:date-format "yyyy/MM/dd"})
           "2016/04/24"))))


(deftest test-format-value
  (testing "format-value"
    (is (= (format-value 0.0) "0.00"))
    (is (= (format-value -10.237) "-10.24"))
    (is (= (format-value 1234.56) "1,234.56"))
    (is (= (format-value 1234567.89) "1,234,567.89"))))


(deftest test-format-value-with-nonUS-locale
  (let [default-locale (Locale/getDefault)]
    (Locale/setDefault (Locale. "sv" "SE"))
    (testing "format-value-with-nonUS-locale"
      (is (= (format-value 0.0) "0.00"))
      (is (= (format-value -10.237) "-10.24"))
      (is (= (format-value 1234.56) "1,234.56"))
      (is (= (format-value 1234567.89) "1,234,567.89")))
    (Locale/setDefault default-locale)))


(deftest test-convert-amount
  (testing "convert-amount"
    (is (= (convert-amount "12500,19"
                           {:amount-decimal-separator  \,
                            :amount-grouping-separator \space})
           "12,500.19")
        "A number without grouping separators")
    (is (= (convert-amount "1 125 000,00"
                           {:amount-decimal-separator  \,
                            :amount-grouping-separator \space})
           "1,125,000.00")
        "A number with space as grouping separator")
    (is (= (convert-amount "Lots of chars here, and the number -> 100200.12 <- the number"
                           {:amount-decimal-separator  \.
                            :amount-grouping-separator \,})
           "100,200.12")
        "A number surrounded by text")
    (is (= (convert-amount "egy azaz 1 krumpli"
                           {:amount-decimal-separator  \.
                            :amount-grouping-separator \,})
           "1.00")
        "A number with a prefix and a suffix")
    (is (= (convert-amount "--12"
                           {:amount-decimal-separator  \.
                            :amount-grouping-separator \,})
           "-12.00")
        "A negative number with 2 minus signs")
    (is (= (convert-amount "+12.34"
                           {:amount-decimal-separator  \.
                            :amount-grouping-separator \,})
           "12.34")
        "A positive number with an explicit plus sign")
    (is (= (convert-amount "-123.45 kr"
                           {:amount-decimal-separator  \.
                            :amount-grouping-separator \,})
           "-123.45")
        "A negative number with a currency suffix")
    (is (= (convert-amount "-110.003,45 kr"
                           {:amount-decimal-separator  \,
                            :amount-grouping-separator \.})
           "-110,003.45")
        "A negative number with a currency suffix and custom separators")
    (is (= (convert-amount "8,01"
                           {:amount-decimal-separator  \,
                            :amount-grouping-separator \.})
           "8.01")
        "A number with a custom decimal separator")
    (is (= (convert-amount "876.543,21"
                           {:amount-decimal-separator  \,
                            :amount-grouping-separator \.})
           "876,543.21")
        "A large number with a custom decimal separator")
    (is (= (convert-amount "5 125 000,01"
                           {:amount-decimal-separator  \,
                            :amount-grouping-separator \space})
           "5,125,000.01")
        "A number with spaces as group separator")
    (is (= (convert-amount "-7 000,00"
                           {:amount-decimal-separator  \,
                            :amount-grouping-separator \space})
           "-7,000.00")
        "Another number with spaces as group separator")
    (is (= (convert-amount "9123_4567.89"
                           {:amount-decimal-separator  \.
                            :amount-grouping-separator \_})
           "91,234,567.89")
        "A number with custom spaced underscores as group separator")
    (is (= (convert-amount "usd 10,123.45"
                           {:amount-decimal-separator  \.
                            :amount-grouping-separator \,})
           "10,123.45")
        "A number with a currency prefix and standard separators")
    (is (= (convert-amount "-1234,5671.28"
                           {:amount-decimal-separator  \.
                            :amount-grouping-separator \,})
           "-12,345,671.28")
        "A number with custom spacing for groups")))


(deftest test-convert-amount-error-cases
  (testing "convert-amount handles nil and invalid inputs"
    (is (thrown-with-msg? NumberFormatException #"Amount column value is nil"
          (convert-amount nil {:amount-decimal-separator \.
                               :amount-grouping-separator \,}))
        "Nil input should throw with clear message")
    (is (thrown-with-msg? NumberFormatException #"No valid number found"
          (convert-amount "" {:amount-decimal-separator \.
                              :amount-grouping-separator \,}))
        "Empty string should throw")
    (is (thrown-with-msg? NumberFormatException #"No valid number found"
          (convert-amount "abc xyz" {:amount-decimal-separator \.
                                     :amount-grouping-separator \,}))
        "Text without numbers should throw")
    (is (thrown-with-msg? NumberFormatException #"No valid number found"
          (convert-amount "   " {:amount-decimal-separator \.
                                 :amount-grouping-separator \,}))
        "Whitespace-only should throw")))


(deftest test-unquote-string
  (testing "unquote-string"
    (is (= (unquote-string "abcdef") "abcdef"))
    (is (= (unquote-string "\"abcdef\"") "abcdef"))
    (is (= (unquote-string "\"abcdef") "\"abcdef"))
    (is (= (unquote-string "abcdef\"") "abcdef\""))
    (is (= (unquote-string "'abcdef'") "abcdef"))
    (is (= (unquote-string "'abcdef") "'abcdef"))
    (is (= (unquote-string "abcdef'") "abcdef'"))))


(deftest test-all-indices
  (testing "all-indices"
    (is (= (all-indices "abcdef" ",")
           []))
    (is (= (all-indices ",abc,def," ",")
           [0 4 8]))
    (is (= (all-indices "abc,de,\"f,g,x\",hi,\"al,ma\"" ",")
           [3 6 9 11 14 17 21]))))


(deftest test-split-by-indices
  (testing "split-by-indices"
    (is (= (split-by-indices "abc:def:gh:ij" [3 7 10])
           ["abc" "def" "gh" "ij"]))))


(deftest test-format-colspec
  (testing "format-colspec"
    (is (= (format-colspec ["1st" "2nd" "3rd"] "%0") "1st"))
    (is (= (format-colspec ["1st" "2nd" "3rd"] "%0 %2") "1st 3rd"))
    (is (= (format-colspec ["\"1st\"" "\"2nd\"" "\"3rd\""] "%0 %2") "1st 3rd"))
    (is (= (format-colspec ["1st" "2nd" "3rd"] "%1-%0-%2") "2nd-1st-3rd"))
    (is (= (format-colspec ["1st" "2nd" "   "] "%0 %2") "1st"))
    (is (= (format-colspec ["1st" "2nd" "   "] "%0 %2 %1") "1st     2nd"))
    (is (= (format-colspec (vec (map str (range 1 12))) "%10") "11"))))


(deftest test-get-col
  (testing "get-col"
    (is (= (get-col ["1st" "2nd" "3rd"] "%0") "1st"))
    (is (= (get-col ["1st" "2nd" "3rd"] "%0!%1!%2") "1st"))
    (is (= (get-col ["\"1st\"" "\"2nd\""] "%0 %1") "1st 2nd"))
    (is (= (get-col ["   " "2nd" "3rd"] "%0!%1!%2") "2nd"))
    (is (= (get-col ["   " "2nd" "3rd"] "%0 %2!%1") "3rd"))
    (is (= (get-col ["   " "2nd" "3rd"] "%0!%1%0%2!%2") "2nd   3rd"))
    (is (= (get-col ["   " "2nd" "3rd"] "%0!%1%0!%2") "2nd"))
    (is (= (get-col (vec (map str (range 1 12))) "%10") "11"))))


(deftest test-parse-csv-entry-column-out-of-bounds
  (testing "parse-csv-entry handles column out of bounds"
    (let [options {:date-col                  0
                   :amount-col                2
                   :descr-col                 "%1"
                   :ref-col                   -1
                   :date-format               "yyyy-MM-dd"
                   :amount-decimal-separator  \.
                   :amount-grouping-separator \,}
          csv-cols ["2024-01-01" "Description"]]            ; Missing column 2
      (try
        (parse-csv-entry 5 options csv-cols)
        (is false "Should have thrown exception")
        (catch ExceptionInfo e
          (let [msg (ex-message e)
                data (ex-data e)]
            (is (re-find #"CSV row 5" msg) "Error message should include row number")
            (is (re-find #"Column index out of bounds" msg) "Error message should describe the problem")
            (is (re-find #"Row has 2 column" msg) "Error message should show actual column count")
            (is (= :column-out-of-bounds (:type data)) "Exception data should have correct type")
            (is (= 5 (:row data)) "Exception data should include row number")
            (is (= 2 (:column-count data)) "Exception data should include column count")))))))


(deftest test-parse-csv-entry-date-parse-error
  (testing "parse-csv-entry handles date parsing errors"
    (let [options {:date-col                  0
                   :amount-col                2
                   :descr-col                 "%1"
                   :ref-col                   -1
                   :date-format               "yyyy-MM-dd"
                   :amount-decimal-separator  \.
                   :amount-grouping-separator \,}
          csv-cols ["2024/01/01" "Description" "100.00"]]   ; Wrong date format
      (try
        (parse-csv-entry 10 options csv-cols)
        (is false "Should have thrown exception")
        (catch ExceptionInfo e
          (let [msg (ex-message e)
                data (ex-data e)]
            (is (re-find #"CSV row 10" msg) "Error message should include row number")
            (is (re-find #"Failed to parse date" msg) "Error message should describe the problem")
            (is (re-find #"2024/01/01" msg) "Error message should show the problematic value")
            (is (re-find #"yyyy-MM-dd" msg) "Error message should show expected format")
            (is (= :date-parse-error (:type data)) "Exception data should have correct type")
            (is (= 10 (:row data)) "Exception data should include row number")
            (is (= "2024/01/01" (:date-value data)) "Exception data should include date value")))))))


(deftest test-parse-csv-entry-amount-parse-error
  (testing "parse-csv-entry handles amount parsing errors"
    (let [options {:date-col                  0
                   :amount-col                2
                   :descr-col                 "%1"
                   :ref-col                   -1
                   :date-format               "yyyy-MM-dd"
                   :amount-decimal-separator  \.
                   :amount-grouping-separator \,}
          csv-cols ["2024-01-01" "Description" "not-a-number"]]
      (try
        (parse-csv-entry 15 options csv-cols)
        (is false "Should have thrown exception")
        (catch ExceptionInfo e
          (let [msg (ex-message e)
                data (ex-data e)]
            (is (re-find #"CSV row 15" msg) "Error message should include row number")
            (is (re-find #"Failed to parse amount" msg) "Error message should describe the problem")
            (is (re-find #"not-a-number" msg) "Error message should show the problematic value")
            (is (= :amount-parse-error (:type data)) "Exception data should have correct type")
            (is (= 15 (:row data)) "Exception data should include row number")
            (is (= "not-a-number" (:amount-value data)) "Exception data should include amount value")))))))


(deftest test-parse-csv-entry-success
  (testing "parse-csv-entry succeeds with valid input"
    (let [options {:date-col                  0
                   :amount-col                2
                   :descr-col                 "%1"
                   :ref-col                   3
                   :date-format               "yyyy-MM-dd"
                   :amount-decimal-separator  \.
                   :amount-grouping-separator \,}
          csv-cols ["2024-01-01" "Test Transaction" "1,234.56" "REF123"]
          result (parse-csv-entry 1 options csv-cols)]
      (is (= "2024/01/01" (:date result)) "Date should be converted correctly")
      (is (= "1,234.56" (:amount result)) "Amount should be parsed correctly")
      (is (= "Test Transaction" (:descr result)) "Description should be extracted")
      (is (= "REF123" (:ref result)) "Reference should be extracted"))))
