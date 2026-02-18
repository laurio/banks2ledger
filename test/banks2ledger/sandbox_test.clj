(ns banks2ledger.sandbox-test
  "Tests for sandboxed hooks evaluation."
  (:require
    [banks2ledger.hooks :as hooks]
    [banks2ledger.sandbox :as sandbox]
    [clojure.test :refer [deftest is testing use-fixtures]]))


(defn reset-hooks-fixture
  "Reset the hooks atom before each test."
  [f]
  (reset! hooks/ledger-entry-hooks [])
  (f)
  (reset! hooks/ledger-entry-hooks []))


(use-fixtures :each reset-hooks-fixture)


(deftest test-load-hooks-file-registers-hooks
  (testing "Loading a hooks file registers hooks via the sandbox"
    (sandbox/load-hooks-file "test/data/hooks.clj")
    (is (= 3 (count @hooks/ledger-entry-hooks))
        "hooks.clj should register 3 hooks (salary, fkassa, ignore)")))


(deftest test-sandbox-blocks-system-exit
  (testing "System/exit is not accessible in the sandbox"
    (is (thrown? Exception
          (sandbox/load-hooks-file "test/data/malicious_exit.clj")))))


(deftest test-sandbox-blocks-file-access
  (testing "slurp (file I/O) is not accessible in the sandbox"
    (is (thrown? Exception
          (sandbox/load-hooks-file "test/data/malicious_slurp.clj")))))


(deftest test-sandbox-blocks-shell-access
  (testing "Shell commands are not accessible in the sandbox"
    (is (thrown? Exception
          (sandbox/load-hooks-file "test/data/malicious_shell.clj")))))


(deftest test-load-hooks-file-not-found
  (testing "Loading a non-existent hooks file throws ex-info"
    (let [ex (try
               (sandbox/load-hooks-file "nonexistent-hooks.clj")
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "Should throw ExceptionInfo")
      (is (= :hooks-file-not-found (:type (ex-data ex)))))))


(deftest test-syntax-error-message
  (testing "Syntax errors produce a clear message with location"
    (let [ex (try
               (sandbox/load-hooks-file "test/data/malicious_syntax.clj")
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "Should throw ExceptionInfo")
      (is (= :hooks-load-error (:type (ex-data ex))))
      (is (re-find #"Syntax error in hooks file" (.getMessage ex)))
      (is (re-find #"at line \d+" (.getMessage ex))))))


(deftest test-unresolved-symbol-error-message
  (testing "Unresolved symbol errors produce a clear message with location"
    (let [ex (try
               (sandbox/load-hooks-file "test/data/malicious_unresolved.clj")
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "Should throw ExceptionInfo")
      (is (= :hooks-load-error (:type (ex-data ex))))
      (is (re-find #"Error in hooks file" (.getMessage ex)))
      (is (re-find #"at line \d+" (.getMessage ex)))
      (is (re-find #"no-such-function" (.getMessage ex))))))
