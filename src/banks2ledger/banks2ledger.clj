#!/usr/bin/env bb
(ns banks2ledger.banks2ledger
  "Main entry point for banks2ledger - converts bank CSV files to ledger format."
  (:gen-class)
  (:require
    [banks2ledger.bayesian :as bayesian]
    [banks2ledger.csv-parser :as csv-parser]
    [banks2ledger.hooks :as hooks]
    [banks2ledger.ledger-parser :as ledger]
    [clojure.java.io :as io]
    [clojure.string :as string]
    [clojure.tools.cli :as tools-cli]))


(defn filepath-exists?
  [filepath]
  (.exists (io/file filepath)))


(defn- debug-print-acc-maps
  "Produce a printout of the acc-maps passed as arg; useful for debugging"
  [acc-maps]
  (with-open [acc-maps-dump-file (io/writer "acc_maps_dump.txt")]
    (binding [*out* acc-maps-dump-file]
      (doseq [acc (sort (keys acc-maps))]
        (printf "'%s':%n" acc)
        (let [acc-map (get acc-maps acc)]
          (doseq [tok-count (sort-by second > acc-map)]
            (printf " %6d  '%s'%n" (second tok-count) (first tok-count))))
        (println)))))


(defn generate-ledger-entry!
  "generate a ledger entry -- invoke user-defined hooks"
  [{:keys [account currency] :as options}
   acc-maps
   {:keys [date ref amount descr]}]
  (let [counter-acc (bayesian/decide-account acc-maps descr account options)
        entry       {:account     account
                     :amount      amount
                     :counter-acc counter-acc
                     :currency    currency
                     :date        date
                     :descr       descr
                     :ref         ref}]
    (hooks/process-hooks! entry)))


;; command line args spec
(def cli-options
  [["-l" "--ledger-file LEDGER-FILE" "Ledger file to get accounts and probabilities"
    :default "ledger.dat"
    :validate [filepath-exists? "The specified ledger file doesn't exist"]]
   ["-f" "--csv-file CSV-FILE" "Input transactions in CSV format"
    :default "transactions.csv"
    :validate [filepath-exists? "The specified transactions csv file doesn't exist"]]
   ["-e" "--csv-file-encoding ENCODING" "Encoding of the CSV file"
    :default "UTF-8"
    :validate [(complement string/blank?) "Invalid CSV file encoding"]]
   ["-a" "--account ACCOUNT" "Originating account of transactions"
    :default "Assets:Checking"
    :validate [(complement string/blank?) "Invalid originating account of transactions"]]
   ["-j" "--csv-field-separator SEPARATOR" "CSV field separator"
    :default \,
    :parse-fn first
    :validate [some? "Invalid CSV field separator"]]
   ["-b" "--csv-skip-header-lines INTEGER" "CSV header lines to skip"
    :default 0
    :parse-fn parse-long
    :validate [nat-int? "Invalid value specified for CSV header lines to skip"]]
   ["-z" "--csv-skip-trailer-lines INTEGER" "CSV trailer lines to skip"
    :default 0
    :parse-fn parse-long
    :validate [nat-int? "Invalid value specified for CSV trailer lines to skip"]]
   ["-c" "--currency CURRENCY" "Currency"
    :default "SEK"
    :validate [(complement string/blank?) "Invalid currency"]]
   ["-D" "--date-format FORMAT" "Format of date field in CSV file"
    :default "yyyy-MM-dd"
    :validate [(complement string/blank?) "Invalid format of date field in CSV file"]]
   ["-d" "--date-col INTEGER" "Date column index (zero-based)"
    :default 0
    :parse-fn parse-long
    :validate [nat-int? "Invalid date column index"]]
   ["-r" "--ref-col INTEGER" "Payment reference column index (zero-based)"
    :default -1
    :parse-fn parse-long
    :validate [int? "Must be integer"]]
   ["-m" "--amount-col INTEGER" "Amount column index (zero-based)"
    :default 2
    :parse-fn parse-long
    :validate [nat-int? "Must be >= 0"]]
   ["-t" "--descr-col INDEX-SPECS" "Text (descriptor) column index specs (zero-based)"
    :default "%3"
    :validate [(complement string/blank?) "Must be specified"]]
   ["-x" "--amount-decimal-separator SEPARATOR" "Decimal sign character"
    :default "."
    :parse-fn first
    :validate [some? "Must be specified"]]
   ["-y" "--amount-grouping-separator SEPARATOR" "Decimal group (thousands) separator character"
    :default ","
    :parse-fn first
    :validate [some? "Must be specified"]]
   ["-k" "--hooks-file FILE" "Hooks file defining customized output entries"
    :default nil
    :validate [filepath-exists? "The specified hooks hooks file doesn't exist"]]
   ["-g" "--debug DEBUG" "Include debug information in the generated output"
    :default false
    :parse-fn #{"true" "false"}]
   ["-h" "--help"]])


(defn usage
  [options-summary]
  (->> ["A tool to convert bank account CSV files to ledger."
        "Guesses account name via simple Bayesian inference based on your existing ledger file."
        ""
        "Usage: banks2ledger [options]"
        ""
        "Options:"
        options-summary
        ""
        "Please refer to the home page for more information."]
       (string/join \newline)))


(defn error-msg
  [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))


(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
   should exit (with an error message, and optional ok status), or a map
   indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options errors summary]} (tools-cli/parse-opts args cli-options)]
    (cond
      (:help options)                                       ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}

      errors                                                ; errors => exit with description of errors
      {:exit-message (error-msg errors)}

      (not (filepath-exists? (:ledger-file options)))
      {:exit-message (error-msg [(str "Ledger file '" (:ledger-file options) "' not found")])}

      :else
      {:options options})))


(defn exit
  [status msg]
  (println msg)
  (System/exit status))


(defn -main
  "Convert CSV of bank account transactions to corresponding ledger entries"
  [& args]
  (let [{:keys [options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (let [acc-maps (ledger/parse-ledger (:ledger-file options))]
        (when (:debug options)
          (debug-print-acc-maps acc-maps))
        (some-> (:hooks-file options)
                load-file)
        (with-open [reader (io/reader (:csv-file options)
                                      :encoding (:csv-file-encoding options))]
          (run! (partial generate-ledger-entry! options acc-maps)
                (csv-parser/parse-csv reader options))
          (flush))))))


(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
