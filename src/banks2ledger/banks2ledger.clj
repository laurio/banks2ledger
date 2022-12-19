#!/usr/bin/env bb
(ns banks2ledger.banks2ledger
  (:gen-class)
  (:require
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]
    [clojure.string :as string]
    [clojure.tools.cli :as tools-cli])
  (:import
    (java.text
      SimpleDateFormat)
    (java.util
      Locale)))


;; Set to true to include debug information in the generated output.
;; To set this at runtime, add '--debug true' to the argument list.
(def debug! (atom false))


;; Bump account's token counter for token
(defn toktab-inc
  [toktab [account token]]
  (let [acctab0 (get toktab account {})
        cnt     (get acctab0 token 0)
        acctab  (conj acctab0 [token (inc cnt)])]
    (conj toktab [account acctab])))


;; Update toktab by bumping all accounts in entry for all tokens
(defn toktab-update
  [toktab {accs :accs toks :toks}]
  (reduce toktab-inc toktab (for [acc accs
                                  tok toks]
                              [acc tok])))


;; Create tokens from string
;; One string may become one or more tokens, returned as a seq
;; - replace dates with degraded forms
;; - Convert to uppercase
;; - Split at '/' ',' and space
(defn tokenize
  [s]
  (filter seq
          (-> (reduce-kv string/replace
                         s
                         {#"20\d{6}"            "YYYYMMDD"
                          #"/\d{2}-\d{2}-\d{2}" "/YY-MM-DD"})
              string/upper-case
              (string/split #",|/| "))))


;; N_occur is the occurrence count of token among all tokens
;; recorded for account.
;; acc-maps is the output of parse-ledger.
(defn n_occur
  [acc-maps token account]
  (let [acc-table (get acc-maps account)]
    (get acc-table token 0)))


;; P_belong is the probability that a transaction with
;; token in its descriptor belongs to account.
;; acc-maps is the output of parse-ledger.
(defn p_belong
  [acc-maps token account]
  (let [n_occ     (n_occur acc-maps token account)
        n_occ_all (apply + (map (fn [acc] (n_occur acc-maps token acc))
                                (keys acc-maps)))]
    (if (= 0 n_occ_all)
      0.0
      (/ (float n_occ) n_occ_all))))


;; Combine probability values according to the Bayes theorem
(defn bayes*
  [probs]
  (let [prod-probs (apply * probs)
        prod-comps (apply * (map #(- 1.0 %) probs))]
    (/ prod-probs (+ prod-probs prod-comps))))


;; Combined p_belong of given tokens for account
(defn p_belong*
  [acc-maps tokens account]
  (bayes* (map (fn [tok] (p_belong acc-maps tok account))
               tokens)))


;; Return a list of [p_belong, account] pairs in descending order
;; only accounts with nonzero probs are returned
(defn best-accounts
  [acc-maps token]
  (->> (keys acc-maps)
       (map (fn [acc] [(p_belong acc-maps token acc) acc]))
       (filter #(> (first %) 0.0))
       (sort-by first >)))


;; Print a table of combined probs for given tokens
(defn p_table
  [acc-maps tokens]
  (let [nz-toks (filter #(pos? (count (best-accounts acc-maps %)))
                        tokens)]
    (->> (keys acc-maps)
         (map (fn [acc] [(p_belong* acc-maps nz-toks acc) acc]))
         (filter #(> (first %) 0.0))
         (sort-by first >))))


;; Return the most probable counter-accounts for given descr captured for
;; account. This account will be excluded from possible counter-accounts.
(defn account-for-descr
  [acc-maps descr account]
  (let [tokens (tokenize descr)
        p_tab  (p_table acc-maps tokens)]
    (when @debug!
      (printf "; Deciding \"%s\" for %s%n", descr, account)
      (printf "; Tokens: ") (print tokens) (newline)
      (printf "; Account probabilities per token:%n")
      (doseq [tok tokens]
        (printf ";  '%s':%n" tok)
        (doseq [p (best-accounts acc-maps tok)]
          (printf ";     %40s %f%n" (second p) (first p))))
      (printf "; Combined probability table:%n")
      (doseq [e p_tab]
        (printf ";     %40s %f%n" (second e) (first e))))
    (remove #(string/includes? (second %) account)
            p_tab)))


(defn decide-account
  [acc-maps descr account]
  (let [accs (account-for-descr acc-maps descr account)]
    (cond
      (empty? accs) "Unknown"
      (= (ffirst accs) (first (second accs))) "Unknown"
      :else (second (first accs)))))


;; Science up to this point. From here, only machinery.

;; Clip string to the part before the given endmark
;; endmark is first arg to allow meaningful use of `partial'
(defn clip-string
  [endmark string]
  (let [end-idx (.indexOf string endmark)]
    (cond-> string
      (not= -1 end-idx) (subs 0 end-idx))))


;; Predicate for full comment lines in the ledger file
(defn comment-line?
  [line]
  (contains? #{\; \# \| \* \%}
             (first line)))


;; Split ledger entry string to a sequence of separate lines
(defn split-ledger-entry
  [entry]
  (->> (string/split entry #"\r?\n")
       (remove comment-line?)
       (map (partial clip-string ";"))
       (map string/trim)
       (filter seq)))


;; Parse a ledger entry from string to acc-map
(defn parse-ledger-entry
  [entry-seq]
  (let [[first-line0 & rest-lines] entry-seq
        first-line (clip-string "|" first-line0)            ; N.B: this is non-standard (not part of ledger-cli)
        [date descr] (string/split first-line #" " 2)
        toks       (tokenize descr)
        accs       (map (partial clip-string "  ") rest-lines)]
    {:date date :toks toks :accs accs}))


;; Read and parse a ledger file; return acc-maps
(defn parse-ledger
  [filename]
  (->> (string/split (slurp filename) #"\r?\n\r?\n")
       (map string/trim)                                    ; remove odd newlines
       (filter seq)
       (map split-ledger-entry)
       (filter #(> (count %) 1))
       (map parse-ledger-entry)
       (reduce toktab-update {})))


;; Produce a printout of the acc-maps passed as arg; useful for debugging
(defn print-acc-maps
  [acc-maps]
  (doseq [acc (sort (keys acc-maps))]
    (printf "'%s':%n" acc)
    (let [acc-map (get acc-maps acc)]
      (doseq [tok-count (sort-by second > acc-map)]
        (printf " %6d  '%s'%n" (second tok-count) (first tok-count))))
    (println)))


;; Convert date field from CSV format to Ledger entry format
(defn convert-date
  [{:keys [date-format]} datestr]
  (.format
    (SimpleDateFormat. "yyyy/MM/dd")
    (.parse (SimpleDateFormat. date-format)
            datestr)))


;; Remove everything up to a number (an optional minus followed by a digit)
(defn remove-leading-garbage
  [s]
  (let [up-to-a-digit-re #".+?(?=-?\d)"]
    (string/replace-first (str " " s) up-to-a-digit-re "")))


(defn remove-trailing-garbage
  [s]
  (->> s
       (re-matches #"([-+]?[0-9]*\.?[0-9]+).*")
       last))


;; Convert a double value to a canonically formatted amount
(defn format-value
  [value]
  (String/format Locale/US "%,.2f" (into-array Double [value])))


;; Convert CSV amount string - note the return value is still a string!
(defn convert-amount
  [options s]
  (-> s
      remove-leading-garbage
      (string/replace (str (:amount-grouping-separator options)) "")
      (string/replace (str (:amount-decimal-separator options)) ".")
      remove-trailing-garbage
      parse-double
      format-value))


;; Remove quotes from start & end of the string, if both present
(defn unquote-string
  [s]
  (let [len (count s)]
    (cond
      (< len 3) s

      (or (and (= \' (first s)) (= \' (last s)))
          (and (= \" (first s)) (= \" (last s))))
      (subs s 1 (dec len))

      :else s)))


(defn all-indices-1
  [str sub pos acc]
  (let [idx (.indexOf str sub pos)]
    (if (= -1 idx)
      acc
      (all-indices-1 str sub (inc idx) (conj acc idx)))))


;; Return an array of all indices where sub starts within str
(defn all-indices
  [str sub]
  (all-indices-1 str sub 0 []))


;; Split string at the list of supplied indices and return a
;; list of substrings. Note that the sublists do not contain the
;; characters at the indices, only those in between.
(defn split-by-indices
  [str ixs]
  (let [op-ixs (concat (list -1) ixs (list (count str)))]
    (map (fn [[s e]]
           (subs str (inc s) e))
         (partition 2 1 op-ixs))))


;; Render a colspec to an actual string based on cols;
;; return whitespace-trimmed version.
(defn format-colspec
  [cols colspec]
  (-> colspec
      (string/replace #"\%(\d*)"
                      #(unquote-string (nth cols (parse-long (second %)))))
      string/trim))


(defn get-col-1
  [cols [spec & spec-list]]
  (let [fmt (format-colspec cols spec)]
    (if (or (seq fmt)
            (empty? spec-list))
      fmt
      (get-col-1 cols spec-list))))


;; Get column data from cols according to colspec, which is a string
;; similar to a printf format string but allowing alternatives to be
;; used if an earlier spec results in an empty string.
;; "%4" - get fourth column
;; "%4 %5" - get fourth and fifth column separated by a space
;; "%4!%1 %2 %3!%7" - fourth column by default, but if that is empty,
;;   (contains only whitespace) concatenate the first three columns;
;;   if that is empty, take the seventh column.
(defn get-col
  [cols colspec]
  (let [delim-ixs (all-indices colspec "!")
        spec-list (split-by-indices colspec delim-ixs)]
    (get-col-1 cols spec-list)))


;; Parse a line of CSV into a map with :date :ref :amount :descr
(defn parse-csv-entry
  [params cols]
  (let [ref-col (:ref-col params)]
    {:date   (convert-date params (nth cols (:date-col params)))
     :ref    (when (nat-int? ref-col)
               (unquote-string (nth cols ref-col)))
     :amount (convert-amount params (nth cols (:amount-col params)))
     :descr  (unquote-string (get-col cols (:descr-col params)))}))


;; Drop the configured number of header and trailer lines
(defn drop-lines
  [lines options]
  (->> lines
       (drop (:csv-skip-header-lines options))
       (drop-last (:csv-skip-trailer-lines options))))


;; Parse input CSV into a list of maps
(defn parse-csv
  [reader {:keys [csv-field-separator] :as options}]
  (->> options
       (drop-lines (csv/read-csv reader
                                 :separator csv-field-separator))
       (map (partial parse-csv-entry options))))


;; print a ledger entry to *out*
;; An entry has a list of verifications (transaction entries),
;; each of them will produce a line of output. Each line is
;; either a comment or an entry containing an account and
;; optionally an amount and currency.
(defn print-ledger-entry!
  [entry]
  (let [ref    (:ref entry)
        verifs (:verifs entry)]
    (printf "%s " (:date entry))
    (when (seq ref)
      (printf "(%s) " ref))
    (println (:descr entry))
    (doseq [verif verifs]
      (let [comment (:comment verif)
            account (:account verif)
            amount  (:amount verif)]
        (if (some? comment)
          (printf "    ; %s%n" comment)
          (if (nil? amount)
            (printf "    %s%n" account)
            (printf "    %-38s%s %s%n" account (:currency verif) amount)))))
    (println)))


;; generate verifications for the default case
(defn add-default-verifications
  [entry]
  (let [amount      (:amount entry)
        currency    (:currency entry)
        account     (:account entry)
        counter-acc (:counter-acc entry)]
    (conj entry
          [:verifs (case (first amount)
                     \- [{:account  counter-acc
                          :amount   (subs amount 1)
                          :currency currency}
                         {:account account}]
                     [{:account  account
                       :amount   amount
                       :currency currency}
                      {:account counter-acc}])])))


;; hooks allow the user to generate custom output for certain entries
(def ledger-entry-hooks! (atom nil))


(defn add-entry-hook
  [hook]
  (swap! ledger-entry-hooks! #(conj % hook)))


(defn process-hooks!
  [entry]
  (loop [hooks @ledger-entry-hooks!]
    (let [{:keys [formatter predicate] :as hook} (first hooks)]
      (if (nil? hook)
        (print-ledger-entry! (add-default-verifications entry))
        (if (predicate entry)
          (when formatter
            (formatter entry))
          (recur (rest hooks)))))))


;; generate a ledger entry -- invoke user-defined hooks
(defn generate-ledger-entry!
  [{:keys [account currency]} acc-maps
   {:keys [date ref amount descr]}]
  (let [counter-acc (decide-account acc-maps descr account)
        entry       {:date    date :ref ref :amount amount :currency currency
                     :account account :counter-acc counter-acc :descr descr}]
    (process-hooks! entry)))


(defn filepath-exists?
  [filepath]
  (.exists (io/file filepath)))


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


;; Convert CSV of bank account transactions to corresponding ledger entries
(defn -main
  [& args]
  (let [{:keys [options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (let [acc-maps (parse-ledger (:ledger-file options))]
        (reset! debug! (:debug options))
        (when @debug!
          (with-open [acc-maps-dump-file (io/writer "acc_maps_dump.txt")]
            (binding [*out* acc-maps-dump-file]
              (print-acc-maps acc-maps))))
        (some-> (:hooks-file options)
                load-file)
        (with-open [reader (io/reader (:csv-file options)
                                      :encoding (:csv-file-encoding options))]
          (run! (partial generate-ledger-entry! options acc-maps)
                (parse-csv reader options))
          (flush))))))


(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
