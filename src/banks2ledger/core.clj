#!/usr/bin/env bb
(ns banks2ledger.core
  (:gen-class)
  (:require
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]
    [clojure.string :as string])
  (:import
    (java.text
      SimpleDateFormat)
    (java.util
      Locale)))


;; Set to true to include debug information in the generated output.
;; To set this at runtime, add '-dbg true' to the argument list.
(def +debug+ (atom false))


;; Bump account's token counter for token
(defn toktab-inc
  [toktab [account token]]
  (let [acctab0 (or (get toktab account) {})
        cnt     (or (get acctab0 token) 0)
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
    (or (get acc-table token) 0)))


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
    (when @+debug+
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


;; command line args spec
(def cl-args-spec
  (array-map
    :ledger-file
    {:opt  "-l" :value "ledger.dat"
     :help "Ledger file to get accounts and probabilities"}

    :csv-file
    {:opt  "-f" :value "transactions.csv"
     :help "Input transactions in CSV format"}

    :csv-file-encoding
    {:opt  "-e" :value "UTF-8"
     :help "Encoding of the CSV file"}

    :account
    {:opt  "-a" :value "Assets:Checking"
     :help "Originating account of transactions"}

    :csv-field-separator
    {:opt "-F" :value "," :help "CSV field separator"}

    :csv-skip-header-lines
    {:opt  "-sa" :value 0 :conv-fun #(Integer. %)
     :help "CSV header lines to skip"}

    :csv-skip-trailer-lines
    {:opt  "-sz" :value 0 :conv-fun #(Integer. %)
     :help "CSV trailer lines to skip"}

    :currency
    {:opt "-c" :value "SEK" :help "Currency"}

    :date-format
    {:opt "-D" :value "yyyy-MM-dd" :help "Format of date field in CSV file"}

    :date-col
    {:opt  "-d" :value 0 :conv-fun #(Integer. %)
     :help "Date column index (zero-based)"}

    :ref-col
    {:opt  "-r" :value -1 :conv-fun #(Integer. %)
     :help "Payment reference column index (zero-based)"}

    :amount-col
    {:opt  "-m" :value 2 :conv-fun #(Integer. %)
     :help "Amount column index (zero-based)"}

    :descr-col
    {:opt  "-t" :value "%3"
     :help "Text (descriptor) column index specs (zero-based)"}

    :amount-decimal-separator
    {:opt  "-ds" :value "." :conv-fun first
     :help "Decimal sign character"}

    :amount-grouping-separator
    {:opt  "-gs" :value "," :conv-fun first
     :help "Decimal group (thousands) separator character"}

    :hooks-file
    {:opt  "-hf" :value nil
     :help "Hooks file defining customized output entries"}

    :debug
    {:opt  "-dbg" :value false
     :help "Include debug information in the generated output"}))


(defn print-usage-and-die
  [message]
  (println message)
  (println)
  (println "Usage: banks2ledger [options]")
  (println "  available options (syntax to set: -x value)")
  (doseq [{:keys [opt value help]} (vals cl-args-spec)]
    (println (format "%4s" opt) ":" help)
    (println "       default:" value))
  (System/exit 0))


(defn optvec
  [args-spec]
  (into [] (map :opt (vals args-spec))))


;; Get the value of an argument given by key.
;; If present, apply converter function.
(defn get-arg
  [args-spec key]
  (let [arg-spec  (key args-spec)
        conv-fun  (:conv-fun arg-spec)
        raw-value (:value arg-spec)]
    (cond-> raw-value
      (some? conv-fun) conv-fun)))


(defn find-first
  "Finds the first item in a collection that matches a predicate."
  [pred coll]
  (reduce (fn [_ x]
            (when (pred x)
              (reduced x)))
          nil
          coll))


(defn set-arg
  [args-spec arg value]
  (let [key  (find-first #(= arg (:opt (% args-spec)))
                         (keys args-spec))
        val0 (key args-spec)
        val  (conj val0 [:value value])]
    (conj args-spec [key val])))


(declare parse-args)


(defn parse-arg
  [args-spec arg rest-args]
  (if (not-any? #{arg} (optvec cl-args-spec))
    (print-usage-and-die (str "Invalid argument: " arg))
    (if-let [value (first rest-args)]
      (parse-args (set-arg args-spec arg value)
                  (rest rest-args))
      (print-usage-and-die (str "Value expected for option " arg)))))


;; Go through the args list and return an updated args-spec
(defn parse-args
  [args-spec args]
  (cond-> args-spec
    (seq args) (parse-arg (first args) (rest args))))


;; Convert date field from CSV format to Ledger entry format
(defn convert-date
  [args-spec datestr]
  (.format
    (SimpleDateFormat. "yyyy/MM/dd")
    (.parse (SimpleDateFormat. (get-arg args-spec :date-format))
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
  [args-spec s]
  (-> s
      remove-leading-garbage
      (string/replace (str (get-arg args-spec :amount-grouping-separator)) "")
      (string/replace (str (get-arg args-spec :amount-decimal-separator)) ".")
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
  (let [ref-col (get-arg params :ref-col)]
    {:date   (convert-date params (nth cols (get-arg params :date-col)))
     :ref    (when (nat-int? ref-col)
               (unquote-string (nth cols ref-col)))
     :amount (convert-amount params (nth cols (get-arg params :amount-col)))
     :descr  (unquote-string (get-col cols (get-arg params :descr-col)))}))


;; Drop the configured number of header and trailer lines
(defn drop-lines
  [lines params]
  (->> lines
       (drop (get-arg params :csv-skip-header-lines))
       (drop-last (get-arg params :csv-skip-trailer-lines))))


;; Parse input CSV into a list of maps
(defn parse-csv
  [reader params]
  (->> params
       (drop-lines (csv/read-csv reader
                                 :separator
                                 (first (get-arg params :csv-field-separator))))
       (map (partial parse-csv-entry params))))


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
(def +ledger-entry-hooks+ (atom nil))


(defn add-entry-hook
  [hook]
  (swap! +ledger-entry-hooks+ #(conj % hook)))


(defn process-hooks!
  [entry]
  (loop [hooks @+ledger-entry-hooks+]
    (let [{:keys [formatter predicate] :as hook} (first hooks)]
      (if (nil? hook)
        (print-ledger-entry! (add-default-verifications entry))
        (if (predicate entry)
          (when formatter
            (formatter entry))
          (recur (rest hooks)))))))


;; generate a ledger entry -- invoke user-defined hooks
(defn generate-ledger-entry!
  [params acc-maps
   {:keys [date ref amount descr]}]
  (let [account     (get-arg params :account)
        counter-acc (decide-account acc-maps descr account)
        currency    (get-arg params :currency)
        entry       {:date    date :ref ref :amount amount :currency currency
                     :account account :counter-acc counter-acc :descr descr}]
    (process-hooks! entry)))


;; Convert CSV of bank account transactions to corresponding ledger entries
(defn -main
  [& args]
  (let [params   (parse-args cl-args-spec args)
        acc-maps (parse-ledger (get-arg params :ledger-file))]
    (reset! +debug+ (get-arg params :debug))
    (when @+debug+
      (with-open [acc-maps-dump-file (io/writer "acc_maps_dump.txt")]
        (binding [*out* acc-maps-dump-file]
          (print-acc-maps acc-maps))))
    (some-> (get-arg params :hooks-file)
            load-file)
    (with-open [reader (io/reader (get-arg params :csv-file)
                                  :encoding (get-arg params :csv-file-encoding))]
      (run! (partial generate-ledger-entry! params acc-maps)
            (parse-csv reader params))
      (flush))))


(when (= *file* (System/getProperty "babashka.file"))
  (-main))
