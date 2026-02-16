(ns banks2ledger.ledger-parser
  "Ledger file parsing and output formatting."
  (:require
    [banks2ledger.bayesian :as bayesian]
    [banks2ledger.csv-parser :as csv]
    [clojure.string :as string])
  (:import
    (java.io
      FileNotFoundException
      IOException)))


;; Constants for ledger parsing and formatting
(def ^:private comment-line-markers
  "Set of characters that mark a full comment line in ledger format"
  #{\; \# \| \* \%})


(def ^:private newline-pattern
  "Regex pattern for newlines (handles both LF and CRLF)"
  #"\r?\n")


(def ^:private entry-separator-pattern
  "Regex pattern for entry separator (double newline)"
  #"\r?\n\r?\n")


(def ^:private inline-comment-marker
  "Character used for inline comments in ledger entries"
  ";")


(def ^:private pipe-separator
  "Non-standard pipe separator used to truncate descriptions"
  "|")


(def ^:private space-split-pattern
  "Pattern to split on first space"
  #" ")


(def ^:private space-split-limit
  "Limit for space splitting (date from description)"
  2)


(def ^:private double-space-separator
  "Double space separator used to truncate account lines"
  "  ")


(def ^:private min-entry-line-count
  "Minimum number of lines for a valid ledger entry"
  1)


(def ^:private account-column-width
  "Column width for account names in formatted output"
  38)


(def ^:private amount-substring-start
  "Start index for extracting amount (skipping minus sign)"
  1)


(defn comment-line?
  "Predicate for full comment lines in the ledger file"
  [line]
  (contains? comment-line-markers
             (first line)))


(defn split-ledger-entry
  "Split ledger entry string to a sequence of separate lines"
  [entry]
  (->> (string/split entry newline-pattern)
       (remove comment-line?)
       (map (partial csv/clip-string inline-comment-marker))
       (map string/trim)
       (filter seq)))


(defn parse-ledger-entry
  "Parse a ledger entry from string to acc-map"
  [entry-seq]
  (let [[first-line0 & rest-lines] entry-seq
        first-line (csv/clip-string pipe-separator first-line0) ; N.B: this is non-standard (not part of ledger-cli)
        [date descr] (string/split first-line space-split-pattern space-split-limit)
        accs (map (partial csv/clip-string double-space-separator) rest-lines)]
    {:accs accs
     :date date
     :toks (bayesian/tokenize descr)}))


(defn parse-ledger
  "Read and parse a ledger file; return acc-maps"
  [filename]
  (try
    (->> (string/split (slurp filename) entry-separator-pattern)
         (map string/trim)                                  ; remove odd newlines
         (filter seq)
         (map split-ledger-entry)
         (filter #(> (count %) min-entry-line-count))
         (map parse-ledger-entry)
         (reduce bayesian/toktab-update {}))
    (catch FileNotFoundException e
      (throw (ex-info (str "Failed to read ledger file: " (.getMessage e))
                      {:type :file-not-found
                       :file filename}
                      e)))
    (catch IOException e
      (throw (ex-info (str "Error reading ledger file '" filename "': " (.getMessage e))
                      {:type :io-error
                       :file filename}
                      e)))))


(defn print-ledger-entry!
  "print a ledger entry to *out*
   An entry has a list of verifications (transaction entries),
   each of them will produce a line of output. Each line is
   either a comment or an entry containing an account and
   optionally an amount and currency."
  [{:keys [date descr ref verifs]}]
  (printf "%s " date)
  (when (seq ref)
    (printf "(%s) " ref))
  (println descr)
  (doseq [{:keys [account amount comment currency]} verifs]
    (cond
      comment
      (printf "    ; %s%n" comment)

      amount
      (printf (str "    %-" account-column-width "s%s %s%n") account currency amount)

      :else
      (printf "    %s%n" account)))
  (println))


(defn add-default-verifications
  "generate verifications for the default case"
  [{:keys [account amount counter-acc currency] :as entry}]
  (assoc entry :verifs (case (first amount)
                         \- [{:account  counter-acc
                              :amount   (subs amount amount-substring-start)
                              :currency currency}
                             {:account account}]
                         [{:account  account
                           :amount   amount
                           :currency currency}
                          {:account counter-acc}])))
