(ns banks2ledger.ledger-parser
  "Ledger file parsing and output formatting."
  (:require
    [banks2ledger.bayesian :as bayesian]
    [banks2ledger.csv-parser :as csv]
    [clojure.string :as string]))


(defn comment-line?
  "Predicate for full comment lines in the ledger file"
  [line]
  (contains? #{\; \# \| \* \%}
             (first line)))


(defn split-ledger-entry
  "Split ledger entry string to a sequence of separate lines"
  [entry]
  (->> (string/split entry #"\r?\n")
       (remove comment-line?)
       (map (partial csv/clip-string ";"))
       (map string/trim)
       (filter seq)))


(defn parse-ledger-entry
  "Parse a ledger entry from string to acc-map"
  [entry-seq]
  (let [[first-line0 & rest-lines] entry-seq
        first-line (csv/clip-string "|" first-line0)            ; N.B: this is non-standard (not part of ledger-cli)
        [date descr] (string/split first-line #" " 2)
        accs       (map (partial csv/clip-string "  ") rest-lines)]
    {:accs accs
     :date date
     :toks (bayesian/tokenize descr)}))


(defn parse-ledger
  "Read and parse a ledger file; return acc-maps"
  [filename]
  (try
    (->> (string/split (slurp filename) #"\r?\n\r?\n")
         (map string/trim)                                    ; remove odd newlines
         (filter seq)
         (map split-ledger-entry)
         (filter #(> (count %) 1))
         (map parse-ledger-entry)
         (reduce bayesian/toktab-update {}))
    (catch java.io.FileNotFoundException e
      (throw (ex-info (str "Failed to read ledger file: " (.getMessage e))
                      {:type :file-not-found
                       :file filename}
                      e)))
    (catch java.io.IOException e
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
      (printf "    %-38s%s %s%n" account currency amount)

      :else
      (printf "    %s%n" account)))
  (println))


(defn add-default-verifications
  "generate verifications for the default case"
  [{:keys [account amount counter-acc currency] :as entry}]
  (conj entry
        [:verifs (case (first amount)
                   \- [{:account  counter-acc
                        :amount   (subs amount 1)
                        :currency currency}
                       {:account account}]
                   [{:account  account
                     :amount   amount
                     :currency currency}
                    {:account counter-acc}])]))
