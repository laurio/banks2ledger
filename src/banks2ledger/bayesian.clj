(ns banks2ledger.bayesian
  "Bayesian inference for account classification based on transaction descriptors."
  (:require
    [clojure.string :as string]))


;; Constants for tokenization
(def ^:private date-yyyymmdd-pattern
  "Regex pattern for dates in YYYYMMDD format (e.g., 19990101, 20060923)"
  #"(?:19|20|21)\d{2}(?:0[1-9]|1[0-2])(?:0[1-9]|[12]\d|3[01])")


(def ^:private date-yyyymmdd-token
  "Token replacement for YYYYMMDD-format dates"
  "YYYYMMDD")


(def ^:private date-short-pattern
  "Regex pattern for short dates like /YY-MM-DD (e.g., /16-03-21)"
  #"/\d{2}-\d{2}-\d{2}")


(def ^:private date-short-token
  "Token replacement for short-format dates"
  "/YY-MM-DD")


(def ^:private token-split-pattern
  "Regex pattern to split tokens by comma, slash, or space"
  #",|/| ")


(def ^:private default-probability
  "Default probability when no occurrences are found"
  0.0)


(def ^:private unity
  "Unity constant for probability calculations"
  1.0)


(def ^:private debug-format-width
  "Column width for account names in debug output"
  40)


(def ^:private unknown-account
  "Account name used when no match is found"
  "Unknown")


(defn toktab-inc
  "Bump account's token counter for token"
  [toktab [account token]]
  (let [acctab0 (get toktab account {})
        cnt     (get acctab0 token 0)
        acctab  (assoc acctab0 token (inc cnt))]
    (assoc toktab account acctab)))


(defn toktab-update
  "Update toktab by bumping all accounts in entry for all tokens"
  [toktab {accs :accs toks :toks}]
  (reduce toktab-inc toktab (for [acc accs
                                  tok toks]
                              [acc tok])))


(defn tokenize
  "Create tokens from string
   One string may become one or more tokens, returned as a seq
   - replace dates with degraded forms
   - convert to uppercase
   - split at '/' ',' and space
   Returns empty seq if input is nil or blank."
  [s]
  (if (string/blank? s)
    []
    (filter seq
            (-> (reduce (fn [s [pat repl]] (string/replace s pat repl))
                        s
                        [[date-yyyymmdd-pattern date-yyyymmdd-token]
                         [date-short-pattern    date-short-token]])
                string/upper-case
                (string/split token-split-pattern)))))


(defn n-occur
  "N_occur is the occurrence count of token among all tokens
   recorded for account.
   acc-maps is the output of parse-ledger."
  [acc-maps token account]
  (let [acc-table (get acc-maps account)]
    (get acc-table token 0)))


(defn p-belong
  "P_belong is the probability that a transaction with
   token in its descriptor belongs to account.
   acc-maps is the output of parse-ledger."
  [acc-maps token account]
  (let [n-occ-all (->> (keys acc-maps)
                       (map (fn [acc] (n-occur acc-maps token acc)))
                       (apply +))]
    (if (zero? n-occ-all)
      default-probability
      (let [n-occ (n-occur acc-maps token account)]
        (/ (float n-occ) n-occ-all)))))


(defn bayes*
  "Combine probability values according to the Bayes theorem.
   Returns default-probability for empty input or zero denominator."
  [probs]
  (if (empty? probs)
    default-probability
    (let [prod-probs (apply * probs)
          prod-comps (apply * (map #(- unity %) probs))
          denom      (+ prod-probs prod-comps)]
      (if (zero? denom)
        default-probability
        (/ prod-probs denom)))))


(defn p-belong*
  "Combined p_belong of given tokens for account"
  [acc-maps tokens account]
  (bayes* (map (fn [tok] (p-belong acc-maps tok account))
               tokens)))


(defn best-accounts
  "Return a list of [p_belong, account] pairs in descending order
   only accounts with nonzero probabilities are returned"
  [acc-maps token]
  (->> (keys acc-maps)
       (map (fn [account] [(p-belong acc-maps token account) account]))
       (filter (comp pos? first))
       (sort-by first >)))


(defn p-table
  "Print a table of combined probs for given tokens"
  [acc-maps tokens]
  (let [nz-toks (filter #(->> %
                              (best-accounts acc-maps)
                              count
                              pos?)
                        tokens)]
    (->> (keys acc-maps)
         (map (fn [account] [(p-belong* acc-maps nz-toks account) account]))
         (filter (comp pos? first))
         (sort-by first >))))


(defn account-for-descr
  "Return the most probable counter-accounts for given descr captured for
   account. This account will be excluded from possible counter-accounts."
  [acc-maps descr account debug?]
  (let [tokens (tokenize descr)
        p-tab  (p-table acc-maps tokens)]
    (when debug?
      (printf "; Deciding \"%s\" for %s%n" descr account)
      (printf "; Tokens: ") (print tokens) (newline)
      (printf "; Account probabilities per token:%n")
      (doseq [tok tokens]
        (printf ";  '%s':%n" tok)
        (doseq [p (best-accounts acc-maps tok)]
          (printf (str ";     %-" debug-format-width "s %f%n") (second p) (first p))))
      (printf "; Combined probability table:%n")
      (doseq [e p-tab]
        (printf (str ";     %-" debug-format-width "s %f%n") (second e) (first e))))
    (remove #(string/includes? (second %) account)
            p-tab)))


(defn decide-account
  "Decide the most likely counter-account for a transaction based on its descriptor.
   Returns the account name or 'Unknown' if no clear match is found."
  [acc-maps descr account {:keys [debug]}]
  (let [accs (account-for-descr acc-maps descr account debug)]
    (cond
      (empty? accs)
      unknown-account

      ;; If the top two accounts have equal probability, the match is ambiguous
      (= (ffirst accs) (first (second accs)))
      unknown-account

      :else
      (second (first accs)))))
