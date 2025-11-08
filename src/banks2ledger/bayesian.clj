(ns banks2ledger.bayesian
  "Bayesian inference for account classification based on transaction descriptors."
  (:require [clojure.string :as string]))


(defn toktab-inc
  "Bump account's token counter for token"
  [toktab [account token]]
  (let [acctab0 (get toktab account {})
        cnt     (get acctab0 token 0)
        acctab  (conj acctab0 [token (inc cnt)])]
    (conj toktab [account acctab])))


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
   - split at '/' ',' and space"
  [s]
  (filter seq
          (-> (reduce-kv string/replace
                         s
                         {#"20\d{6}"            "YYYYMMDD"
                          #"/\d{2}-\d{2}-\d{2}" "/YY-MM-DD"})
              string/upper-case
              (string/split #",|/| "))))


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
      0.0
      (let [n-occ (n-occur acc-maps token account)]
        (/ (float n-occ) n-occ-all)))))


(defn bayes*
  "Combine probability values according to the Bayes theorem"
  [probs]
  (let [prod-probs (apply * probs)
        prod-comps (apply * (map #(- 1.0 %) probs))]
    (/ prod-probs (+ prod-probs prod-comps))))


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
          (printf ";     %40s %f%n" (second p) (first p))))
      (printf "; Combined probability table:%n")
      (doseq [e p-tab]
        (printf ";     %40s %f%n" (second e) (first e))))
    (remove #(string/includes? (second %) account)
            p-tab)))


(defn decide-account
  "Decide the most likely counter-account for a transaction based on its descriptor.
   Returns the account name or 'Unknown' if no clear match is found."
  [acc-maps descr account {:keys [debug]}]
  (let [accs (account-for-descr acc-maps descr account debug)]
    (cond
      (empty? accs)
      "Unknown"

      (= (ffirst accs) (first (second accs)))
      "Unknown"

      :else
      (second (first accs)))))
