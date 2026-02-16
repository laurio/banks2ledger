(ns banks2ledger.hooks
  "Hooks system for customizing ledger entry generation."
  (:require
    [banks2ledger.ledger-parser :as ledger]))


;; hooks allow the user to generate custom output for certain entries
(def ledger-entry-hooks (atom nil))


(defn add-entry-hook
  "Add a hook to customize ledger entry generation.
   A hook is a map with :predicate and :formatter keys.
   The predicate determines if the hook applies to an entry.
   The formatter generates the output for matching entries."
  [hook]
  (swap! ledger-entry-hooks #(conj % hook)))


(defn process-hooks!
  "Process an entry through the registered hooks.
   If a hook's predicate matches, use its formatter.
   Otherwise, use the default formatter."
  [entry]
  (loop [hooks @ledger-entry-hooks]
    (let [{:keys [formatter predicate] :as hook} (first hooks)]
      (cond
        (nil? hook)
        (-> entry
            ledger/add-default-verifications
            ledger/print-ledger-entry!)

        (predicate entry)
        (when formatter
          (formatter entry))

        :else
        (recur (rest hooks))))))
