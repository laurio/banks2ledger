(ns banks2ledger.sandbox
  "Sandboxed evaluation of hooks files using SCI."
  (:require
    [banks2ledger.bayesian :as bayesian]
    [banks2ledger.csv-parser :as csv-parser]
    [banks2ledger.hooks :as hooks]
    [banks2ledger.ledger-parser :as ledger]
    [clojure.string]
    [sci.core :as sci])
  (:import
    (java.io
      FileNotFoundException)))


(defn- make-sci-context
  "Create a SCI context exposing only the project functions needed by hooks."
  []
  (sci/init
    {:namespaces
     {'banks2ledger.hooks          {'add-entry-hook hooks/add-entry-hook}
      'banks2ledger.bayesian       {'tokenize bayesian/tokenize}
      'banks2ledger.ledger-parser  {'print-ledger-entry! ledger/print-ledger-entry!
                                    'add-default-verifications ledger/add-default-verifications}
      'banks2ledger.csv-parser     {'format-value csv-parser/format-value}
      'clojure.string              (sci/copy-ns clojure.string (sci/create-ns 'clojure.string))}}))


(defn- format-sci-error
  "Format a SCI exception into a user-friendly error message."
  [hooks-file ^Exception e]
  (let [data (ex-data e)
        line (:line data)
        col  (:column data)
        loc  (when line
               (str " at line " line (when col (str ", column " col))))
        detail (.getMessage e)]
    (case (:type data)
      :sci.error/parse
      (str "Syntax error in hooks file '" hooks-file "'" loc ": " detail)

      :sci/error
      (str "Error in hooks file '" hooks-file "'" loc ": " detail)

      ;; Fallback for unexpected exception types
      (str "Error loading hooks file '" hooks-file "': " detail))))


(defn load-hooks-file
  "Load and evaluate a hooks file in a sandboxed SCI context.
   Only explicitly exposed project functions are available to the hooks code."
  [hooks-file]
  (try
    (let [code (slurp hooks-file)
          ctx  (make-sci-context)]
      (sci/eval-string* ctx code))
    (catch FileNotFoundException e
      (throw (ex-info (str "Failed to load hooks file: " (.getMessage e))
                      {:type :hooks-file-not-found
                       :file hooks-file}
                      e)))
    (catch Exception e
      (throw (ex-info (format-sci-error hooks-file e)
                      {:type :hooks-load-error
                       :file hooks-file}
                      e)))))
