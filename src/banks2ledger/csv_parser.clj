(ns banks2ledger.csv-parser
  "CSV parsing and data conversion utilities."
  (:require
    [clojure.data.csv :as csv]
    [clojure.string :as string])
  (:import
    (java.time LocalDate)
    (java.time.format DateTimeFormatter)
    (java.util Locale)))


(defn clip-string
  "Clip string to the part before the given endmark
   endmark is first arg to allow meaningful use of `partial'"
  [endmark s]
  (let [end-idx (string/index-of s endmark)]
    (cond-> s
      end-idx (subs 0 end-idx))))


(defn unquote-string
  "Remove quotes from start & end of the string, if both present"
  [s]
  (let [len (count s)]
    (cond
      (< len 3)
      s

      (or (and (= \' (first s)) (= \' (last s)))
          (and (= \" (first s)) (= \" (last s))))
      (subs s 1 (dec len))

      :else
      s)))


(defn all-indices-1
  [s sub pos acc]
  (if-let [idx (string/index-of s sub pos)]
    (all-indices-1 s sub (inc idx) (conj acc idx))
    acc))


(defn all-indices
  "Return an array of all indices where sub starts within str"
  [s sub]
  (all-indices-1 s sub 0 []))


(defn split-by-indices
  "Split string at the list of supplied indices and return a
   list of substrings. Note that the sublists do not contain the
   characters at the indices, only those in between."
  [str ixs]
  (let [op-ixs (concat (list -1) ixs (list (count str)))]
    (map (fn [[s e]]
           (subs str (inc s) e))
         (partition 2 1 op-ixs))))


(defn format-colspec
  "Render a colspec to an actual string based on cols;
   return whitespace-trimmed version."
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


(defn get-col
  "Get column data from cols according to colspec, which is a string
   similar to a printf format string but allowing alternatives to be
   used if an earlier spec results in an empty string.
   \"%4\" - get fourth column
   \"%4 %5\" - get fourth and fifth column separated by a space
   \"%4!%1 %2 %3!%7\" - fourth column by default, but if that is empty,
   (contains only whitespace) concatenate the first three columns;
   if that is empty, take the seventh column."
  [cols colspec]
  (let [delim-ixs (all-indices colspec "!")
        spec-list (split-by-indices colspec delim-ixs)]
    (get-col-1 cols spec-list)))


(defn remove-leading-garbage
  "Remove everything up to a number (an optional minus followed by a digit)"
  [s]
  (let [up-to-a-digit-re #".+?(?=-?\d)"]
    (string/replace-first (str " " s) up-to-a-digit-re "")))


(defn remove-trailing-garbage
  [s]
  (->> s
       (re-matches #"([-+]?[0-9]*\.?[0-9]+).*")
       last))


(defn format-value
  "Convert a double value to a canonically formatted amount"
  [value]
  (String/format Locale/US "%,.2f" (into-array Double [value])))


(defn convert-amount
  "Convert CSV amount string - note the return value is still a string!"
  [s {:keys [amount-decimal-separator amount-grouping-separator]}]
  (-> s
      remove-leading-garbage
      (string/replace (str amount-grouping-separator) "")
      (string/replace (str amount-decimal-separator) ".")
      remove-trailing-garbage
      parse-double
      format-value))


(let [ledger-entry-date-fmt (DateTimeFormatter/ofPattern "yyyy/MM/dd")]
  (defn convert-date
    "Convert date field from CSV format to Ledger entry format"
    [date-string {:keys [date-format]}]
    (-> date-string
        (LocalDate/parse
          (DateTimeFormatter/ofPattern date-format))
        (.format ledger-entry-date-fmt))))


(defn parse-csv-entry
  "Parse a line of CSV into a map with :amount :date :descr :ref"
  [{:keys [amount-col date-col descr-col ref-col] :as options} csv-cols]
  {:amount (convert-amount (nth csv-cols amount-col) options)
   :date   (convert-date (nth csv-cols date-col) options)
   :descr  (unquote-string (get-col csv-cols descr-col))
   :ref    (when (nat-int? ref-col)
             (unquote-string (nth csv-cols ref-col)))})


(defn drop-lines
  "Drop the configured number of header and trailer lines"
  [csv-lines {:keys [csv-skip-header-lines
                     csv-skip-trailer-lines]}]
  (->> csv-lines
       (drop csv-skip-header-lines)
       (drop-last csv-skip-trailer-lines)))


(defn parse-csv
  "Parse input CSV into a list of maps"
  [reader {:keys [csv-field-separator] :as options}]
  (map (partial parse-csv-entry options)
       (-> reader
           (csv/read-csv :separator csv-field-separator)
           (drop-lines options))))
