(import jdn)
(import argparse)
(import path)
(import jdn-loader)
(import jdn::statement-formats :jdn-loader/binding-type :struct)

(def- ext-peg
  (peg/compile ~{:back (> -1 (+ (* ($) (set "\\/.")) :back))
                 :main :back}))

(defn split-filename
  [path]
  (if-let [m (peg/match ext-peg path (length path))]
    (let [i (m 0)]
      (if (= (path i) 46)
        (string/split "." path i 2)))))

(def argparse-params
  ["teller CLI"
   "config" {:kind :option
             :short "c"
             :help "The path to the config file. "}
   "input" {:kind :option
            :short "i"
            :help "Required. The path to the input file. Must be a pdf."
            :required true}
   "output" {:kind :option
             :short "o"
             :help "Optional. The path to the output file. Must be a tsv. If not provided but statement-dir is defined in config, output to a file of the same input name there. Otherwise, prints to stdout."}
   "password" {:kind :option
               :short "p"
               :help "Optional. If pdf is password protected, unlocks the pdf for this session. Essential if you want to pipe stdout to a file."}
   "stdout" {:kind :flag
             :short "s"
             :help "If flagged, prints to stdout even if output is defined."}
   "statement-format" {:kind :option
                       :short "f"
                       :help "Optional. If provided, will override the corresponding field in config."}
   "csv-delimiter" {:kind :option
                    :short "d"
                    :help "The character used as a delimiter for the output csv."}])

(defn run-cmd [args]
  (let [out-temp-file (file/temp)
        err-temp-file (file/temp)
        exit-code (os/execute args :p {:err err-temp-file :out out-temp-file})]
    (file/seek out-temp-file :set 0)
    (file/seek err-temp-file :set 0)
    {:exit-code exit-code
     :err (file/read err-temp-file :all)
     :out (file/read out-temp-file :all)}))

(def base-grammar
  '{:mm (choice (sequence "0" (range "19"))
                (sequence "1" (range "02")))
    :yy (sequence :d :d)
    :yyyy (sequence :d :d :d :d)
    :dd (choice (sequence "0" :d)
                (sequence (range "12") :d)
                (sequence "3" (range "01")))
    :/ (set "/-")
    :yyyy-mm-dd (sequence :yyyy :/ :mm :/ :dd)
    :dd-mm-yyyy (sequence :dd :/ :mm :/ :yyyy)
    :mm-dd-yyyy (sequence :mm :/ :dd :/ :yyyy)
    :mm-dd-yy (sequence :mm :/ :dd :/ :yy)
    :yy-mm-dd (sequence :yy :/ :mm :/ :dd)
    :financial-figure (sequence
                        (? "-")
                        (at-most 3 :d)
                        (any (sequence
                               ","
                               (repeat 3 :d)))
                        "."
                        (some :d))
    :non-newline-whitespace (set " \t\0\f\v")
    :simple-phrase (sequence (some :S)
                             (any (sequence :non-newline-whitespace
                                     (some :S))))})

(defn with-base-grammar
  [grammar-config]
  (-> grammar-config
      (struct/to-table)
      (update :entries struct/to-table)
      (update :filename struct/to-table)
      (update-in [:entries :grammar]
                 (partial merge base-grammar))
      (update-in [:filename :grammar]
                 (partial merge base-grammar))))

(defn read-pdf [path password]
  (let [{:exit-code exit-code
         :err err
         :out out} (run-cmd ["pdftotext" "-nopgbrk" "-layout" "-upw" password path "-"])
        password-buffer @""]
    (cond
      (= exit-code 0) out
      (and (not= exit-code 0)
           (string/find "Command Line Error: Incorrect password" err)) (do (print "prompt for password")
                                                                         (getline "Password: " password-buffer)
                                                                         (read-pdf path (string/trimr (string/slice password-buffer) "\n")))
      :else (do (print "lol some other error")))))

(defn pad-array
  "Pad array up to n with padding. Defaults to nil."
  [arr n &opt padding]
  (default padding nil)
  (let [diff (- n (length arr))]
    (repeat diff
      (array/push arr padding))
    arr))

(defn render-row
  [delimiter tuple-row]
  (string/join tuple-row delimiter))

(defn data->tsv [delimiter parsed-soa]
  (string/join
   (map (partial render-row delimiter)
         parsed-soa)
   "\n"))

(defn wrap-quotes [s]
  (string "\"" s "\""))

(defn parse-entries
  "Normalize \\xE2\\x80\\x90 into '-' before running peg/match."
  [statement-facts entries-format text]
  (let [normalized-pdf-text (string/replace-all "\xE2\x80\x90" "-" text)
        {:grammar entries-grammar
         :group-keys entries-group-keys} entries-format
        parsed-soa (peg/match entries-grammar normalized-pdf-text)
        entry-keys (get entries-group-keys :type/entry)]
    (map (fn [tuple-row]
           (let [[row-type & row-rest] tuple-row
                 entry (zipcoll entry-keys row-rest)]
             (map (fn [key]
                    (let [cell (get entry key)]
                      (if (array? cell)
                        # Convention is that the first entry of the group
                        # is the type declaration
                        (let [[cell-type & rest] cell
                              # The keys of group-formats needs to be keyed
                              # with all the possible values of cell-type
                              cell-keys (get entries-group-keys cell-type)]
                          (case cell-type
                            :type/date (let [structured-date (zipcoll cell-keys rest)
                                             {:day day
                                              :month month
                                              :year year} structured-date]
                                         (if (nil? year)
                                           (let [statement-date (get-in statement-facts [:filename :teller/statement-date])
                                                 {:year billing-year
                                                  :month billing-month} statement-date
                                                 parsed-billing-year (scan-number billing-year)
                                                 parsed-billing-month (scan-number billing-month)
                                                 parsed-entry-month (scan-number month)
                                                 # If we know that a bill points to January, then any December entry
                                                 # should have a year decremented.
                                                 entry-year (if (and (= parsed-billing-month 1)
                                                                     (= parsed-entry-month 12))
                                                              (dec parsed-billing-year)
                                                              parsed-billing-year)]
                                             (wrap-quotes (string entry-year "-" month "-" day)))
                                           (wrap-quotes (string year "-" month "-" day))))
                            (string "\"" (string/join rest "-") "\"")))
                        (string "\"" cell "\""))))
                  entry-keys)))
         parsed-soa)))

(defn parse-filename
  [filename-format text]
  (let [{:grammar filename-grammar
         :group-keys filename-group-keys} filename-format
        parsed-filename (peg/match filename-grammar text)]
    (zipcoll
     (get filename-group-keys :main)
     (map (fn [data]
            (if (not (array? data))
             data
             (let [[cell-type & cell-rest] data
                   cell-keys (get filename-group-keys cell-type)]
               (assert cell-keys
                       (error (string "Could not find cell-keys for cell-type: " cell-type ". Ensure that a group-key exists for any group prefixed with a type.")))
               (zipcoll cell-keys cell-rest))))
             parsed-filename))))

(defn main [& args]
  (with-dyns [:args args]
    (let [res (argparse/argparse (splice argparse-params))]
      (unless res
        (os/exit 1))
      (let [home-path (os/getenv "HOME")
            input-path (get res "input")
            stdout? (get res "stdout")
            password (or (get res "password")
                         (os/getenv "TELLER_PDF_PASSWORD")
                         "")
            config-path (or (get res "config")
                            (os/getenv "TELLER_CONFIG_PATH")
                            (string home-path "/.config/teller/config.jdn"))
            config (try (jdn/decode (slurp config-path))
                     ([err fiber]
                       {}))
            statement-format (or (keyword (get res "statement-format"))
                                 (get config :statement-format)
                                 (os/getenv "TELLER_STATEMENT_FORMAT")
                                 :bdo)
            statement-dir (or (get res "statement-dir")
                              (get config :statement-dir)
                              (os/getenv "TELLER_STATEMENT_DIR"))
            delimiter-character (or (get res "csv-delimiter")
                                    (get config :csv-delimiter)
                                    (os/getenv "TELLER_CSV_DELIMITER")
                                    ",")
            input-filename (path/basename input-path)
            output-path (get res "output")
            output-filepath (if (nil? output-path)
                              (if statement-dir
                                (let [input-ext (path/ext input-filename)
                                      no-ext (if (string/has-suffix? input-ext input-filename)
                                               (first (split-filename input-filename))
                                               input-filename)]
                                  (string statement-dir "/" (string/replace " " "_" no-ext) ".csv"))
                                (let [files-in-statement-dir (os/dir statement-dir)]
                                  # TODO: autoincrement
                                  # Filter all files whose prefix is out and suffix is .tsv.
                                  # Split each file such that given some form `out-n.tsv`, n is separated and parsed as an int
                                  # Get the highest int, then increment.
                                  (string statement-dir "/" "out.tsv")))
                              (if (= :directory (get (os/stat output-path) :mode))
                                (path/join output-path (string input-filename ".csv"))
                                output-path))
            statement-format (-> jdn::statement-formats/jdns
                                 (get statement-format)
                                 (with-base-grammar))
            filename-format (get statement-format :filename)
            entries-format (get statement-format :entries)
            parsed-filename (parse-filename filename-format input-filename)
            statement-facts {:filename parsed-filename}
            pdf-text (read-pdf input-path password)
            parsed-soa (parse-entries statement-facts entries-format pdf-text)
            output-text (data->tsv delimiter-character parsed-soa)
           ]
        (if (or stdout? (not output-path))
          (print output-text)
          (spit output-filepath output-text))))))
