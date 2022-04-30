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
                       :help "Optional. If provided, will override the corresponding field in config."}])

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
    :simple-phrase (sequence (some :S) (any (sequence :non-newline-whitespace (some :S))))})

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
  [tuple-row]
  (string/join tuple-row "\t"))

(defn data->tsv [parsed-soa]
  (string/join
    (map render-row
         parsed-soa)
    "\n"))

(defn parse-soa
  "Normalize \\xE2\\x80\\x90 into '-' before running peg/match."
  [statement-grammar text]
  (let [normalized-pdf-text (string/replace-all "\xE2\x80\x90" "-" text)
        {:pattern statement-pattern
         :entry-keys entry-keys} statement-grammar
        parsed-soa (peg/match statement-pattern normalized-pdf-text)]
    (map (fn [tuple-row]
           (let [entry (zipcoll entry-keys tuple-row)
                 {:date1 date1
                  :date2 date2
                  :desc1 desc1
                  :desc2 desc2
                  :amount amount} entry
                 rearranged-row [date1 date2 desc1 desc2 amount]]
             (map (fn [cell] (string "\"" cell "\""))
                  rearranged-row)))
         parsed-soa)))

(defn main [& args]
  (with-dyns [:args args]
    (let [res (argparse/argparse (splice argparse-params))]
      (unless res
        (os/exit 1))
      (let [home-path (os/getenv "HOME")
            input-path (get res "input")
            stdout? (get res "stdout")
            password (or (get res "password") (os/getenv "TELLER_PDF_PASSWORD") "")
            config-path (or (get res "config") (os/getenv "TELLER_CONFIG_PATH") (string home-path "/.config/teller/config.jdn"))
            config (try (jdn/decode (slurp config-path))
                     ([err fiber]
                       {}))
            statement-format (or (keyword (get res "statement-format")) (get config :statement-format) (os/getenv "TELLER_STATEMENT_FORMAT") :bdo)
            statement-dir (or (get res "statement-dir") (get config :statement-dir) (os/getenv "TELLER_STATEMENT_DIR"))
            output-path (get res "output"
                             (if statement-dir
                               (let [input-filename (path/basename input-path)
                                     input-ext (path/ext input-filename)
                                     no-ext (if (string/has-suffix? input-ext input-filename)
                                              (first (split-filename input-filename))
                                              input-filename)]
                                 (string statement-dir "/" (string/replace " " "_" no-ext) ".tsv"))
                               (let [files-in-statement-dir (os/dir statement-dir)]
                                 # TODO: autoincrement
                                 # Filter all files whose prefix is out and suffix is .tsv.
                                 # Split each file such that given some form `out-n.tsv`, n is separated and parsed as an int
                                 # Get the highest int, then increment.
                                 (string statement-dir "/" "out.tsv"))))
            statement-format-peg (get jdn::statement-formats/jdns statement-format)
            statement-grammar (update (struct/to-table statement-format-peg)
                                      :pattern
                                 (fn [pattern]
                                   (merge base-grammar pattern)))
            pdf-text (read-pdf input-path password)
            parsed-soa (parse-soa statement-grammar pdf-text)
            output-text (data->tsv parsed-soa)]
        (if (or stdout? (not output-path))
          (print output-text)
          (spit output-path output-text))))))


