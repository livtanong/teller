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
             :help "The path to the config file."}
   "format" {:kind :option
             :short "f"
             :help "The path to the format jdn."}
   "input" {:kind :option
            :short "i"
            :help "Optional. The path to the input file. Must be a pdf. If not provided, will look in config file."}
   "output" {:kind :option
             :short "o"
             :help "Optional. The path to the output file. Must be a tsv. If not provided, will print to STDOUT"}])

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
  :simple-phrase (sequence (some :S) (any (sequence :s (some :S))))})

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
  (let [padded-row (pad-array tuple-row 5 "")]
    (string/join padded-row "\t")))

(defn data->tsv [parsed-soa]
  (string/join
   (map render-row
        parsed-soa)
   "\n"))

(defn main [& args]
  (with-dyns [:args args]
    (let [res (argparse/argparse (splice argparse-params))]
      (unless res
        (os/exit 1))
      (let [home-path (os/getenv "HOME")
            default-config-path (os/getenv "TELLER_CONFIG" (string home-path "/.config/teller/config.jdn"))
            format-key (get res "format")
            input-path (get res "input")
            config-path (get res "config" default-config-path)
            config (jdn/decode (slurp config-path))
            statement-dir (get config :statement-dir)
            output-path (get res "output"
                          (if statement-dir
                            (let [input-filename (path/basename input-path)
                                  input-ext (path/ext input-filename)
                                  no-ext (if (string/has-suffix? input-ext input-filename)
                                           (first (split-filename input-filename))
                                           input-filename)]
                              (string statement-dir "/" no-ext ".tsv"))
                            (let [files-in-statement-dir (os/dir statement-dir)]
                              # TODO: autoincrement
                              # Filter all files whose prefix is out and suffix is .tsv.
                              # Split each file such that given some form `out-n.tsv`, n is separated and parsed as an int
                              # Get the highest int, then increment.
                              (string statement-dir "/" "out.tsv"))))
            statement-format (get jdn::statement-formats/jdns (get config :statement-format))
            statement-grammar (table/to-struct
                               (merge base-grammar statement-format))
            pdf-text (read-pdf input-path "")
            parsed-soa (peg/match statement-grammar pdf-text)
            output-text (data->tsv parsed-soa)]
        (if output-path
          (spit output-path output-text)
          output-text)))))

#(main "teller" "--input" "/mnt/c/Users/Levi/Downloads/statement.pdf")
