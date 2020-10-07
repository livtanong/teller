(defn run-cmd [args]
  (let [out-temp-file (file/temp)
        err-temp-file (file/temp)
        exit-code (os/execute args :p {:err err-temp-file :out out-temp-file})]
    (file/seek out-temp-file :set 0)
    (file/seek err-temp-file :set 0)
    {:exit-code exit-code 
     :err (file/read err-temp-file :all) 
     :out (file/read out-temp-file :all)}))

(defn read-pdf-with-password []
  )

(defn derp []
  (run-cmd ["ls"]))

(defn foo [s]
  (print s))

(defn bar []
  (foo "derp"))

(def soa-grammar
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
    :date (choice :yyyy-mm-dd
             :dd-mm-yyyy
             :mm-dd-yyyy
             :mm-dd-yy
             :yy-mm-dd)
    :description (sequence (some :S) (any (sequence :s (some :S))))
    :financial-number (sequence
                       (? "-")
                       (at-most 3 :d)
                       (any (sequence
                             ","
                             (repeat 3 :d)))
                       "."
                       (some :d))
    :amount :financial-number
    :simple-entry (sequence
                     # :s*
                     (capture :date :date1)
                     :s+
                     (capture :date :date2)
                     :s+
                     (capture :description :description)
                     :s+
                     (capture :amount :amount)
                     # :s*
                     )
    :multiline-entry  (sequence
                        :simple-entry 
                        :s*
                         # :s*
                         # (not :simple-entry)
                         (if-not :simple-entry (capture :description))
                         )
                         
    :entry (thru (group (choice :multiline-entry :simple-entry)))
  :main (thru (some :entry)) #(sequence (any (choice :s :S)) :entries)
    })
(peg/match "\n" "\n   ")
(peg/match '(any (sequence (? ",") (repeat 3 :d))) "123")
(peg/match soa-grammar "2020/08/08   2020/08/11      yo waddup dawg   1,340.50")
(peg/match soa-grammar "wat 2020/08/08   2020/08/11      yo waddup dawg   1,340.50    \n     somethign something    \n       2020/08/09   2020/08/12      iasdpfiawser   1,420.50    ")
(peg/match soa-grammar "2020/08/08 2020/08/11")
(peg/match '(sequence "0" (range "19")) "08")

(defn read-pdf [password]
  (let [{:exit-code exit-code
         :err err
         :out out} (run-cmd ["pdftotext" "-nopgbrk" "-layout" "-upw" password "/mnt/c/Users/Levi/Downloads/statement.pdf" "-"])
        password-buffer @""]
    (print "exit code: " exit-code)
    (cond
      (= exit-code 0) out
      (and (not= exit-code 0)
           (string/find "Command Line Error: Incorrect password" err)) (do (print "prompt for password")
                                                                           (getline "Password: " password-buffer)
                                                                           (read-pdf (string/trimr (string/slice password-buffer) "\n")))
      :else (do (print "lol some other error")))))

(peg/match '(thru "Credit Cards") (read-pdf "250107"))
(peg/match soa-grammar (read-pdf "250107"))
