(asdf:defsystem "teller"
  :description "A simple tool to parse a bank statement pdf, and convert to csv."
  :depends-on ("cl-ppcre" "str" "cl-csv" "parse-float" "unix-opts")
  :components ((:file "teller")))
