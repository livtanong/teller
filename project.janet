(declare-project
  :name "teller"
  :description ""
  :dependencies ["https://github.com/pyrmont/testament"
                 "https://github.com/andrewchambers/janet-jdn"
                 "https://github.com/janet-lang/argparse"])

(declare-source
 :source ["src/teller.janet" "src/statement-formats/bdo.janet"])

(declare-executable
 :name "teller"
 :entry "src/teller.janet")
