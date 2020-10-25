(declare-project
  :name "teller"
  :description ""
  :dependencies ["https://github.com/pyrmont/testament"
                 "https://github.com/andrewchambers/janet-jdn"
                 "https://github.com/janet-lang/argparse"
                 "https://github.com/levitanong/jdn-loader"])

(declare-source
  :source ["src/teller.janet"])

(declare-executable
  :name "teller"
  :entry "src/teller.janet")
