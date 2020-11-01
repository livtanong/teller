(import testament :prefix "" :exit true)
(import src/teller :as teller)

(deftest simple-parse
  (is (= (peg/match teller/soa-grammar "2020/08/08   2020/08/11      yo waddup dawg   1,340.50")
         @[@["2020/08/08" "2020/08/11" "yo waddup dawg" "1,340.50"]])))

(deftest multiline-parse
  (is (= (peg/match teller/soa-grammar "wat 2020/08/08   2020/08/11      yo waddup dawg   1,340.50    \n     somethign something    \n       2020/08/09   2020/08/12      iasdpfiawser   1,420.50    ")
         @[@["2020/08/08" "2020/08/11" "yo waddup dawg" "1,340.50" "somethign something"]
           @["2020/08/09" "2020/08/12" "iasdpfiawser" "1,420.50"]])))
