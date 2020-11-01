(import testament :prefix "" :exit true)
(import src/teller :as teller)
(import jdn-loader)
(import jdn::statement-formats :jdn-loader/binding-type :struct)

(def grammar
  (table/to-struct
    (merge teller/base-grammar
      (get jdn::statement-formats/jdns :bdo))))

(deftest simple-parse
  (is (= @[@["2020/08/08" "2020/08/11" "yo waddup dawg" "1,340.50"]]
         (peg/match teller/soa-grammar "2020/08/08   2020/08/11      yo waddup dawg   1,340.50"))))

(deftest multiline-parse
  (is (= @[@["2020/08/08" "2020/08/11" "yo waddup dawg" "1,340.50" "somethign something"]
           @["2020/08/09" "2020/08/12" "iasdpfiawser" "1,420.50"]]
         (peg/match teller/soa-grammar "wat 2020/08/08   2020/08/11      yo waddup dawg   1,340.50    \n     somethign something    \n       2020/08/09   2020/08/12      iasdpfiawser   1,420.50    "))))
