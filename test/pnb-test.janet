(import testament :prefix "" :exit true)
(import ../src/teller :as teller)
(import jdn-loader)
(import jdn::../src/statement-formats :as statement-formats :jdn-loader/binding-type :struct)

(def grammar
  (teller/with-base-grammar
   (get statement-formats/jdns :pnb)))

(def multiline-statement-text
  ``
1801-11-818-9-818 or visit www.pnbcards.com.ph for complete Terms and Conditions.

1801-11-818-9-818. Errors on your statement must be reported to us within

1801-11-818-9-818. Please note that all

1801-11-818-9-818. Visit www.pnbcards.com.ph for more
information.

1801-11-818-9-818.

ACCOUNT DETAILS
  TRANS    POST                     REFERENCE                                         DESCRIPTION                                             AMOUNT
   DATE    DATE                      NUMBER
                                                          PREVIOUS BALANCE                                                             P                0.00
                                                          1234-5678-9012-3456 JOSEPH JOESTAR
    19/12            19/12         12345678901            BAYAD CENTER MERALCO                  PASAY          WTF                                2,883.55
    24/12            25/12         23456789012            FACEBK XTJBT7P442                 FB.ME/ADS         IRL                                 1,666.12
    09/01            11/01         34567890123            BAYAD CENTER MERALCO                  PASAY          WTF                                2,420.02
                                                          CURRENT BALANCE                                                                         6,969.69

  ``)

(deftest multiline-parse
  (is (deep= @[@["\"19/12\"" "\"19/12\"" "\"12345678901\"" "\"BAYAD CENTER MERALCO\"" "\"PASAY\"" "\"WTF\"" "\"2,883.55\"" "\"\""]
               @["\"24/12\"" "\"25/12\"" "\"23456789012\"" "\"FACEBK XTJBT7P442\"" "\"FB.ME/ADS\"" "\"IRL\"" "\"1,666.12\"" "\"\""]
               @["\"09/01\"" "\"11/01\"" "\"34567890123\"" "\"BAYAD CENTER MERALCO\"" "\"PASAY\"" "\"WTF\"" "\"2,420.02\"" "\"\""]]
             (teller/parse-entries {} (get grammar :entries) multiline-statement-text))))

(run-tests!)
