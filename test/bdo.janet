(import testament :prefix "" :exit true)
(import ../src/teller :as teller)
(import jdn-loader)
(import jdn::../src/statement-formats :as statement-formats :jdn-loader/binding-type :struct)

(def grammar
  (teller/with-base-grammar
   (struct/to-table
    (get statement-formats/jdns :bdo))))

(def multiline-statement-text
  ``
        08/08/20   08/11/20      yo waddup dawg        1,340.50    
                                 somethign something    
        08/09/20   08/12/20      iasdpfiawser          1,420.50      
  ``)

(deftest simple-parse
  (is (deep= @[@["\"08/08/20\"" "\"08/11/20\"" "\"yo waddup dawg\"" "\"\"" "\"1,340.50\""]]
             (teller/parse-soa grammar "        08/08/20   08/11/20      yo waddup dawg        1,340.50     "))))

(deftest multiline-parse
  (is (deep= @[@["\"08/08/20\"" "\"08/11/20\"" "\"yo waddup dawg\"" "\"somethign something\"" "\"1,340.50\""]
               @["\"08/09/20\"" "\"08/12/20\"" "\"iasdpfiawser\"" "\"\"" "\"1,420.50\""]]
             (teller/parse-soa grammar multiline-statement-text))))

(run-tests!)
