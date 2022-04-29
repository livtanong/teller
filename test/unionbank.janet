(import testament :prefix "" :exit true)
(import ../src/teller :as teller)
(import jdn-loader)
(import jdn::../src/statement-formats :as statement-formats :jdn-loader/binding-type :struct)

(def grammar
  (table/to-struct
    (merge teller/base-grammar
           (get statement-formats/jdns :unionbank))))

           
(def multiline-statement-text
  ``
  For billing concerns you may call our Customer Service Hotline at (01)2345‐6789 or send an email to customer.service@unionbankph.com
  Transaction            Posting Date                                        Transaction Details                                      Amount
     Date
                                              PREVIOUS BALANCE                                                                                   0.00

                                              MASTERCARD 123456******7890

  01/18/2021             01/18/2021           U‐SECURE Premium CHARGE                                                                          270.48

                                              SUBTOTAL                                                                                         270.48

  01/23/2021             01/23/2021           STEAMGAMES.COM 12345678, Hamburg                                                               1,557.99

  01/25/2021             01/26/2021           EVEDEVICES.COM, WAN CHAI                                                                      39,535.32

                                              USD 797.00

  02/01/2021             02/02/2021           Patreon* Membership, INTERNET                                                                     99.19

                                              USD 2.00

  02/05/2021             02/06/2021           LAZADA KZ, LIGMA                                                                                 731.40

  Credit Card Statement of Account (Page 2/3)                                                                     STATEMENT SUMMARY
  ``)

(deftest simple-parse
  (is (deep= @[["01/23/2021" "01/23/2021" "STEAMGAMES.COM 12345678, Hamburg" "" "1,557.99"]]
             (teller/parse-soa grammar "   01/23/2021             01/23/2021           STEAMGAMES.COM 12345678, Hamburg                                                               1,557.99    "))))

(deftest multiline-parse
  (is (deep= @[["01/18/2021" "01/18/2021" "U-SECURE Premium CHARGE" "" "270.48"]
               ["01/23/2021" "01/23/2021" "STEAMGAMES.COM 12345678, Hamburg" "" "1,557.99"]
               ["01/25/2021" "01/26/2021" "EVEDEVICES.COM, WAN CHAI" "USD 797.00" "39,535.32"]
               ["02/01/2021" "02/02/2021" "Patreon* Membership, INTERNET" "USD 2.00" "99.19"]
               ["02/05/2021" "02/06/2021" "LAZADA KZ, LIGMA" "" "731.40"]]
             (teller/parse-soa grammar multiline-statement-text))))

(run-tests!)
