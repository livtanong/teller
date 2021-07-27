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
  For billing concerns you may call our Customer Service Hotline at (02)8841‐8600 or send an email to customer.service@unionbankph.com
  Transaction            Posting Date                                        Transaction Details                                      Amount
     Date
                                              PREVIOUS BALANCE                                                                                   0.00

                                              MASTERCARD 545420******2250

  07/19/2021             07/19/2021           U‐SECURE Premium CHARGE                                                                          270.46

                                              SUBTOTAL                                                                                         270.46

  06/26/2021             06/26/2021           STEAMGAMES.COM 4259522, Hamburg                                                               1,457.99

  06/28/2021             06/29/2021           PP*1476CODE, 4029357733                                                                          ‐100.00

  06/28/2021             06/29/2021           PP*1476CODE, 4029357733                                                                          100.00

  06/29/2021             06/30/2021           EVEDEVICES.COM, WAN CHAI                                                                     39,535.31

                                              USD 798.00

  07/01/2021             07/02/2021           Patreon* Membership, INTERNET                                                                     99.09

                                              USD 2.00

  07/01/2021             07/02/2021           Patreon* Membership, INTERNET                                                                    247.72

                                              USD 5.00

  07/05/2021             07/06/2021           LAZADA PH, MAKATI                                                                                733.40
  ``)

(deftest simple-parse
  (is (deep= @[["06/26/2021" "06/26/2021" "STEAMGAMES.COM 4259522, Hamburg" "" "1,457.99"]]
             (teller/parse-soa grammar "   06/26/2021             06/26/2021           STEAMGAMES.COM 4259522, Hamburg                                                               1,457.99    "))))

             # 
(deftest multiline-parse
  (is (deep= @[["07/19/2021" "07/19/2021" "U-SECURE Premium CHARGE" "" "270.46"]
               ["06/26/2021" "06/26/2021" "STEAMGAMES.COM 4259522, Hamburg" "" "1,457.99"]
               ["06/28/2021" "06/29/2021" "PP*1476CODE, 4029357733" "" "-100.00"]
               ["06/28/2021" "06/29/2021" "PP*1476CODE, 4029357733" "" "100.00"]
               ["06/29/2021" "06/30/2021" "EVEDEVICES.COM, WAN CHAI" "USD 798.00" "39,535.31"]
               ["07/01/2021" "07/02/2021" "Patreon* Membership, INTERNET" "USD 2.00" "99.09"]
               ["07/01/2021" "07/02/2021" "Patreon* Membership, INTERNET" "USD 5.00" "247.72"]
               ["07/05/2021" "07/06/2021" "LAZADA PH, MAKATI" "" "733.40"]]
             (teller/parse-soa grammar multiline-statement-text))))

(run-tests!)
