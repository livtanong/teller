(defpackage :teller
  (:use :common-lisp)
  (:export #:tell))

(in-package :teller)


(defvar sane-date-format "yyyy/mm/dd")

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(opts:define-opts
    (:name :help
     :description "Print this help text"
     :short #\h
     :long :help)
    (:name :input
     :description "Input file. Should be a pdf."
     :short #\i
     :long "input"
     :arg-parser #'pathname
     :meta-var "FILE"
     :required t)
  (:name :output
   :description "Output file. Should be csv."
   :short #\o
   :long "output"
   :arg-parser #'pathname
   :meta-var "FILE")
  (:name :password
   :description "Password for encrypted file"
   :short #\p
   :long "password"
   :arg-parser #'identity
   :meta-var "STRING")
  (:name :input-date-format
   :description "Date format of the pdf because some institutions are insane. Default: \"yyyy/mm/dd\". If the year is only two y's, (e.g. mm/dd/yy) teller prepends \"20\" to the year."
   :short #\d
   :long "input-date-format"
   :arg-parser #'identity
   :meta-var "STRING"))

(ppcre:define-parse-tree-synonym :comma
  #\,)

(defun n-digits (n)
  `(:NON-GREEDY-REPETITION ,n ,n :DIGIT-CLASS))

(setf (ppcre:parse-tree-synonym :2-digits) (n-digits 2))

(setf (ppcre:parse-tree-synonym :3-digits) (n-digits 3))

(setf (ppcre:parse-tree-synonym :4-digits) (n-digits 4))

(ppcre:define-parse-tree-synonym :long-space
    (:NON-GREEDY-REPETITION 1 NIL (:CHAR-CLASS #\  #\t)))

(ppcre:define-parse-tree-synonym :mm
    (:GROUP
     (:ALTERNATION
      (:SEQUENCE #\0 (:CHAR-CLASS (:RANGE #\1 #\9)))
      (:SEQUENCE #\1 (:CHAR-CLASS (:RANGE #\0 #\2))))))

(ppcre:define-parse-tree-synonym :dd
    (:GROUP
     (:ALTERNATION
      (:SEQUENCE #\0 :DIGIT-CLASS)
      (:SEQUENCE (:CHAR-CLASS #\1 #\2) :DIGIT-CLASS)
      (:SEQUENCE #\3 (:CHAR-CLASS #\0 #\1)))))

(ppcre:define-parse-tree-synonym :yy
  :2-digits)

(ppcre:define-parse-tree-synonym :yyyy
  :4-digits)

(ppcre:define-parse-tree-synonym :/
  (:ALTERNATION #\/ #\-))

(ppcre:define-parse-tree-synonym :date
    (:ALTERNATION
     ;; Sanest
     (:SEQUENCE :yyyy :/ :mm :/ :dd)
     ;; Sane
     (:SEQUENCE :dd :/ :mm :/ :yyyy)
     ;; Insane
     (:SEQUENCE :mm :/ :dd :/ :yyyy)
     ;; Even more insane.
     (:SEQUENCE :mm :/ :dd :/ :yy)))

(ppcre:define-parse-tree-synonym :amount
    (:SEQUENCE
     (:NON-GREEDY-REPETITION 1 3 :DIGIT-CLASS)
     (:NON-GREEDY-REPETITION 0 NIL
                             (:GROUP
                              (:SEQUENCE
                               (:NON-GREEDY-REPETITION 0 1 #\,)
                               :3-digits
                               )))
     #\.
     :2-digits))

(ppcre:define-parse-tree-synonym :description
    (:SEQUENCE
     (:GREEDY-REPETITION 1 NIL :NON-WHITESPACE-CHAR-CLASS)
     (:GREEDY-REPETITION 1 NIL
                         (:GROUP
                          (:SEQUENCE
                           (:NEGATIVE-LOOKAHEAD (:GREEDY-REPETITION 2 2 :WHITESPACE-CHAR-CLASS))
                           :EVERYTHING)))))

(ppcre:define-parse-tree-synonym :forex
    (:SEQUENCE :amount " U.S.DOLLARS"))

(ppcre:define-parse-tree-synonym :charge-reference
    (:SEQUENCE
     "Reference: "
     (:ALTERNATION
      "1              0"
      (:GREEDY-REPETITION 1 NIL :DIGIT-CLASS))))

(ppcre:define-parse-tree-synonym :simple-entry
    (:SEQUENCE
     (:REGISTER :date)
     :long-space
     (:REGISTER :date)
     :long-space
     (:REGISTER :description)
     :long-space
     (:REGISTER :amount)))

(ppcre:define-parse-tree-synonym :misc
    (:ALTERNATION
     :forex
     :charge-reference))

(ppcre:define-parse-tree-synonym :entry
    (:SEQUENCE
     :simple-entry
     (:GREEDY-REPETITION 0 1
                         (:GROUP
                          (:SEQUENCE
                           (:NON-GREEDY-REPETITION 0 1 :long-space)
                           #\Newline
                           :long-space
                           (:REGISTER
                            :misc))))))

(defun pdf-to-text (pdf-path password)
  "Runs a shell script, `pdftotext'. There may be many incarnations of this
script, so to be safe, use the version that comes with `poppler'."
  (let ((command (if password
                     (format nil "pdftotext -nopgbrk -layout -upw ~a ~a -" password pdf-path)
                     (format nil "pdftotext -nopgbrk -layout ~a -" pdf-path))))
    (uiop:run-program command :output :string)))

(defun transform-statement-entries (values input-date-format)
  (let* ((sale-date (normalize-date (nth 0 values) input-date-format))
         ;; I certainly hope your bank uses the same date format for both sale-date and post-date
         (post-date (normalize-date (nth 1 values) input-date-format))
         (amount (ppcre:regex-replace "," (nth 3 values) ""))
         ;; because we want to be sane. We also don't want to rebuild the list from scratch
         ;; because we don't know if `values' is of size 4 or 5.
         ;; (see :simple-entry vs :entry)
         (new-values (copy-list values)))
    (setf (nth 0 new-values) sale-date)
    (setf (nth 1 new-values) post-date)
    (setf (nth 3 new-values) amount)
    new-values))

(defun parse-statement (pdf-path password input-date-format)
  "Eventually we can pass on the transform function to be available to the cli, maybe."
  (let* ((pdf-text (pdf-to-text (namestring pdf-path) password))
         (matches (ppcre:all-matches-as-strings :entry pdf-text)))
    (map 'list
         (lambda (match)
           ;; ppcre:scan-to-strings returns two values: the match, and the registers.
           ;; We want the registers, because they're the "extracted" data.
           (transform-statement-entries
            (coerce (nth-value 1 (ppcre:scan-to-strings :entry match)) 'list)
            input-date-format))
         matches)))

(defun normalize-date (insane-date-string input-date-format)
  "Always returns dates in yyyy/mm/dd format"
  (let* ((date-format (ppcre:split "\/" input-date-format))
         (date (ppcre:split "\/" insane-date-string))
         (date-plist (mapcan (lambda (k v)
                               (list (intern (string-upcase k) "KEYWORD") v))
                             date-format date))
         (yyyy (getf date-plist :yyyy))
         (year (or yyyy (concatenate 'string "20" (getf date-plist :yy)))))
    ;; prepending with 20 because... i wrote this at 2019, and i'm going to be irresponsible.
    (concatenate 'string year "/" (getf date-plist :mm) "/" (getf date-plist :dd))))

(defun pdf-to-csv (pdf-path csv-path password input-date-format)
  (let* ((transformed (parse-statement pdf-path
                                       password
                                       input-date-format))
         (output-data (cons '("sale-date" "post-date" "description" "amount" "misc") transformed))
         (output-stream (if csv-path
                            (open csv-path :direction :output
                                           :if-exists :overwrite
                                           :if-does-not-exist :create)
                            *standard-output*)))
    (cl-csv:write-csv output-data :stream output-stream)
    (close output-stream)))

(defun tell ()
  (let* ((options (opts:get-opts))
         (input (getf options :input))
         (output (getf options :output))
         (password (getf options :password))
         (input-date-format (or (getf options :input-date-format) sane-date-format))
         )
    (pdf-to-csv input output password input-date-format)))
