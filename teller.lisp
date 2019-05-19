(defpackage :teller
  (:use :common-lisp)
  (:export #:tell))

(in-package :teller)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(opts:define-opts
  (:name :input
   :description "Input file. Should be a pdf."
   :short #\i
   :long "input"
   :arg-parser #'pathname
   :meta-var "FILE")
  (:name :output
   :description "Output file. Should be csv."
   :short #\o
   :long "output"
   :arg-parser #'pathname
   :meta-var "FILE"))

(ppcre:define-parse-tree-synonym :comma
  #\,)

(defun n-digits (n)
  `(:NON-GREEDY-REPETITION ,n ,n :DIGIT-CLASS))

(setf (ppcre:parse-tree-synonym :2-digits) (n-digits 2))

(setf (ppcre:parse-tree-synonym :3-digits) (n-digits 3))

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

(ppcre:define-parse-tree-synonym :/
  (:ALTERNATION #\/ #\-))

(ppcre:define-parse-tree-synonym :mm/dd/yy
    (:SEQUENCE :mm :/ :dd :/ :yy))

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
     (:REGISTER :mm/dd/yy)
     :long-space
     (:REGISTER :mm/dd/yy)
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

(defun pdf-to-text (pdf-path)
  "Runs a shell script, `pdftotext'. There may be many incarnations of this
script, so to be safe, use the version that comes with `poppler'."
  (let ((command (format nil "pdftotext -nopgbrk -layout ~a -" pdf-path)))
    (uiop:run-program command :output :string)))

(defun parse-statement (pdf-path)
  (let* ((pdf-text (pdf-to-text (namestring pdf-path)))
         (matches (ppcre:all-matches-as-strings :entry pdf-text)))
    (map 'list
         (lambda (match)
           (coerce (nth-value 1 (ppcre:scan-to-strings :entry match)) 'list))
         matches)))

(defun pdf-to-csv (pdf-path csv-path)
  (let ((output-data (cons '("sale-date" "post-date" "description" "amount" "misc")
                           (parse-statement pdf-path)))
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
         (output (getf options :output)))
    (pdf-to-csv input output)))
