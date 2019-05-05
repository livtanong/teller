(ql:quickload "cl-ppcre")
(ql:quickload "cl-pdf-parser")
(ql:quickload "str")

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

(ppcre:define-parse-tree-synonym :date
    (:SEQUENCE :mm #\/ :dd #\/ :yy))

(ppcre:define-parse-tree-synonym :sale-date :date)

(ppcre:define-parse-tree-synonym :post-date :date)

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
     :2-digits
     ))

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
      (:GREEDY-REPETITION 1 NIL :DIGIT-CLASS)
      "1              0")))

(ppcre:define-parse-tree-synonym :simple-entry
    (:SEQUENCE
     (:REGISTER :sale-date)
     :long-space
     (:REGISTER :post-date)
     :long-space
     (:REGISTER :description)
     :long-space
     (:REGISTER :amount)
     ))

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
                           :long-space
                           #\Newline
                           :long-space
                           (:REGISTER
                            :misc))))
     ))

(defvar sample-entry "02/21/19   02/24/19   Globe   799.00
   94.28 U.S.DOLLARS
  02/21/19   02/24/19   Globe 2  800.00  
  02/21/19   02/24/19   Globe 2  800.00
                        Reference: 867585647654   ")

(defun from-home (path)
  "Specifically for `uiop:run-program' because it has trouble with tildes.
`path' must be some path relative to home.
Return a string representing the absolute path."
  (let ((home-string (namestring (user-homedir-pathname))))
    (concatenate 'string home-string path)))

(defun pdftotext (pdf-path &key page-start)
  "Runs a shell script, `pdftotext'. There may be many incarnations of this
script, so to be safe, use the version that comes with `poppler'.
`pdf-path' is assumed not to be able to handle `~~' as the shortcut for home.
Make sure to use `from-home' to get a path that comes from home.
Return output text."
  (uiop:run-program (list "pdftotext"
                          "-f" (write-to-string page-start)
                          "-nopgbrk"
                          "-layout" ; Preserve original PDF layout
                          pdf-path
                          "-")
                    :output :string
                    ))

(defun parse-statement (pdf-path)
  ;; Generate a text file that we can parse
  (let ((pdf-text (pdftotext pdf-path :page-start 2))
        (entries (list)))
    ;; (ppcre:all-matches-as-strings entry-regex pdf-text)
    (ppcre:do-register-groups (sale-date post-date description amount misc)
        (:entry pdf-text)
      (push (list sale-date post-date description amount misc) entries))
    (reverse entries))
  ;; (let ((transactions (list)))
  ;;   ;; Open the temp file
  ;;   (with-open-file (*pdf-stream* temp-path
  ;;                                 :direction :input
  ;;                                 :external-format :utf8)
  ;;     (loop for line = (read-line *pdf-stream* nil)
  ;;           while line
  ;;           do (cond
  ;;                ((ppcre:scan entry-scanner line) (let ((entry (parse-entry line)))
  ;;                                                   (setf transactions (cons entry transactions))))
  ;;                ((ppcre:scan misc-scanner line) (let* ((misc (parse-misc line)))
  ;;                                                  (setf (getf (car transactions) :misc) misc))))))

  ;;   ;; Delete temp file because let's be responsible.
  ;;   (print (format nil "Deleting temp file at: ~a" temp-path))
  ;;   (delete-file temp-path)
  ;;   (print "Temp file deleted")
  ;;   transactions)
  )

(parse-statement (from-home "Downloads/statement.pdf"))
