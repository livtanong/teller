(ql:quickload "cl-ppcre")
(ql:quickload "cl-pdf-parser")

(let ((ptrn (ppcre:create-scanner "[1-9]")))
  (ppcre:scan-to-strings ptrn "1"))

(defvar *pdf-stream* nil)
(defvar date-regex "((0[1-9]|1[0-2])\/(0[1-9]|[12]\\d|3[01])\/\\d\\d)")
(defvar currency-regex "([0-9]{1,3},([0-9]{3},)*[0-9]{3}|[0-9]+)(.[0-9][0-9])")
(defvar reference-regex "Reference: \\d+")
(defvar forex-regex "\\d+\\.\\d+ (U.S.DOLLARS)")
(defvar misc-regex (format nil "(~a|~a)" reference-regex forex-regex))
(defvar entry-regex (format nil "~a[ \t]+~a[ \t]+.+~a"
                            date-regex date-regex currency-regex))

(defvar entry-scanner (ppcre:create-scanner entry-regex))
(defvar misc-scanner (ppcre:create-scanner misc-regex))

(defun from-home (path)
  "Specifically for `uiop:run-program' because it has trouble with tildes.
`path' must be some path relative to home.
Return a string representing the absolute path."
  (let ((home-string (namestring (user-homedir-pathname))))
    (concatenate 'string home-string path)))

(defun pdftotext (pdf-path txt-path &key page-start)
  "Runs a shell script, `pdftotext'. There may be many incarnations of this
script, so to be safe, use the version that comes with `poppler'. Both
`pdf-path' and `txt-path' are assumed not to be able to handle `~~' as the
shortcut for home.
Make sure to use `from-home' to get a path that comes from home."
  (uiop:run-program (list "pdftotext"
                          "-f" (write-to-string page-start)
                          "-nopgbrk"
                          "-layout" ; Preserve original PDF layout
                          pdf-path
                          txt-path)))

(defun parse-entry (line)
  "Parse only the first identifiable line of an entry. Return a PLIST"
  (let* ((listed (ppcre:split "\\s{3,}" (string-trim " " line)))
         (sale-date (first listed))
         (post-date (second listed))
         (description (third listed))
         (value (fourth listed)))
    (list
     :sale-date sale-date
     :post-date post-date
     :description description
     :value value)))

(defun parse-misc (line)
  "Parse misc info after the first identifiable line of an entry."
  (let ((misc (string-trim " " line)))
    misc))

(defun parse-statement (pdf-path)
  (let* ((temp-dir (namestring (uiop:temporary-directory)))
         (temp-filename "statement")
         (temp-path-string (concatenate 'string temp-dir temp-filename ".txt"))
         (temp-path (pathname temp-path-string)))
    (print (format nil "Converting pdf to: ~a" temp-path))
    ;; Generate a text file that we can parse
    (pdftotext pdf-path
               temp-path-string
               :page-start 2)
    (let ((transactions (list)))
      ;; Open the temp file
      (with-open-file (*pdf-stream* temp-path
                                    :direction :input
                                    :external-format :utf8)
        (loop for line = (read-line *pdf-stream* nil)
              while line
              do (progn
                   (cond
                     ((ppcre:scan entry-scanner line) (let ((entry (parse-entry line)))
                                                        (setf transactions (cons entry transactions))))
                     ((ppcre:scan misc-scanner line) (let* ((misc (parse-misc line)))
                                                       (setf (getf (car transactions) :misc) misc)))))))

      ;; Delete temp file because let's be responsible.
      (print (format nil "Deleting temp file at: ~a" temp-path))
      (delete-file temp-path)
      (print "Temp file deleted")
      transactions)
    ))

(parse-statement (from-home "Downloads/statement.pdf"))
