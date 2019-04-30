(ql:quickload "cl-ppcre")
(ql:quickload "cl-pdf-parser")

(let ((ptrn (ppcre:create-scanner "[1-9]")))
  (ppcre:scan-to-strings ptrn "1"))

(defvar *pdf-stream* nil)
(defvar date-regex "((0[1-9]|1[0-2])\/(0[1-9]|[12]\\d|3[01])\/\\d\\d)")
(defvar date-scanner (ppcre:create-scanner date-regex))

(ppcre:scan-to-strings date-scanner "02/23/19")
(ppcre:scan-to-strings date-scanner "")

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

(pdftotext (from-home "Downloads/statement.pdf")
           (from-home "Downloads/statement.txt")
           :page-start 2)

(with-open-file (*pdf-stream* #P"~/Downloads/statement.txt"
                              :direction :input
                              :external-format :utf8)
  (let ((prev-line nil))
    (loop for line = (read-line *pdf-stream* nil)
          while line
          do (progn
               (if (ppcre:scan-to-strings date-scanner line)
                   (print line)
                   (when (ppcre:scan-to-strings date-scanner prev-line)
                     (print line)))
               (setf prev-line line))))
  )
