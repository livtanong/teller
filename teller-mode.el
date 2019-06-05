;;; teller-mode.el --- Helps match statement line items to ledger transactions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar teller-statement
  (shell-command-to-string
   "teller -i '~/Downloads/decrypted-statement.pdf'"
   ))

(provide 'teller-mode)
;;; teller-mode.el ends here
