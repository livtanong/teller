;;; teller-mode.el --- Helps match statement line items to ledger transactions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun teller-reconcile ()
  "Reconcile statement."
  (let ((statement (shell-command-to-string "teller -i '~/Downloads/decrypted-statement.pdf'")))
    (if (string-match-p "WARNING" statement)
        (warn "Locale error. Please ensure that the environment variable LANG is set properly.")
      statement)))

(teller-reconcile)

(provide 'teller-mode)
;;; teller-mode.el ends here
