;;; teller-mode.el --- Helps match statement line items to ledger transactions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun teller-reconcile (password)
  "Reconcile statement. PASSWORD for encrypted files."
  (let ((statement (shell-command-to-string
                    (concat
                     "teller -i '~/Downloads/statement.pdf'"
                     (if password
                         (concat " -p " password)
                       "")))))
    (if (string-match-p "WARNING" statement)
        (warn "Locale error. Please ensure that the environment variable LANG is set properly.")
      statement)))

(provide 'teller-mode)
;;; teller-mode.el ends here
