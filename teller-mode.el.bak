;;; teller-mode.el --- Helps match statement line items to ledger transactions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun teller-string (password)
  "Reconcile statement. PASSWORD for encrypted files."
  (let ((statement (shell-command-to-string
                    (concat
                     "teller -i '~/Downloads/statement.pdf' -d 'mm/dd/yy'"
                     (if password
                         (concat " -p " password)
                       "")))))
    (if (string-match-p "WARNING" statement)
        (warn "Locale error. Please ensure that the environment variable LANG is set properly.")
      ;; Remove last newline
      (replace-regexp-in-string "\n\\'" "" statement))))

(defun teller-reconcile (password)
  (let* ((teller-output (split-string (teller-string password) "\n"))
         (raw-rows (mapcar (lambda (x)
                             (split-string x ","))
                           teller-output))
         (columns (apply #'vector (mapcar (lambda (field)
                                            (list field
                                                  (cond
                                                   ((equal field "sale-date") 11)
                                                   ((equal field "post-date") 11)
                                                   ((equal field "amount") 11)
                                                   (t 30))))
                                          (car raw-rows))))
         (rows (mapcar (lambda (line)
                         (list nil (apply #'vector line)))
                       (cdr raw-rows))))
    (switch-to-buffer "*teller*")
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)))

;; For each item in the list of transactions
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Tabulated-List-Mode.html
;; Maybe make this a checklist.
;; Find a list of potential candidates in the transaction list for any selected element.
;; Also see https://github.com/magit/transient

(provide 'teller-mode)
;;; teller-mode.el ends here
