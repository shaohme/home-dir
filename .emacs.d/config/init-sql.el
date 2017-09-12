;;; package -- Summary
;;; Commentary:
;;; SQL development settings
;;; Code:

(defun init-sql-mode()
  ;; (require 'auto-complete)
  ;; (require 'auto-complete-config)
  ;; (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary
  ;;  ac-source-words-in-same-mode-buffers))
  ;; (setq-local ac-ignore-case t)
  ;; (auto-complete-mode t)
  ;; (ac-flyspell-workaround)
  (flyspell-prog-mode))

(add-hook 'sql-mode-hook 'init-sql-mode)

(defun init-sql-interactive-mode()
  (toggle-truncate-lines t))

(add-hook 'sql-interactive-mode-hook 'init-sql-interactive-mode)


(provide 'init-sql)
;;; init-sql.el ends here
