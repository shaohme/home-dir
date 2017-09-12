;;; package -- Summary
;;; Commentary:
;;; Ledger settings
;;; Code:

;;; Ledger mode-line
(defun init-ledger-mode()
  (flycheck-mode t)
  )

(add-hook 'ledger-mode-hook 'init-ledger-mode)


(provide 'init-ledger)
;;; init-ledger.el ends here
