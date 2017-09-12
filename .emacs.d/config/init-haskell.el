;;; package -- Summary
;;; Commentary:
;;; Haskell development settings
;;; Code:

(require 'company)
(require 'haskell)

(defun init-haskell-mode()
  (add-to-list 'company-backends 'company-ghc)
  (company-mode t)
  ;; (company-quickhelp-mode t)
  ;; (flycheck-haskell-setup)
  ;; (flycheck-mode t)
  )

(add-hook 'haskell-mode-hook 'init-haskell-mode)


(provide 'init-haskell)
;;; init-haskell.el ends here
