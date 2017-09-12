;;; package -- Summary
;;; Commentary:
;;; JavaScript settings
;;; Code:
(require 'company)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defun init-javascript-mode()
  (add-to-list 'company-backends 'company-tern)
  (tern-mode t)
  (company-mode t)
  (flycheck-mode t)
  (turn-on-auto-fill))

(add-hook 'js2-mode-hook 'init-javascript-mode)


(provide 'init-javascript)
;;; init-javascript.el ends here
