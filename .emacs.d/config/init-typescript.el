;;; package -- Summary
;;; Commentary:
;;; TypeScript settings
;;; Code:
(require 'company)
(require 'flycheck)
(require 'tide)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(defun init-typescript-mode()
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(json-jsonlist)))
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (projectile-mode t)
  (company-mode t)
  (flycheck-mode t)
  (turn-on-auto-fill)
  (tide-setup)
  (eldoc-mode t)
  (tide-hl-identifier-mode t)
  (local-set-key (kbd "C-c .") 'tide-jump-to-definition)
)

(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook 'init-typescript-mode)


(provide 'init-typescript)
;;; init-typescript.el ends here
