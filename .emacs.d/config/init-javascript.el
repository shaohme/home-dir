;;; package -- Summary
;;; Commentary:
;;; JavaScript settings
;;; Code:
(require 'company)
(require 'flycheck)
(require 'tide)
(require 'js2-mode)
(require 'js2-refactor)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defun init-javascript-mode()
  (setq flycheck-jscsrc ".jscsrc.json")
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(json-jsonlist)))
  ;; (flycheck-add-mode 'javascript-eslint 'javascript-mode)
  (add-to-list 'company-backends 'company-tern)
  (projectile-mode t)
  (tern-mode t)
  (company-mode t)
  (flycheck-mode t)
  (turn-on-auto-fill)
  (js2-refactor-mode t)
)

(add-hook 'js2-mode-hook 'init-javascript-mode)

(provide 'init-javascript)
;;; init-javascript.el ends here
