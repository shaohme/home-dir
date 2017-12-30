;;; package -- Summary
;;; Commentary:
;;; General settings for development environments
;;; Code:

(require 'gdb-mi)
(require 'helm-projectile)

(setq gdb-many-windows t
      gdb-show-main t)

;; (show-smartparens-global-mode +1)
;; (smartparens-global-mode 1)

;; when you press RET, the curly braces automatically
;; add another newline

(add-hook 'projectile-mode-hook #'helm-projectile-on)

(provide 'init-dev-common)
;;; init-dev-common.el ends here
