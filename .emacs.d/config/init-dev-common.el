;;; package -- Summary
;;; Commentary:
;;; General settings for development environments
;;; Code:

(require 'gdb-mi)
(require 'smartparens-config)

(setq gdb-many-windows t
      gdb-show-main t)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; when you press RET, the curly braces automatically
;; add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))



(provide 'init-dev-common)
;;; init-dev-common.el ends here
