;;; package -- Summary
;;; Commentary:
;;; JSON development settings
;;; Code:

(require 'yaml-mode)

(defun init-yaml-mode()
  (company-mode t)
  (flycheck-mode t)
  (flycheck-yamllint-setup)
  )

(add-hook 'yaml-mode-hook 'init-yaml-mode)

(provide 'init-yaml)
;;; init-yaml.el ends here
