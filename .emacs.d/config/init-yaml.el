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



(provide 'init-yaml)
;;; init-yaml.el ends here
