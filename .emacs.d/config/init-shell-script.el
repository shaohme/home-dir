;;; package -- Summary
;;; Commentary:
;;; Shell script development settings
;;; Code:

(require 'flycheck-checkbashisms)
(require 'company-shell)

(defun init-shell-script-mode()
  (add-to-list 'company-backends '(company-shell company-shell-env))
  (flycheck-mode t)
  (company-mode t)
  (flycheck-checkbashisms-setup)
  )

(add-hook 'sh-set-shell-hook 'init-shell-script-mode)


(provide 'init-shell-script)
;;; init-shell-script.el ends here
