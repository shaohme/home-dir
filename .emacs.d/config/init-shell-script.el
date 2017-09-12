;;; package -- Summary
;;; Commentary:
;;; Shell script development settings
;;; Code:

(defun init-shell-script-mode()
  (flycheck-mode t)
  (company-mode t)
  )

(add-hook 'sh-set-shell-hook 'init-shell-script-mode)


(provide 'init-shell-script)
;;; init-shell-script.el ends here
