;;; package -- Summary
;;; Commentary:
;;; Groovy development settings
;;; Code:


(defun init-groovy-mode()
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case t)
  (flycheck-mode t)
  (company-mode t)
  )

(add-to-list 'groovy-mode-hook 'init-groovy-mode)

(provide 'init-groovy)
;;; init-groovy.el ends here
