;;; package -- Summary
;;; Commentary:
;;; JSON development settings
;;; Code:

(defun init-json-mode()
  (company-mode t)
  (flycheck-mode t)
  )

(add-hook 'json-mode-hook 'init-json-mode)


(provide 'init-json)
;;; init-json.el ends here
