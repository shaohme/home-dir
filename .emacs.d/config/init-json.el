;;; package -- Summary
;;; Commentary:
;;; JSON development settings
;;; Code:

(require 'js)

(defun init-json-mode()
  (company-mode t)
  (flycheck-mode t)
  (local-set-key (kbd "C-c C-i") 'json-pretty-print-buffer)
  (setq js-indent-level 2)
  )

(add-hook 'json-mode-hook 'init-json-mode)


(provide 'init-json)
;;; init-json.el ends here
