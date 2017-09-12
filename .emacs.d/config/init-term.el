;;; package -- Summary
;;; Commentary:
;;; Terminal settings
;;; Code:

(require 'yasnippet)

(defun init-term-mode()
  (setq yas-dont-activate-functions t)
  )

(add-hook 'term-mode-hook 'init-term-mode)


(provide 'init-term)
;;; init-term.el ends here
