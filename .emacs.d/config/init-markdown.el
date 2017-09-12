;;; package -- Summary
;;; Commentary:
;;; Markdown development settings
;;; Code:

(defun init-mark-down()
  (auto-fill-mode t)
  (flyspell-mode t)
  (flycheck-mode t)
  (company-mode t)
  )

(add-hook 'markdown-mode-hook 'init-mark-down)


(provide 'init-markdown)
;;; init-markdown.el ends here
