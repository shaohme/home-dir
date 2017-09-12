;;; package -- Summary
;;; Commentary:
;;; Markdown development settings
;;; Code:

(defun init-mark-down()
  (flycheck-mode t)
  (auto-fill-mode t)
  )

(add-hook 'markdown-mode-hook 'init-mark-down)


(provide 'init-markdown)
;;; init-markdown.el ends here
