;;; package -- Summary
;;; Commentary:
;;; Apache settings
;;; Code:

(defun init-apache-mode()
  (local-set-key (kbd "C-c C-i") 'apache-ident-line))

(add-hook 'apache-mode-hook 'init-apache-mode)


(provide 'init-apache)
;;; init-apache.el ends here
