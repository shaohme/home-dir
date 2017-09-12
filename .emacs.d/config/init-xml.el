;;; package -- Summary
;;; Commentary:
;;; XML development settings
;;; Code:

(require 'nxml-mode)

(defun nxml-pretty-format ()
  "."
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
    (nxml-mode)
    (indent-region 0 (count-lines (point-min) (point-max))))
  )


(defun init-nxml-mode()
  "."
  (setq nxml-slash-auto-complete-flag t)
  (flycheck-mode t)
  (local-set-key (kbd "C-c C-i") 'nxml-pretty-format)
  )

(add-hook 'nxml-mode-hook 'init-nxml-mode)


(provide 'init-xml)
;;; init-xml.el ends here
