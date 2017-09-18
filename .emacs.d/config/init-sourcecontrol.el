;;; package -- Summary
;;; Commentary:
;;; General settings for source controls
;;; Code:
(require 'git-commit)

(defun init-git-commit-mode()
  ;; (git-commit-setup)
  (turn-on-auto-fill)
  (company-mode t)
  (flyspell-mode t)
  )

(add-hook 'git-commit-mode-hook 'init-git-commit-mode)

(provide 'init-sourcecontrol)
;;; init-sourcecontrol.el ends here
