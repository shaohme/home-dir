;;; package -- Summary
;;; Commentary:
;;; Git settings
;;; Code:

(defun init-git-mode()
  (flyspell-mode 1)
  (turn-on-auto-fill))

(add-hook 'git-commit-mode-hook 'init-git-mode)


(provide 'init-git)
;;; init-git.el ends here
