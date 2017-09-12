;;; package -- Summary
;;; Commentary:
;;; Python settings
;;; Code:

(require 'python)
(require 'jedi-core)
(require 'gud)
(require 'company)

(setq
      ;; company-emacs-eclim-ignore-case t
      python-indent-guess-indent-offset nil
      python-indent-offset 4
      python-environment-directory "~/.virtualenvs"
      python-shell-interpreter "python3"
      ;; python-python-command (concat python-shell-interpreter " -i")
      gud-pdb-command-name (concat python-shell-interpreter " -m pdb")
      jedi:environment-root "default"
      jedi:server-command (list (concat python-environment-directory "/" jedi:environment-root "/bin/jediepcserver"))
      jedi:complete-on-dot t
      )

;;; Python mode
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(defun init-python-mode()
  (jedi:setup)
  (add-to-list 'company-backends 'company-jedi)
  (company-mode t)
  ;; (company-quickhelp-mode t)
  (flycheck-mode t)
  ;; (eldoc-mode t)
  (projectile-mode t))

(add-hook 'python-mode-hook 'init-python-mode)

(provide 'init-python)
;;; init-python.el ends here
