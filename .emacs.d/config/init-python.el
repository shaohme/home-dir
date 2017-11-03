;;; package -- Summary
;;; Commentary:
;;; Python settings
;;; Code:

(require 'python)
(require 'jedi-core)
(require 'gud)
(require 'company)
(require 'projectile)
(require 'flycheck)
(require 'elpy)
(require 'flymake)

(setq
 ;; company-emacs-eclim-ignore-case t
 flycheck-python-pycompile-executable "python3"
 flycheck-json-python-json-executable "python3"
 python-indent-guess-indent-offset nil
 python-indent-offset 4
 python-environment-directory "~/.virtualenvs"
 python-shell-interpreter "python3"
 ;; python-python-command (concat python-shell-interpreter " -i")
 gud-pdb-command-name (concat python-shell-interpreter " -m pdb")
 jedi:environment-root "default"
 jedi:server-command (list (concat python-environment-directory "/" jedi:environment-root "/bin/jediepcserver"))
 jedi:complete-on-dot t
 elpy-rpc-backend "jedi"
 elpy-rpc-python-command "python3"
 flymake-error-bitmap (quote (exclamation-mark flycheck-error))
 flymake-warning-bitmap (quote (question-mark flycheck-warning))
 )

;;; Python mode
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(defun init-python-mode()
  (jedi:setup)
  (add-to-list 'company-backends 'company-jedi)
  (company-mode t)
  ;; (company-quickhelp-mode t)
  (flycheck-mode t)
  ;; (flycheck-pycheckers-setup)
  ;; (elpy-mode t)
  (eldoc-mode t)
  (projectile-mode t))

(add-hook 'python-mode-hook 'init-python-mode)

(defun init-elpy-mode()
  (local-set-key (kbd "C-c C-k") 'comment-region)
  )

(add-hook 'elpy-mode-hook 'init-elpy-mode)

(provide 'init-python)
;;; init-python.el ends here
