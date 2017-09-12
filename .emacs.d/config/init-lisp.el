;;; package -- Summary
;;; Commentary:
;;; Lisp development settings
;;; Code:

(defun custom-lisp-mode-hook()
  ;; (setq
   ;; flycheck-emacs-lisp-initialize-packages 'auto
      ;; )
      ;; )
  (flycheck-mode t)
  ;; (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (auto-fill-mode t)
  (company-mode t)
  ;; (ac-emacs-lisp-mode-setup)
  ;; (auto-complete-mode t)
  ;; (require 'smartparens-config)
  ;; (setq sp-base-key-bindings 'paredit
  ;; 		sp-autoskip-closing-pair 'always
  ;; 		sp-hybrid-kill-entire-symbol nil)
  ;; (sp-use-paredit-bindings)
  ;; (show-smartparens-mode +1)
  ;; (smartparens-mode t)
  )

(add-hook 'lisp-mode-hook 'custom-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'custom-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


(provide 'init-lisp)
;;; init-lisp.el ends here
