;;; package -- Summary
;;; Commentary:
;;; Common C/C++ development environments settings
;;; Code:

(require 'cl-lib)
(require 'company)
;; (require 'irony)
;; (require 'company-irony)
;; (require 'company-irony-c-headers)
(require 'rtags)
(require 'company-rtags)
(require 'flycheck-rtags)
(require 'ggtags)
(require 'helm-gtags)
(require 'semantic)


(defun compile-pkg (&optional command startdir)
  "Compile a package with COMMAND.
Moving up to the parent directory containing configure.ac, if it
exists.  Start in STARTDIR if defined, else start in the current directory."
  (interactive)

  (let ((dirname)
	(dir-buffer nil))
    (setq startdir (expand-file-name (if startdir startdir ".")))
    (setq command  (if command command compile-command))

    (setq dirname (upward-find-file "configure.ac" startdir))
    (setq dirname (if dirname dirname (upward-find-file "Makefile" startdir)))

    (setq dirname (if dirname dirname (expand-file-name ".")))
    ; We've now worked out where to start. Now we need to worry about
    ; calling compile in the right directory
    (save-excursion
      (setq dir-buffer (find-file-noselect dirname))
      (set-buffer dir-buffer)
      (compile command)
      (kill-buffer dir-buffer))))

(defun upward-find-file (filename &optional startdir)
  "Move up directories until we find a certain filename FILENAME.
If we manage to find it, return the containing directory.  Else if we get to the
toplevel directory and still can't find it, return nil.  Start at STARTDIR or .  if STARTDIR not given."

  (let ((dirname (expand-file-name
		  (if startdir startdir ".")))
	(found nil) ; found is set as a flag to leave loop if we find it
	(top nil))  ; top is set when we get
		    ; to / so that we only check it once

    ; While we've neither been at the top last time nor have we found
    ; the file.
    (while (not (or found top))
      ; If we're at / set top flag.
      (if (string= (expand-file-name dirname) "/")
	  (setq top t))

      ; Check for the file
      (if (file-exists-p (expand-file-name filename dirname))
	  (setq found t)
	; If not, move up a directory
	(setq dirname (expand-file-name ".." dirname))))
    ; return statement
    (if found dirname nil)))

(defun init-c-mode()
  (make-local-variable 'company-backends)
  (setq-default c-default-style "gnu"
                c-basic-offset 4
                tab-width 4
                indent-tabs-mode nil)
  (setq  ;; RTags creates more accurate overlays.

        rtags-autostart-diagnostics t
        rtags-completions-enabled t
        rtags-spellcheck-enabled nil
        helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "\C-cg"
        helm-gtags-suggested-key-mapping t
        )
  ;; (setq-local rtags-autostart-diagnostics t
  ;;             rtags-completions-enabled t
  ;;             rtags-spellcheck-enabled nil
  ;;             )
  ;; (setq-local company-backends (delete 'company-semantic company-backends))
  ;; (setq-local company-backends (delete 'company-clang company-backends))
  ;; (setq-local company-backends (delete 'company-bbdb company-backends))
  ;; (setq-local company-backends (delete 'company-eclim company-backends))
  ;; (setq-local company-backends (delete 'company-nxml company-backends))
  ;; (setq-local company-backends (delete 'company-css
  ;; company-backends))
  ;;  (setq-local company-backends (company-irony company-rtags))

  ;; (add-to-list 'company-backends 'company-irony)
  ;; (add-to-list 'company-backends 'company-irony-c-headers);
  (ggtags-mode t)
  (helm-gtags-mode t)
  (rtags-diagnostics)
  ;; (global-semanticdb-minor-mode t)
  ;; (global-semantic-idle-scheduler-mode t)
  ;; (semantic-mode t)
  ;; (push 'company-rtags company-backends)
  (company-mode t)
  ;; (irony-mode t)
  (flycheck-mode t)
  ;; (company-irony-setup-begin-commands)
  ;; (irony-cdb-autosetup-compile-options)
  ;; (rtags-start-process-unless-running)

  (projectile-mode t)
  (flycheck-select-checker 'rtags)

  ;; (setq-local flycheck-highlighting-mode nil)
  ;; (setq-local flycheck-check-syntax-automatically nil)

  (local-set-key (kbd "C-c ;") 'iedit-mode)
  (local-set-key (kbd "C-c C-j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c C-s") 'semantic-ia-show-summary)
  (local-set-key (kbd "C-x C-m") 'cmake-ide-run-cmake)
  (local-set-key (kbd "C-c .") 'rtags-find-symbol-at-point)
  (local-set-key (kbd "C-c ,") 'rtags-find-references-at-point)
  ;; (local-set-key (kbd "TAB") 'company-complete-selection)
  (local-set-key (kbd "M-TAB") 'company-complete)
  (local-set-key (kbd "C-c C-c") (lambda ()
                                   (interactive)
                                   (setq-local compilation-read-command nil)
                                   (call-interactively 'compile-pkg)))
  ;; (rtags-enable-standard-keybindings)
  (hs-minor-mode t))


(add-hook 'c-mode-hook 'init-c-mode)
(add-hook 'c++-mode-hook 'init-c-mode)

(add-hook 'c-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'c++-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)


(defun init-cmake-mode()
  (company-mode t)
  )

(add-hook 'cmake-mode-hook 'init-cmake-mode)


;; (defun reinit-after-save()
;;   (irony-server-kill)
;;   )

;; (add-hook 'after-save-hook 'reinit-after-save)

(provide 'init-c-common)
;;; init-c-common.el ends here
