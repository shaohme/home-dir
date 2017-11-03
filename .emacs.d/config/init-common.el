;;; package -- Summary
;;; Commentary:
;;; General settings
;;; Code:

(require 'flyspell)
(require 'company)
(require 'recentf)
(require 'browse-url)
(require 'eww)
(require 'volatile-highlights)
(require 'xkcd)
(require 'color-theme-sanityinc-solarized)

(load-theme 'sanityinc-solarized-light t)

(setq-default frame-title-format '("%b [%m] %F")
;;; Disable tab-indentation, because it screws with web-mode offset's
              indent-tabs-mode nil
              save-place 1
              flycheck-emacs-lisp-load-path load-path)

(setq user-full-name "Martin Kjær Jørgensen"
      default-frame-alist '((font . "monospace 12"))
      browse-url-browser-function 'browse-url-firefox
      browse-url-new-window-flag t
      browse-url-firefox-new-window-is-tab t
      inhibit-startup-message t
      eww-download-directory "~/dwl/"
      password-cache t
      password-cache-expiry 86400
      ispell-program-name "hunspell"
      ispell-dictionary "en_US"
      recentf-max-menu-items 25
      flyspell-issue-message-flag nil
      uniquify-buffer-name-style 'forward
      current-language-environment "English"
      backup-directory-alist `(("." . "~/.saves"))
      backup-by-copying t
      backup-inhibited t
      auto-save-default nil
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      visible-cursor nil
      initial-major-mode 'text-mode
      initial-scratch-message nil
      x-underline-at-descent-line t
      scroll-step 1
      company-idle-delay 0.2
      company-minimum-prefix-length 2
      ;; aligns annotation to the right hand side
      company-tooltip-align-annotations t
      )

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-c C-k") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x t") 'beginning-of-buffer)
(global-set-key (kbd "C-x e") 'end-of-buffer)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "C-x C-b") nil)
(global-set-key (kbd "<f8>") 'switch-dictionary)
(global-set-key (kbd "M-<f12>") 'toggle-solarized)
(global-set-key (kbd "M-<backspace>") 'backward-delete-word)
;; (global-set-key (kbd "C-M-i") 'company-complete)
(global-set-key (kbd "C-<tab>") 'company-complete)

(global-unset-key (kbd "C-x c"))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
;; fix for zsh strange chars in shell
(setenv "ESHELL" (expand-file-name "~/bin/eshell"))
(mouse-wheel-mode 1)
(blink-cursor-mode 0)
(menu-bar-mode -1)
(tool-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(volatile-highlights-mode t)
(global-anzu-mode)
(recentf-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "en_US") "da_DK" "en_US")))
	(ispell-change-dictionary change)
	;; Dont check whole buffer as it can be of several languages, and large
	;; (if (bound-and-true-p flyspell-mode)
	;; 	(flyspell-buffer))
	(message "Dictionary switched from %s to %s" dic change)))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))


(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-selection)
     (define-key company-active-map [tab] 'company-complete-selection)))

(provide 'init-common)
;;; init-common.el ends here
