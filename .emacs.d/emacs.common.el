;;; package --- common init file
;;; Commentary:
;;; Code:

(require 'local-common)

;; Load theme first
;; (ensure-package 'color-theme-sanityinc-tomorrow)
;; (require 'color-theme-sanityinc-tomorrow)
(ensure-package 'zenburn-theme)
(require 'zenburn-theme)

(load-theme 'zenburn t)

(require 'ispell)
(require 'recentf)
(require 'eww)
(require 'flyspell)
(require 'browse-url)
(require 'gud)
(require 'gdb-mi)
(require 'tramp)
(require 'smerge-mode)
(ensure-package 'popup)
(require 'popup)
(ensure-package 'async)
(require 'async)
(require 'ldap-mode)
(require 'mouse)
(require 'mwheel)


(setq-default frame-title-format '((:eval (if (buffer-file-name)
		                                      (abbreviate-file-name (buffer-file-name))
                                            "%b")))
 ;; frame-title-format '("%b [%m] %F")
;;; Disable tab-indentation, because it screws with web-mode offset's
              indent-tabs-mode nil
              tab-width 4
              save-place 1
              flycheck-emacs-lisp-load-path load-path
              major-mode 'text-mode
              )
(setq user-full-name "Martin Kjær Jørgensen"
      inhibit-startup-message t
      password-cache t
      password-cache-expiry 86400
      ;; uniquify-buffer-name-style 'forward
      ; Show path if names are same
      uniquify-buffer-name-style 'post-forward-angle-brackets
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
      ;; x-underline-at-descent-line t
      scroll-step 1
      dired-listing-switches "-lah"
      directory-free-space-args "-Pkh"
      recentf-max-menu-items 25
      eww-download-directory "~/dwl/"
      flyspell-issue-message-flag nil
      browse-url-browser-function 'browse-url-firefox
      browse-url-new-window-flag t
      browse-url-firefox-new-window-is-tab t
      tramp-default-method "ssh"
      gdb-many-windows t
      gdb-show-main t
      buffer-file-coding-system 'utf-8 ; utf-8-unix
      save-buffer-coding-system 'utf-8-unix ; nil
      process-coding-system-alist (cons '("grep" utf-8 . utf-8) process-coding-system-alist)
      ;; adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      ;; adaptive-fill-first-line-regexp "^* *$"
      ;; sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      ;; sentence-end-double-space nil
      )


(ensure-package 'realgud)
(require 'realgud)


(defalias 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8-unix)
(set-locale-environment "en_US.UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8) ; included by set-selection-coding-system
(set-keyboard-coding-system 'utf-8) ; configured by prefer-coding-system
(set-terminal-coding-system 'utf-8) ; configured by prefer-coding-system
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
;; fix for zsh strange chars in shell
;; (setenv "ESHELL" (expand-file-name "~/bin/eshell"))

(unless window-system
  (xterm-mouse-mode t)
  (mouse-wheel-mode t))

(blink-cursor-mode 0)
(menu-bar-mode -1)
(tool-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(recentf-mode 1)
(transient-mark-mode 1)
(size-indication-mode 1)


;; save recentf every X sec because we're running in daemon mode
;; (run-at-time (current-time) 60 'recentf-save-list)

;; (global-set-key (kbd "C-c C-k") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x t") 'beginning-of-buffer)
(global-set-key (kbd "C-x e") 'end-of-buffer)
(global-set-key (kbd "C-x C-b") nil)
(global-set-key (kbd "M-<backspace>") 'backward-delete-word)
(global-set-key (kbd "<f8>") 'switch-dictionary)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'comment-dwim)
(global-unset-key (kbd "C-x c"))

(add-to-list 'compilation-finish-functions 'notify-compilation-result)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook #'goto-address-mode)

(require 'saveplace)
;;; should we use save-place-local-mode instead?
(add-hook 'after-init-hook 'save-place-mode)


(ensure-package 'volatile-highlights)
(require 'volatile-highlights)

(volatile-highlights-mode t)

(ensure-package 'undo-tree)
(require 'undo-tree)

(global-undo-tree-mode 1)

(ensure-package 'xclip)
(require 'xclip)

(xclip-mode 1)



(ensure-package 'ag)
(require 'ag)

(setq ag-reuse-buffers t
      ag-reuse-window t)

(ensure-package 'wgrep)
(require 'wgrep)
(ensure-package 'wgrep-ag)
(require 'wgrep-ag)


(setq wgrep-enable-key "e"
      wgrep-auto-save-buffer t
      wgrep-change-readonly-file t)


;; Colorize output in the *compilation* buffer (for example when running test via elpy)
;; https://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


(ensure-package 'presentation)
(require 'presentation)



(require 'paren)
(setq show-paren-style 'mixed
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

(add-hook 'after-init-hook 'show-paren-mode)



;;; Makefile mode

(defun init-makefile-mode()
  (setq indent-tabs-mode t)
  )

(add-hook 'makefile-mode-hook 'init-makefile-mode)
(add-hook 'eww-mode-hook 'auto-fill-mode)

;;; Which-key mode

(ensure-package 'which-key)
(require 'which-key)
(which-key-mode t)




;;; Ivy
(ensure-package 'amx)
(require 'amx)
(ensure-package 'ivy)
(require 'ivy)
(ensure-package 'counsel)
(require 'counsel)
(ensure-package 'swiper)
(require 'swiper)

(ivy-mode 1)
;; used to add history to minibuffer selections like M-x
;; instead of smex which produces compile errors on install
(amx-mode 1)

(setq ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) ")

(global-set-key (kbd "C-s") 'swiper-isearch)

;;; Helm mode

;; (ensure-package 'helm)
;; (require 'helm)
;; (require 'helm-config)
;; (require 'helm-grep)
;; (require 'helm-utils)
;; (require 'helm-man)
;; (require 'helm-help)
;; (require 'helm-files)
;; (ensure-package 'helm-swoop)
;; (require 'helm-swoop)
;; (ensure-package 'helm-tramp)
;; (require 'helm-tramp)
;; (ensure-package 'helm-pass)
;; (require 'helm-pass)
;; (ensure-package 'helm-ls-git)
;; (require 'helm-ls-git)
;; (ensure-package 'helm-ag)
;; (require 'helm-ag)
;; (ensure-package 'helm-flycheck)
;; (require 'helm-flycheck)


;; (setq ;; mouse-sel-retain-highlight t
;;       ;; open helm buffer inside current window, not occupy whole other window
;;       helm-split-window-inside-p t
;;       ;; scroll 4 lines other window using M-<next>/M-<prior>
;;       helm-scroll-amount 4
;;       ;; limit the number of displayed canidates
;;       helm-candidate-number-limit 500
;;       ;; move to end or beginning of source when reaching top or bottom of source.
;;       helm-move-to-line-cycle-in-source t
;;       ;; fuzzy matching buffer names when non-nil
;;       ;; useful in helm-mini that lists buffers
;;       helm-autoresize-max-height 40
;;       helm-autoresize-min-height 20
;;       helm-ff-search-library-in-sexp t
;;       helm-ff-file-name-history-use-recentf t
;;       helm-multi-swoop-edit-save t
;;       ;; If this value is t, split window inside the current window
;;       helm-swoop-split-with-multiple-windows t
;;       ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
;;       helm-swoop-split-direction 'split-window-vertically
;;       )

;; (global-set-key (kbd "M-x") #'helm-M-x)
;; (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;; (global-set-key (kbd "C-c h") #'helm-command-prefix)
;; (global-set-key (kbd "M-y") #'helm-show-kill-ring)
;; (global-set-key (kbd "C-x b") #'helm-mini)
;; (global-set-key (kbd "C-c s") #'helm-tramp)

;; (define-key global-map [remap find-file] 'helm-find-files)
;; (define-key global-map [remap occur] 'helm-occur)
;; (define-key global-map [remap list-buffers] 'helm-buffers-list)
;; (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
;; (define-key global-map [remap execute-extended-command] 'helm-M-x)

;; (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
;; make TAB works in terminal
;; (define-key helm-map (kbd "C-i") #'helm-execute-persistent-action)
;; list actions ; using C-z
;; (define-key helm-map (kbd "C-z") #'helm-select-action)
;; (define-key helm-grep-mode-map (kbd "<return>") #'helm-grep-mode-jump-other-window)
;; (define-key helm-grep-mode-map (kbd "p") #'helm-grep-mode-jump-other-window-forward)
;; (define-key helm-grep-mode-map (kbd "n") #'helm-grep-mode-jump-other-window-backward)
;; (define-key helm-swoop-map (kbd "M-i") #'helm-multi-swoop-all-from-helm-swoop)
;; (define-key isearch-mode-map (kbd "M-i") #'helm-swoop-from-isearch)

;; (defun spacemacs//helm-hide-minibuffer-maybe ()
;;   "Hide minibuffer in Helm session if we use the header line as input field."
;;   (when (with-helm-buffer helm-echo-input-in-header-line)
;;     (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
;;       (overlay-put ov 'window (selected-window))
;;       (overlay-put ov 'face
;;                    (let ((bg-color (face-background 'default nil)))
;;                      `(:background ,bg-color :foreground ,bg-color)))
;;       (setq-local cursor-type nil))))


;; (add-hook 'helm-minibuffer-set-up-hook 'spacemacs//helm-hide-minibuffer-maybe)
;; (add-hook 'helm-mode-hook #'helm-autoresize-mode)
;; (add-hook 'helm-goto-line-before-hook #'helm-save-current-pos-to-mark-ring)


;; (helm-mode t)
;; (helm-autoresize-mode t)


;; (define-key global-map [remap find-file] 'helm-find-files)
;; (define-key global-map [remap occur] 'helm-occur)
;; (define-key global-map [remap list-buffers] 'helm-buffers-list)
;; (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
;; (define-key global-map [remap execute-extended-command] 'helm-M-x)
;; (define-key global-map [remap apropos-command] 'helm-apropos)

;; Gnus and mail
(ensure-package 'emojify)
(require 'emojify)

(ensure-package 'bbdb)
(require 'bbdb)

(require 'gnus)
(require 'gnus-art)
(require 'gnus-sum)
(require 'gnus-topic)
(require 'gnus-score)
(require 'gnus-salt)
(require 'gnus-topic)
(require 'gnus-async)
(require 'gnus-agent)
(require 'message)
(require 'nndraft)
(require 'nnfolder)
(require 'smtpmail)
(require 'mml)

(setq gnus-asynchronous t
      gnus-directory "~/.emacs.d/gnus/News/"
      gnus-read-newsrc-file nil
      gnus-save-newsrc-file nil
      gnus-cache-directory (concat gnus-directory "cache/")
      gnus-cache-active-file (concat gnus-directory "cache/active") ; no slash! this is a file, not a directory!
      gnus-article-save-directory (concat gnus-directory "save/")
      gnus-kill-files-directory (concat gnus-directory "killfiles/")
      gnus-message-archive-group "Sent"
      gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed")
      ;; gnus-mime-view-all-parts t
      ;; this so ido doesnt interfere with gnus own ido completion
      ;; gnus-completing-read-function 'gnus-ido-completing-read
      ;; dont ask for reading newsrc
      gnus-always-read-dribble-file t
      gnus-novice-user nil
      gnus-interactive-exit nil
      gnus-gcc-mark-as-read t
      ;; gnus-summary-line-format "%U%R%d%z%I%(%[%4L: %-23,23f%]%) %s\n"
      ;; gnus-summary-line-format "%U%R%O %&user-date; %I%(%[%n]%): %s\n"
      gnus-summary-line-format "%U%R%z %&user-date; %I%(%[%4L: %-20,20n%]%) %S\n"
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      gnus-group-line-format "%M%S%8y: %G\n"
      gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M:%S"))
      gnus-thread-sort-functions (quote (gnus-thread-sort-by-most-recent-date))
      gnus-sort-gathered-threads-function (quote gnus-thread-sort-by-date)
      gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-indent " "
      ;;      gnus-sum-thread-tree-leaf-with-other "├► "
      gnus-sum-thread-tree-leaf-with-other "|-> "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-leaf "'->"
      ;;      gnus-sum-thread-tree-single-leaf "╰► "
      ;;      gnus-sum-thread-tree-vertical "│"
      gnus-sum-thread-tree-vertical "|"
      ;; Show pretty widecars
      gnus-use-correct-string-widths t
      gnus-tree-minimize-window nil
      gnus-treat-strip-leading-blank-lines t
      gnus-treat-strip-multiple-blank-lines t
      gnus-treat-strip-trailing-blank-lines t
      gnus-treat-strip-cr t
      gnus-treat-hide-citation-maybe t
      gnus-treat-emphasize t
      gnus-treat-display-smileys t
      gnus-topic-display-empty-topics nil
      gnus-topic-line-format "%i[ %A: %(%{%n%}%) ]%v"
      gnus-agent t
      gnus-agent-directory (concat gnus-directory "agent/")
      ;; gnus-agent-prompt-send-queue nil
      ;; Send mail immediately
      ;; gnus-agent-queue-mail nil
      ;; gnus-agent-synchronize-flags t
      ;; gnus-agent-cache t
      gnus-ignored-from-addresses "Martin Kjær Jørgensen"
      user-mail-address "mkj@gotu.dk"
      mail-user-agent 'gnus-user-agent
      message-directory "~/.emacs.d/gnus/Mail/"
      message-fill-column 78
      message-forward-as-mime nil
      message-wash-forwarded-subjects t
      message-sendmail-envelope-from 'header
      message-alternative-emails "\\(mkj?\\|shaoh\\)@\\(\\(gotu\\)\\.dk\\)"
      message-dont-reply-to-names "\\(mkj?\\|shaoh\\)@\\(\\(gotu\\)\\.dk\\)"
      message-kill-buffer-on-exit t
      message-mail-alias-type nil
      message-send-mail-partially-limit nil
      message-send-mail-function 'message-smtpmail-send-it
      message-setup-hook (quote (message-check-recipients))
      message-subscribed-address-functions (quote (gnus-find-subscribed-addresses))
      message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format "On %a, %b %d %Y, %f wrote:\n"
      ;; put cursor on top of reply
      message-cite-reply-position 'above
      ;; dont autosave
      message-auto-save-directory nil
      smime-CA-directory "/etc/ssl/certs/"
      smime-CA-file "/etc/ssl/certs/ca-certificates.crt"
      smime-certificate-directory "~/.certs/"
      smime-crl-check "-crl_check"
      send-mail-function 'smtpmail-send-it
      ;; render html mails without dark background
      ;; seems to work with gnus-w3m
      mm-text-html-renderer 'gnus-w3m
      ;; dont ask for confirm when open links
      mm-w3m-safe-url-regexp nil
      mm-enable-external t
      mm-default-directory "~/dwl"
      mm-tmp-directory "~/tmp"
      mm-verify-option 'always
      mm-decrypt-option 'always
      gnus-extra-headers (quote (To Cc Newsgroups))
      nnmail-crosspost nil
      nnmail-extra-headers gnus-extra-headers
      nndraft-directory (concat message-directory "drafts/")
      nnfolder-directory (concat message-directory "archive/")
      nnfolder-active-file (concat message-directory "archive")
      smtpmail-debug-info t
      bbdb-file (substitute-in-file-name "$HOME/db/bbdb")
      ;; bbdb-pop-up-window-size 0.15
      ;; bbdb-mua-pop-up-window-size 0.15
      bbdb-mua-update-interactive-p '(query . create)
      ;; bbdb-mua-auto-update-p 'query
      bbdb-mua-auto-update-p 'create
      bbdb-message-all-addresses t)


(define-key gnus-summary-mode-map (kbd ";") #'bbdb-mua-edit-field)

(add-hook 'gnus-summary-mode-hook 'emojify-mode)
(add-hook 'mail-mode-hook 'footnote-mode)
(add-hook 'mail-mode-hook #'turn-on-flyspell)
(add-hook 'mail-mode-hook 'turn-on-auto-fill)
(add-hook 'message-mode-hook 'footnote-mode)
(add-hook 'message-mode-hook #'turn-on-flyspell)
(add-hook 'message-mode-hook 'turn-on-auto-fill)
(add-hook 'message-mode-hook 'emojify-mode)


;; unneeded or find alternative
;; (add-hook 'message-send-hook
;;           (lambda ()
;;             (message-add-header "X-PGP-Key: fp=\"730C C366 E9E2 C833
;;   E62F B412  0D20 8662 8A3D 849A\"; id=\"0x8A3D849A\";
;;   get=<http://www.gotu.dk/8a3d849a.asc>; get=<hkp://pgp.mit.edu/pks/lookup?search=0x0D2086628A3D849A&op=get>; get=<https://keyserver.pgp.com/vkd/DownloadKey.event?keyid=0x0D2086628A3D849A>;")))

(defun after-init-flyspell-mode()
  ;;; Check the whole buffer after flyspell loads, so to see current
  ;;; spelling errors
  (flyspell-buffer)
  )

(add-hook 'flyspell-mode-hook #'after-init-flyspell-mode)


(bbdb-initialize 'gnus 'message 'sendmail)
(bbdb-mua-auto-update-init 'message)

(add-hook 'prog-mode-hook #'auto-revert-mode)

(ensure-package 'pass)
(require 'pass)


(ensure-package 'popwin)
(require 'popwin)

(setq popwin:special-display-config nil)

(push '("*Backtrace*"
        :dedicated t :position bottom :stick t :noselect nil :height 0.33)
      popwin:special-display-config)
;; (push '("*Compilation*"
;;         :dedicated t :position bottom :stick t :noselect t   :height 0.2)
;;       popwin:special-display-config)
(push '("*compilation*"
        :dedicated t :position bottom :stick t :noselect t   :height 0.2)
      popwin:special-display-config)
(push '("*Compile-Log*"
        :dedicated t :position bottom :stick t :noselect t   :height 0.33)
      popwin:special-display-config)
(push '("*Help*"
        :dedicated t :position bottom :stick t :noselect nil :height 0.33)
      popwin:special-display-config)
(push '("*Shell Command Output*"
        :dedicated t :position bottom :stick t :noselect nil :height 0.33)
      popwin:special-display-config)
(push '("*undo-tree*"
        :dedicated t :position bottom :stick t :noselect nil :height 0.33)
      popwin:special-display-config)
(push '("*Warnings*"
        :dedicated t :position bottom :stick t :noselect nil :height 0.33)
      popwin:special-display-config)
(push '("^\\*Man .*\\*$"
        :regexp t    :position bottom :stick t :noselect nil :height 0.33)
      popwin:special-display-config)
(push '(compilation-mode :noselect t :stick t :height 0.2) popwin:special-display-config)

(popwin-mode t)


(ensure-package 'projectile)
(require 'projectile)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq projectile-completion-system 'ivy
      projectile-globally-ignored-directories
      (append '(
                ".git"
                ".svn"
                "bin"
                "out"
                "repl"
                "target"
                "venv"
                "build*"
                )
              projectile-globally-ignored-directories))
(add-hook 'after-init-hook 'projectile-mode)

(ensure-package 'counsel-projectile)
(require 'counsel-projectile)
;; (ensure-package 'helm-projectile)
;; (require 'helm-projectile)

;; (add-hook 'projectile-mode-hook #'helm-projectile-on)

;; dont add counsel-projectile to projectile-mode-hook.
;; lisp max depth errors occurs
(add-hook 'after-init-hook 'counsel-projectile-mode)


(ensure-package 'company)
(require 'company)
(require 'company-dabbrev)
(require 'company-dabbrev-code)
(require 'company-ispell)

(setq company-idle-delay 0.4
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-dabbrev-downcase nil)

(global-set-key (kbd "<C-M-i>") #'company-complete)
(define-key company-active-map (kbd "<tab>") #'company-complete-selection)

(add-hook 'after-init-hook 'global-company-mode)


(ensure-package 'flycheck)
(require 'flycheck)

;; Wait idle seconds before running flycheck
(setq flycheck-idle-change-delay 2
      ;; jump to next error instead of warning or info
      flycheck-navigation-minimum-level 'error)


(ensure-package 'systemd)
(require 'systemd)

(ensure-package 'smartparens)
(require 'smartparens)
(require 'smartparens-config)

;;; Common development

(ensure-package 'diff-hl)
(require 'diff-hl)

(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)

(ensure-package 'yasnippet)
(require 'yasnippet)
(require 'term)

(setq yas-dont-activate-functions t)
(yas-global-mode 1)
(yas-load-directory (expand-file-name "snippets" user-emacs-directory))

(ensure-package 'rainbow-mode)
(require 'rainbow-mode)

(ensure-package 'rainbow-delimiters)
(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


(ensure-package 'impatient-mode)
(require 'impatient-mode)

(setq httpd-host (quote local)
      httpd-port 8218)

(ensure-package 'flymd)
(require 'flymd)


;;; CMake mode

(ensure-package 'cmake-ide)
(require 'cmake-ide)

(ensure-package 'cmake-font-lock)
(require 'cmake-font-lock)
(add-hook 'cmake-mode-hook #'cmake-font-lock-activate)

(ensure-package 'cmake-mode)
(require 'cmake-mode)


;; Git
(ensure-package 'magit)
(require 'magit)

(add-hook 'git-commit-mode-hook #'turn-on-flyspell)
(add-hook 'git-commit-mode-hook 'turn-on-auto-fill)



(ensure-package 'gitattributes-mode)
(require 'gitattributes-mode)

(ensure-package 'gitconfig-mode)
(require 'gitconfig-mode)

(ensure-package 'gitignore-mode)
(require 'gitignore-mode)





;;; C/C++ mode

(require 'cc-mode)
(ensure-package 'rtags)
(require 'rtags)
(ensure-package 'company-irony)
(require 'company-irony)
(ensure-package 'company-irony-c-headers)
(require 'company-irony-c-headers)
(ensure-package 'irony)
(require 'irony)
(ensure-package 'flycheck-irony)
(require 'flycheck-irony)
(ensure-package 'modern-cpp-font-lock)
(require 'modern-cpp-font-lock)

(defun init-cc-mode()
  (set (make-local-variable 'company-backends)
       '((company-irony-c-headers company-irony company-dabbrev-code)
         company-capf company-files))

  ;; (make-local-variable 'company-backends)
  ;; (setq company-backends (delete 'company-semantic company-backends))
  ;; (setq company-backends (delete 'company-clang company-backends))
  ;; (setq company-backends (delete 'company-capf company-backends))
  ;; (setq company-backends (delete 'company-etags company-backends))
  (setq-local c-default-style "stroustrup")
  (setq-local c-basic-offset 4)
  (setq-local tab-width 4)
  (setq-local fill-column 80)
  (setq-local indent-tabs-mode nil)
  (c-set-offset 'innamespace 0)
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET"))))
  (define-key c-mode-base-map (kbd "C-x C-m") #'cmake-ide-run-cmake)
  (define-key c-mode-base-map (kbd "C-c .") #'rtags-find-symbol-at-point)
  (define-key c-mode-base-map (kbd "C-c ,") #'rtags-find-references-at-point)
  (define-key c-mode-base-map (kbd "C-c C-c") #'cmake-ide-compile)
  (define-key c-mode-base-map (kbd "C-c C-k") #'comment-or-uncomment-region)
  )

(add-hook 'c-mode-common-hook #'smartparens-mode)
(add-hook 'c-mode-common-hook #'flycheck-mode)
(add-hook 'c-mode-common-hook #'hs-minor-mode)
(add-hook 'c-mode-common-hook #'auto-fill-mode)
(add-hook 'c-mode-hook #'init-cc-mode)
(add-hook 'c++-mode-hook #'init-cc-mode)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
(add-hook 'c++-mode-hook #'irony-mode)
(add-hook 'c++-mode-hook #'flycheck-irony-setup)
(add-hook 'c++-mode-hook #'cmake-ide-setup)
(add-hook 'c++-mode-hook #'irony-cdb-autosetup-compile-options)


;;; Org mode

(ensure-package 'org)
(require 'org)
(ensure-package 'org-journal)
(require 'org-journal)

(setq org-journal-dir "~/org/journal/")


;;; Python mode

(ensure-package 'elpy)
(require 'elpy)
(elpy-enable)

;; Remove flymake and use flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Set timeout for backend rpc
(setq elpy-rpc-timeout 3
      elpy-rpc-python-command "python3"
      elpy-rpc-virtualenv-path (expand-file-name "~/.virtualenvs/elpyrpc3")
      ;; set this to nil to avoid "Overlapping strings detected"
      ;; errors. according to elpy devs its an python.el issue.
      elpy-eldoc-show-current-function nil)

;; do not try to guess the indent offset
;; Avoid this message: "Can’t guess python-indent-offset, using defaults: 4"
;; http://stackoverflow.com/questions/18778894/emacs-24-3-python-cant-guess-python-indent-offset-using-defaults-4
(setq python-indent-guess-indent-offset nil)

;; enable newline-and-indent on return
;; (define-key global-map (kbd "RET") 'newline-and-indent)

;; navigate between Flycheck errors (I'm not using Flymake)
(define-key elpy-mode-map (kbd "C-c C-n") 'flycheck-next-error)
(define-key elpy-mode-map (kbd "C-c C-p") 'flycheck-previous-error)
;; disable the old ones
(define-key elpy-mode-map (kbd "C-c n") nil)
(define-key elpy-mode-map (kbd "C-c p") nil)

;; disable find-file-in-project because of helm
(define-key elpy-mode-map (kbd "C-c C-f") nil)

;; disable elpy-rgrep-symbol of helm
(define-key elpy-mode-map (kbd "C-c C-s") nil)

;; disable this to be used by avy
(define-key elpy-mode-map (kbd "C-c C-c") nil)
(define-key python-mode-map (kbd "C-c C-c") nil)

(define-key elpy-mode-map (kbd "M-q") 'python-fill-paragraph)
(define-key elpy-mode-map (kbd "C-c C-k") #'comment-dwim)
(define-key elpy-mode-map (kbd "C-c C-i") 'elpy-format-code)

;; https://masteringemacs.org/article/compiling-running-scripts-emacs
(defun python--add-debug-highlight ()
  "Adds a highlighter for '# DEBUG #' string"
  (highlight-lines-matching-regexp "# DEBUG #\\s-*$" 'hi-red-b))
(add-hook 'python-mode-hook 'python--add-debug-highlight)

(defun init-elpy-mode()
  ;; (set (make-local-variable 'company-backends)
  ;;      '((elpy-company-backend company-dabbrev-code) company-capf
  ;;      company-files))

  ;; use elpy primarily and append other backends to it
  (set (make-local-variable 'company-backends)
       '((elpy-company-backend :with company-dabbrev-code :with company-files)))

  ;; this should sort most important backend (elpy) first in
  ;; candidates followed by others candidates
  (setq-local company-transformers '(company-sort-by-backend-importance))

  (setq ;; flycheck-check-syntax-automatically '(save idle-change new-line)
        company-minimum-prefix-length 3

        ;; disable flake8 as it produces annoying E902 errors when
        ;; python code contains syntax errors, like empty "if" statements
        flycheck-disabled-checkers '(python-flake8)

        ;; show quick-access numbers for the first ten candidates (M-<number>
        ;; selects the specific option)
        ;; company-show-numbers t

        ;; all characters from `company-auto-complete-cha2rs' trigger insertion
        ;; of the selected completion candidate
        company-auto-complete nil

        company-auto-complete-chars '(?\( ?\) ?.)

        ;; align annotations to the right tooltip border
        company-tooltip-align-annotations t
        )
  )

;; https://emacs.stackexchange.com/a/12403
;; show private methods/attributes at the end when suggesting
;; (defun company-transform-python (candidates)
;;   (let ((deleted))
;;     (mapcar #'(lambda (c)
;;          (if (or (string-prefix-p "__" c) (string-prefix-p ".__" c))
;;             (progn
;;               (add-to-list 'deleted c)
;;               (setq candidates (delete c candidates)))))
;;             candidates)
;;     (append candidates (nreverse deleted))
;;     ))
;; (append company-transformers '(company-transform-python))

(add-hook 'elpy-mode-hook #'init-elpy-mode)

(pyvenv-workon "default")

(require 'pip-requirements)


;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; (add-hook 'python-mode-hook #'init-python-mode)
;; (add-hook 'python-mode-hook #'after-init-python-mode)


;;; Apache mode

(ensure-package 'apache-mode)
(require 'apache-mode)


;;; SQL mode

(require 'sql)
(add-hook 'sql-mode-hook 'toggle-truncate-lines)
(add-hook 'sql-mode-hook 'flyspell-prog-mode)


;;; CSS mode
(require 'css-mode)

(add-to-list 'auto-mode-alist '("\\.css?\\'" . css-mode))
(add-hook 'css-mode-hook #'flycheck-mode)
(add-hook 'css-mode-hook #'rainbow-mode)


;;; Sass mode
(ensure-package 'sass-mode)
(require 'sass-mode)

(add-hook 'sass-mode-hook #'flycheck-mode)
(add-hook 'sass-mode-hook #'rainbow-mode)


;;; Scss mode
(ensure-package 'scss-mode)
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . scss-mode))

(add-hook 'scss-mode-hook #'flycheck-mode)
(add-hook 'scss-mode-hook #'rainbow-mode)


;;; Web mode
(ensure-package 'web-mode)
(require 'web-mode)
(ensure-package 'company-web)
(require 'company-web)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tlp\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-enable-auto-pairing t
      web-mode-enable-auto-closing t
      web-mode-enable-css-colorization t
      web-mode-enable-engine-detection t
      web-mode-enable-current-column-highlight nil
      web-mode-enable-current-element-highlight t
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2)

(define-key web-mode-map (kbd "C-c C-k") #'web-mode-comment-or-uncomment)

(defun init-web-mode()
  (set (make-local-variable 'company-backends)
       '((company-web-html company-yasnippet company-dabbrev-code)
         company-capf company-files))
  )

(add-hook 'web-mode-hook #'flycheck-mode)
(add-hook 'web-mode-hook #'rainbow-mode)
(add-hook 'web-mode-hook #'impatient-mode)
(add-hook 'web-mode-hook #'init-web-mode)

;;; PHP mode
(ensure-package 'php-mode)
(require 'php-mode)
(ensure-package 'company-php)
(require 'company-php)

(defun init-php-mode()
  (set (make-local-variable 'company-backends)
       '((company-ac-php-backend company-dabbrev-code)
         company-capf company-files))
  )

(add-hook 'php-mode-hook #'init-php-mode)


;;; Lisp mode

(require 'lisp-mode)

(add-hook 'lisp-interaction-mode-hook 'turn-on-auto-fill)
(add-hook 'lisp-interaction-mode-hook 'flycheck-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


(ensure-package 'slime)
(require 'slime)
(ensure-package 'slime-company)
(require 'slime-company)
(require 'slime-autoloads)

(setq inferior-lisp-program "/bin/sbcl")

(defun init-lisp-mode()
  (slime-setup '(slime-company))
  )

(add-hook 'lisp-mode-hook #'init-lisp-mode)

;; LaTeX mode

(ensure-package 'company-auctex)
(require 'company-auctex)
(ensure-package 'auctex)
(require 'tex)

(setq TeX-command-force "pdflatex")


(add-hook 'LaTeX-mode-hook #'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook #'company-auctex-init)
(add-hook 'LaTeX-mode-hook #'turn-on-auto-fill)


;;; Haskell mode

(ensure-package 'haskell-mode)
(require 'haskell-mode)
(ensure-package 'company-ghc)
(require 'company-ghc)
(ensure-package 'flycheck-haskell)
(require 'flycheck-haskell)
(ensure-package 'hindent)
(require 'hindent)

(add-to-list 'auto-mode-alist '("\\.xmobarrc\\'" . haskell-mode))

(defun init-haskell-mode()
  (set (make-local-variable 'company-backends)
       '((company-ghc company-dabbrev-code)
         company-capf company-files))
  )

(add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook #'flycheck-mode)
(add-hook 'haskell-mode-hook #'rainbow-mode)
(add-hook 'haskell-mode-hook #'init-haskell-mode)


;;; JS mode

(require 'js)
(ensure-package 'js2-mode)
(require 'js2-mode)
(ensure-package 'js2-refactor)
(require 'js2-refactor)
(ensure-package 'tide)
(require 'tide)
(ensure-package 'tern)
(require 'tern)
;; (ensure-package 'company-tern)
;; (require 'company-tern)
(ensure-package 'rjsx-mode)
(require 'rjsx-mode)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

(setq js-indent-level 2
      tern-command (append tern-command '("--no-port-file")))

(define-key js-mode-map (kbd "C-c C-i") #'json-pretty-print-buffer)
(define-key tide-mode-map (kbd "C-c .") #'tide-jump-to-definition)

(defun init-js2-mode()
  (setq-local flycheck-disabled-checkers
              (append flycheck-disabled-checkers '(json-jsonlist)))
  ;; (set (make-local-variable 'company-backends)
  ;;      '((company-tern company-dabbrev-code)
  ;;        company-capf company-files))
  )

(defun init-tide-mode()
  (setq-local flycheck-disabled-checkers
              (append flycheck-disabled-checkers '(json-jsonlist)))
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (add-hook 'before-save-hook 'tide-format-before-save)
  )

(add-hook 'js-mode-hook #'flycheck-mode)
(add-hook 'js2-mode-hook #'flycheck-mode)
(add-hook 'js2-mode-hook #'tern-mode)
(add-hook 'js2-mode-hook #'turn-on-auto-fill)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook #'init-js2-mode)
(add-hook 'typescript-mode-hook #'init-tide-mode)
(add-hook 'typescript-mode-hook #'flycheck-mode)
(add-hook 'typescript-mode-hook #'projectile-mode)
(add-hook 'typescript-mode-hook #'tide-hl-identifier-mode)
(add-hook 'typescript-mode-hook #'eldoc-mode)
(add-hook 'typescript-mode-hook #'turn-on-auto-fill)
(add-hook 'typescript-mode-hook #'tide-setup)



;;; Ledger mode

(ensure-package 'ledger-mode)
(require 'ledger-mode)
(ensure-package 'flycheck-ledger)
(require 'flycheck-ledger)

(add-hook 'ledger-mode-hook 'flycheck-mode)


;;; Shell-script mode

(require 'shell)
(ensure-package 'company-shell)
(require 'company-shell)
(ensure-package 'flycheck-checkbashisms)
(require 'flycheck-checkbashisms)

(defun init-sh-set-shell-mode()
  (set (make-local-variable 'company-backends)
       '((company-shell company-shell-env company-files company-dabbrev-code)
         company-capf))
  )

(add-hook 'sh-set-shell-hook #'flycheck-mode)
(add-hook 'sh-set-shell-hook #'flycheck-checkbashisms-setup)
(add-hook 'sh-set-shell-hook #'yas-minor-mode)
(add-hook 'sh-set-shell-hook #'init-sh-set-shell-mode)




;;; XML mode

(ensure-package 'html5-schema)
(require 'html5-schema)
(require 'nxml-mode)

(setq nxml-slash-auto-complete-flag t)

(define-key nxml-mode-map (kbd "C-c C-i") #'nxml-pretty-format)

(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.scxml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.sch\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rng\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.svg\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rss\\'" . nxml-mode))

(add-hook 'nxml-mode-hook #'flycheck-mode)


;;; Markdown mode
(ensure-package 'markdown-mode)
(require 'markdown-mode)
(ensure-package 'markdown-toc)
(require 'markdown-toc)

(setq markdown-command "multimarkdown"
      markdown-hide-markup nil
      markdown-bold-underscore t
      markdown-italic-underscore t
      markdown-header-scaling t
      markdown-indent-function t
      markdown-enable-math t
      markdown-hide-urls nil)

(defun init-markdown-mode()
  (set (make-local-variable 'company-backends)
       '((company-abbrev company-keywords company-ispell)
         company-capf company-files))
  )

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook #'auto-fill-mode)
(add-hook 'markdown-mode-hook #'flycheck-mode)
(add-hook 'markdown-mode-hook #'turn-on-flyspell)
(add-hook 'markdown-mode-hook #'init-markdown-mode)



;;; Groovy mode
(ensure-package 'groovy-mode)
(require 'groovy-mode)

(add-to-list 'flycheck-checkers 'groovy)
(setq flycheck-groovy-executable "groovy")
(add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . groovy-mode))

(defun init-groovy-mode()
  (set (make-local-variable 'company-backends)
       '((company-abbrev company-keywords)
         company-capf company-files))
  )
(add-hook 'groovy-mode-hook #'init-groovy-mode)
(add-hook 'groovy-mode-hook #'flycheck-mode)
(add-hook 'groovy-mode-hook #'groovy-electric-mode)


;;; Indent tools
(ensure-package 'indent-tools)
(require 'indent-tools)


;;; YAML mode

(ensure-package 'flycheck-yamllint)
(require 'flycheck-yamllint)
(ensure-package 'yaml-mode)
(require 'yaml-mode)

(add-hook 'yaml-mode-hook #'flycheck-mode)
(add-hook 'yaml-mode-hook #'flycheck-yamllint-setup)
(add-hook 'yaml-mode-hook #'indent-tools-minor-mode)

;;; Meson mode

(ensure-package 'meson-mode)
(require 'meson-mode)


;;; Docker

(ensure-package 'dockerfile-mode)
(require 'dockerfile-mode)
(ensure-package 'docker-tramp)
(require 'docker-tramp)

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.Dockerfile\\'" . dockerfile-mode))


;;; Kubernetes

(ensure-package 'kubernetes)
(require 'kubernetes)
(ensure-package 'kubernetes-tramp)
(require 'kubernetes-tramp)
(ensure-package 'company-terraform)
(require 'company-terraform)
(ensure-package 'terraform-mode)
(require 'terraform-mode)
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
(add-to-list 'auto-mode-alist '("\\.tf.json\\'" . terraform-mode))


(defun init-terraform-mode()
  (set (make-local-variable 'company-backends)
       '((company-terraform company-dabbrev-code)
         company-capf company-files))
  )

(add-to-list 'terraform-mode-hook #'init-terraform-mode)


;;; JSON mode
(ensure-package 'json-mode)
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(setq flycheck-json-python-json-executable "python3"
      flycheck-jscsrc ".jscsrc.json")


;;; Rust mode

(ensure-package 'company-racer)
(require 'company-racer)
(ensure-package 'rust-mode)
(require 'rust-mode)
(ensure-package 'flycheck-rust)
(require 'flycheck-rust)
(ensure-package 'racer)
(require 'racer)

(defun init-racer-mode()
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (setq company-tooltip-align-annotations t)
  (set (make-local-variable 'company-backends)
       '((company-racer company-dabbrev-code)
         company-capf company-files))
  )

(add-hook 'racer-mode-hook #'init-racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'smartparens-mode)
(add-hook 'rust-mode-hook #'flycheck-mode)
(add-hook 'rust-mode-hook #'hs-minor-mode)
(add-hook 'rust-mode-hook #'auto-fill-mode)


;;; Lua mode

(ensure-package 'lua-mode)
(require 'lua-mode)
(ensure-package 'company-lua)
(require 'company-lua)

;;; combine lua and dabbrev in one completion, so if lua fails dabbrev
;;; can provide

(add-to-list 'auto-mode-alist '("\\.rockspec" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.busted" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.slua" . lua-mode))

(defun init-lua-mode()
  (set (make-local-variable 'company-backends)
       '((company-lua company-dabbrev-code)
         company-capf company-files))
  )

(add-hook 'lua-mode-hook #'init-lua-mode)
(add-hook 'lua-mode-hook #'flycheck-mode)
(add-hook 'lua-mode-hook #'auto-fill-mode)


;;; Go mode
(ensure-package 'go-mode)
(require 'go-mode)
(ensure-package 'go-add-tags)
(require 'go-add-tags)
(ensure-package 'go-projectile)
(require 'go-projectile)
(ensure-package 'go-dlv)
(require 'go-dlv)
(ensure-package 'go-direx)
(require 'go-direx)
(require 'go-eldoc)
(require 'go-guru)
(ensure-package 'golint)
(require 'golint)
(require 'flycheck-golangci-lint)

(setq go-projectile-tools-path (expand-file-name "~/gocode")
      ;; gofmt-args (quote ("-e"))
      )

(define-key go-mode-map (kbd "M-.") #'godef-jump)
(define-key go-mode-map (kbd "C-c C-i") #'gofmt)
(define-key go-mode-map (kbd "C-c C-c") #'compile)
(define-key go-mode-map (kbd "C-c C-r") #'recompile)

(defun init-go-mode()
  ;; (add-hook 'before-save-hook #'gofmt-before-save)
  ;; go-vet disabled because its command "go tool vet" is deprecated
  ;; on newer golang platforms
  (setq flycheck-disabled-checkers '(go-vet go-test))
  (setq-local projectile-globally-ignored-directories
              ;; 'vendor' dir is made by go modules
              (append '("vendor") projectile-globally-ignored-directories))
  ;; (set (make-local-variable 'company-backends)
  ;;      '((company-go company-dabbrev-code) company-files))
  (set (make-local-variable 'company-backends)
       '((company-go) company-files))

  ;; (set (make-local-variable 'company-backends)
  ;;      '((company-go company-dabbrev-code)
  ;;        company-capf company-files))
  (flycheck-golangci-lint-setup)
  )

(add-hook 'go-mode-hook #'flycheck-mode)
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
(add-hook 'go-mode-hook #'go-eldoc-setup)
(add-hook 'go-mode-hook #'init-go-mode)


(ensure-package 'docker-compose-mode)
(require 'docker-compose-mode)
(require 'flycheck-docker-compose-config)

(flycheck-docker-compose-config-enable)


(ensure-package 'protobuf-mode)
(require 'protobuf-mode)

(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))


(ensure-package 'gitconfig-mode)
(require 'gitconfig-mode)

(ensure-package 'apt-sources-list)
(require 'apt-sources-list)

(ensure-package 'x509-mode)
(require 'x509-mode)

(ensure-package 'love-minor-mode)
(require 'love-minor-mode)

(ensure-package 'plantuml-mode)
(require 'plantuml-mode)
(ensure-package 'flycheck-plantuml)
(require 'flycheck-plantuml)

(setq plantuml-jar-path (expand-file-name "~/plantuml.jar")
      plantuml-output-type "txt"
      plantuml-default-exec-mode 'jar)

(add-hook 'plantuml-mode-hook #'flycheck-plantuml-setup)
(add-hook 'plantuml-mode-hook #'flycheck-mode)

(add-to-list 'auto-mode-alist '("\\.plantuml" . plantuml-mode))


(ensure-package 'logview)
(require 'logview)


;; (unless (boundp 'completion-in-region-function)
;;   (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
;;   (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))


(ensure-package 'nginx-mode)
(require 'nginx-mode)

(ensure-package 'company-nginx)
(require 'company-nginx)

(add-hook 'nginx-mode-hook #'company-nginx-keywords)
(add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))


(ensure-package 'csv-mode)
(require 'csv-mode)
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))


(ensure-package 'gdscript-mode)
(require 'gdscript-mode)

(setq gdscript-use-tab-indents nil ;; If true, use tabs for indents. Default: t
      gdscript-indent-offset 4) ;; Controls the width of tab-based indents


(ensure-package 'iedit)
(require 'iedit)
(global-set-key (kbd "C-;") 'iedit-mode)
(setq iedit-toggle-key-default (kbd "C-;"))

(provide 'emacs.common)
;;; emacs.common.el ends here
