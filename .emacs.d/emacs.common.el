;;; package --- common init file
;;; Commentary:
;;; Code:

(require 'local-common)

;; Load theme first
(ensure-package 'zenburn-theme)
(require 'zenburn-theme)

(load-theme 'zenburn t)

;; ignore (require 'cl deprecated) warnings in >=emacs-27
;; (setq byte-compile-warnings '(cl-functions))

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
;;; Disable tab-indentation, because it screws with web-mode offset's
              indent-tabs-mode nil
              tab-width 4
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
      dired-listing-switches "-lah"
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
      confirm-kill-emacs nil
      confirm-kill-processes nil
      ;; ispell options apparently needed to work on Gentoo systems
      ;; otherwise ispell complains it cannot find dictionaries
      ispell-program-name (executable-find "hunspell")
      ispell-dictionary "en_US"
      ispell-really-hunspell t
      ispell-personal-dictionary "~/.ispell"
      ispell-hunspell-dictionary-alist (quote (("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_US") nil utf-8)
                                            ("da_DK" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "da_DK") nil utf-8)
                                            ))
      ispell-local-dictionary "en_US"
      )

(ispell-change-dictionary "en_US")


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


(global-set-key (kbd "C-x t") 'beginning-of-buffer)
(global-set-key (kbd "C-x e") 'end-of-buffer)
(global-set-key (kbd "C-x C-b") nil)
(global-set-key (kbd "M-<backspace>") 'backward-delete-word)
(global-set-key (kbd "<f8>") 'switch-dictionary)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "M-;") 'comment-dwim)
(global-unset-key (kbd "C-x c"))

(add-to-list 'compilation-finish-functions 'notify-compilation-result)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook #'goto-address-mode)

(require 'saveplace)
;;; should we use save-place-local-mode instead?
(save-place-mode 1)

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
(ensure-package 'ivy-rich)
(require 'ivy-rich)
(ensure-package 'counsel)
(require 'counsel)
(ensure-package 'swiper)
(require 'swiper)

;; as per recommendation from ivy-rich website
(setq ivy-use-virtual-buffers t
      ivy-rich-path-style 'abbrev
      ivy-count-format "(%d/%d) "
      enable-recursive-minibuffers t)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

(ivy-mode 1)
(ivy-rich-mode 1)

;; used to add history to minibuffer selections like M-x
;; instead of smex which produces compile errors on install
(amx-mode 1)

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)


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


(ensure-package 'ivy-xref)
(require 'ivy-xref)
(setq xref-show-definitions-function #'ivy-xref-show-defs)

(ensure-package 'dumb-jump)
(require 'dumb-jump)
(setq dumb-jump-selector 'ivy)


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

(define-key global-map [remap projectile-find-file] #'+ivy/projectile-find-file)
(define-key global-map [remap projectile-find-dir] #'counsel-projectile-find-dir)
(define-key global-map [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer)
(define-key global-map [remap projectile-grep] #'counsel-projectile-grep)
(define-key global-map [remap projectile-ag] #'counsel-projectile-ag)
(define-key global-map [remap projectile-switch-project] #'counsel-projectile-switch-project)

;; dont add counsel-projectile to projectile-mode-hook.
;; lisp max depth errors occurs
(add-hook 'after-init-hook 'counsel-projectile-mode)

(ensure-package 'dap-mode)
(require 'dap-mode)


(ensure-package 'lsp-mode)
(require 'lsp-clients)
(require 'lsp-diagnostics)

(ensure-package 'eglot)
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode) "clangd"))

;; disable lsp diagnostics (flycheck) for now.
;; it sets lsp as sole or default flycheck provider
;; and makes errors when idle if enabled.
;; no use for it anyway for now.
(setq lsp-diagnostics-provider :flycheck
      ;; needed for better LSP completion performance
      read-process-output-max (* 1024 1024)
      gc-cons-threshold 134217728)
(ensure-package 'lsp-ivy)
(require 'lsp-ivy)


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
(require 'smartparens-config)

;;; Common development

(require 'erefactor)

(define-key prog-mode-map "\C-c\C-v" erefactor-map)
(add-hook 'prog-mode-hook 'erefactor-lazy-highlight-turn-on)


(defun init-prog-mode()
  ;;; C Mode seems to set indent-tabs-mode to 't'
  (setq indent-tabs-mode nil)
  )

(add-hook 'prog-mode-hook #'init-prog-mode)

(ensure-package 'diff-hl)
(require 'diff-hl)

(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

(ensure-package 'yasnippet)
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(require 'term)

(setq yas-dont-activate-functions t)
(yas-global-mode 1)
(yas-load-directory (expand-file-name "snippets" user-emacs-directory))

(ensure-package 'rainbow-mode)
(require 'rainbow-mode)

(ensure-package 'rainbow-delimiters)
(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;;; Indent tools
(ensure-package 'indent-tools)
(require 'indent-tools)

(add-hook 'prog-mode-hook #'indent-tools-minor-mode)



(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)



(ensure-package 'flymd)
(require 'flymd)


;;; CMake mode

(ensure-package 'cmake-ide)
(require 'cmake-ide)

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





;;; C mode

(require 'cc-mode)
(ensure-package 'company-c-headers)
(require 'company-c-headers)
(ensure-package 'counsel-etags)
(require 'counsel-etags)
(ensure-package 'company-ctags)
(require 'company-ctags)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate t)

(defun init-c-mode()
  (set (make-local-variable 'company-backends)
       '((company-ctags company-c-headers)
         company-files))
  (company-ctags-auto-setup)
  (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
  (add-to-list 'flycheck-disabled-checkers 'c/c++-cppcheck)
  (setq-local c-default-style "linux")
  (setq-local c-basic-offset 4)
  (setq-local tab-width 4)
  (setq-local fill-column 80)
  (setq company-ctags-fuzzy-match-p t
        )
  )

(add-hook 'c-mode-hook #'init-c-mode)


(add-hook 'c-mode-common-hook #'smartparens-mode)
(add-hook 'c-mode-common-hook #'flycheck-mode)
(add-hook 'c-mode-common-hook #'hs-minor-mode)
(add-hook 'c-mode-common-hook #'auto-fill-mode)

;; C++ mode

(defun init-cpp-mode()
  (set (make-local-variable 'company-backends)
       '((company-capf)
         company-files))
  (setq lsp-diagnostics-provider :flycheck)
  (flymake-mode nil)
  )
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'init-cpp-mode)

;;; Org mode

(ensure-package 'org)
(require 'org)
(ensure-package 'org-journal)
(require 'org-journal)

(setq org-journal-dir "~/org/journal/")


;;; Java
(ensure-package 'lsp-java)
(require 'lsp-java)
(require 'dap-java)

(defun init-java-mode()
  (setq lsp-java-java-path "~/.sdkman/candidates/java/11.0.8.hs-adpt/bin/java"
        lsp-diagnostics-provider :flycheck
        lsp-enable-file-watchers t
        lsp-file-watch-threshold 10000)
  (dap-auto-configure-mode)
  )

(add-hook 'java-mode-hook #'init-java-mode)
(add-hook 'java-mode-hook #'lsp)
(add-hook 'java-mode-hook #'dap-mode)
(add-hook 'java-mode-hook #'flycheck-mode)

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
(define-key elpy-mode-map (kbd "C-c C-i") 'elpy-format-code)

;; https://masteringemacs.org/article/compiling-running-scripts-emacs
(defun python--add-debug-highlight ()
  "Adds a highlighter for '# DEBUG #' string"
  (highlight-lines-matching-regexp "# DEBUG #\\s-*$" 'hi-red-b))
(add-hook 'python-mode-hook 'python--add-debug-highlight)

(defun init-elpy-mode()
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

(defun init-elisp-mode()
  ;;; annoying doc warnings
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
  )

(add-hook 'lisp-interaction-mode-hook 'turn-on-auto-fill)
(add-hook 'lisp-interaction-mode-hook 'flycheck-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'init-elisp-mode)
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

(ensure-package 'auctex)
(require 'tex)
(ensure-package 'company-auctex)
(require 'company-auctex)
(ensure-package 'lsp-latex)
(require 'lsp-latex)

(setq TeX-command-force "pdflatex")


(add-hook 'tex-mode-hook #'lsp)
(add-hook 'tex-mode-hook #'flyspell-mode)
(add-hook 'LaTeX-mode-hook #'lsp)
(add-hook 'LaTeX-mode-hook #'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook #'flyspell-mode)
(add-hook 'yatex-mode-hook #'lsp)
(add-hook 'bibtex-mode-hook #'lsp)


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

(defun init-sh-set-shell-mode()
  (set (make-local-variable 'company-backends)
       '((company-capf :with company-dabbrev-code)
         company-files))
  )

(add-hook 'sh-hook #'flycheck-mode)
(add-hook 'sh-hook #'lsp)
(add-hook 'sh-hook #'flycheck-checkbashisms-setup)
(add-hook 'sh-hook #'init-sh-set-shell-mode)




;;; XML mode

(ensure-package 'html5-schema)
(require 'html5-schema)
(require 'nxml-mode)
(require 'lsp-xml)

(defun init-nxml-mode()
  (set (make-local-variable 'company-backends)
       '((company-capf :with company-dabbrev-code)
         company-files))
  )

(setq nxml-slash-auto-complete-flag t
      lsp-xml-jar-file "~/.emacs.d/org.eclipse.lsp4xml-0.3.0-uber.jar"
      )


(define-key nxml-mode-map (kbd "C-c C-i") #'nxml-pretty-format)

(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.scxml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.sch\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rng\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.svg\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rss\\'" . nxml-mode))

(add-hook 'nxml-mode-hook #'init-nxml-mode)
(add-hook 'nxml-mode-hook #'company-mode)
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


;;; YAML mode

(ensure-package 'flycheck-yamllint)
(require 'flycheck-yamllint)
(ensure-package 'yaml-mode)
(require 'yaml-mode)

(defun init-yaml-mode()
  (set (make-local-variable 'company-backends)
       '((company-capf :with company-dabbrev) company-files))
  )

(add-hook 'yaml-mode-hook #'flycheck-mode)
(add-hook 'yaml-mode-hook #'company-mode)
(add-hook 'yaml-mode-hook #'lsp)
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
       '((company-lua :with company-dabbrev-code :with
                       company-yasnippet)
         company-capf company-files))
  )

(add-hook 'lua-mode-hook #'init-lua-mode)
(add-hook 'lua-mode-hook #'flycheck-mode)
(add-hook 'lua-mode-hook #'auto-fill-mode)


;;; Go mode
(ensure-package 'go-mode)
(require 'go-mode)
(ensure-package 'go-projectile)
(require 'go-projectile)
(ensure-package 'go-dlv)
(require 'go-dlv)

(setq go-projectile-tools-path (expand-file-name "~/gocode")
      )

(define-key go-mode-map (kbd "M-.") #'lsp-find-definition)
(define-key go-mode-map (kbd "C-c C-i") #'lsp-format-buffer)

(defun init-go-mode()
  (setq indent-tabs-mode nil)
  ;; go-vet disabled because its command "go tool vet" is deprecated
  ;; on newer golang platforms
  (setq flycheck-disabled-checkers '(go-vet go-test))
  (setq-local projectile-globally-ignored-directories
              ;; 'vendor' dir is made by go modules
              (append '("vendor") projectile-globally-ignored-directories))
  (set (make-local-variable 'company-backends)
       '((company-capf :with company-dabbrev) company-files))
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (define-key go-mode-map (kbd "C-c C-v r") #'lsp-rename)
  )

(add-hook 'go-mode-hook #'init-go-mode)
(add-hook 'go-mode-hook #'flycheck-mode)
(add-hook 'go-mode-hook #'lsp)


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
(setq iedit-toggle-key-default (kbd "C-;")
      compilation-scroll-output t)

(provide 'emacs.common)
;;; emacs.common.el ends here
