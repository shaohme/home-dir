;;; package --- common init file
;;; Commentary:
;;; Code:

(setq-default frame-title-format '("%b [%m] %F")
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
      dired-listing-switches "-lah"
      directory-free-space-args "-Pkh"
      )

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'use-package)

(use-package spacemacs-theme
  :defer t
  :ensure t
  :commands spacemacs-dark
  :init
  (load-theme 'spacemacs-dark t)
  )

(use-package tramp-theme
  :after spacemacs-theme
  :defer t
  :ensure t
  :init
  (load-theme 'tramp t))


(use-package ispell
  :config
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_US")
  )

(use-package recentf
  :config
  (setq recentf-max-menu-items 25)
  )

(use-package local-common
  :load-path "site-lisp"
  :commands switch-dictionary backward-delete-word nxml-pretty-format notify-compilation-result
  )

(use-package mailto
  :load-path "site-lisp"
  :commands mailto-compose-mail
  )

(add-to-list 'compilation-finish-functions 'notify-compilation-result)

(global-set-key (kbd "C-c C-k") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x t") 'beginning-of-buffer)
(global-set-key (kbd "C-x e") 'end-of-buffer)
(global-set-key (kbd "C-x C-b") nil)
(global-set-key (kbd "M-<backspace>") 'backward-delete-word)
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
(recentf-mode 1)
(transient-mark-mode 1)
(size-indication-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook #'goto-address-mode)


(defun init-makefile-mode()
  (setq indent-tabs-mode t)
  )

(add-hook 'makefile-mode-hook 'init-makefile-mode)

(use-package volatile-highlights
  :ensure t
  :commands volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  )

(use-package rainbow-mode
  :defer t
  :ensure)

(use-package eww
  :defer t
  :ensure t
  :init
  (add-hook 'eww-mode-hook 'auto-fill-mode)
  :config
  (setq eww-download-directory "~/dwl/")
  )


(use-package flyspell
  :bind (("<f8>" . switch-dictionary))
  :ensure t
  :config
  (setq flyspell-issue-message-flag nil)
  )

(use-package browse-url
  :config
  (setq browse-url-browser-function 'browse-url-firefox
        browse-url-new-window-flag t
        browse-url-firefox-new-window-is-tab t)
  )

(use-package company
  :defer t
  :ensure t
  :commands company-complete
  :bind (("<C-M-i>" . company-complete)
         :map company-active-map
         ([tab] . company-complete-selection))
  :config
  (setq company-idle-delay 0.4
        company-minimum-prefix-length 2
        ;; aligns annotation to the right hand side
        company-tooltip-align-annotations t
        company-selection-wrap-around t)
  :init
  (add-hook 'after-init-hook 'global-company-mode)
)


(use-package helm
  :ensure t
  :defines helm-map
  :commands helm-autoresize-mode
  :bind (("C-c h" . helm-command-prefix)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-h SPC" . helm-all-mark-rings)
         ([remap find-tag]  . helm-etags-select)
         ([remap list-buffers] . helm-buffers-list)

         ;; ("C-c h o" . helm-occur)
         ;; ("C-c h C-c w" . helm-wikipedia-suggest)
         ;; ("C-c h x" . helm-register)
         ;; ("C-c h o" . helm-swonop)
         ("C-c s" . helm-multi-swoop-all)
         ;; show minibuffer history with Helm
         (:map minibuffer-local-map
               ("M-p" . helm-minibuffer-history)
               ("M-n" . helm-minibuffer-history))

         ;; (:map help-command
         ;;       ("C-f" . helm-apropos)
         ;;       ("r" . helm-info-emacs)
         ;;       ("C-l" . helm-locate-library))
         )
  :config
  (setq ;; mouse-sel-retain-highlight t
        ;; open helm buffer inside current window, not occupy whole other window
        helm-split-window-inside-p t
        ;; scroll 4 lines other window using M-<next>/M-<prior>
        helm-scroll-amount 4
        ;; limit the number of displayed canidates
        helm-candidate-number-limit 500
        ;; move to end or beginning of source when reaching top or bottom of source.
        helm-move-to-line-cycle-in-source t
        ;; fuzzy matching buffer names when non-nil
        ;; useful in helm-mini that lists buffers
        helm-autoresize-max-height 40
        helm-autoresize-min-height 20)
  (helm-mode t)
  (helm-autoresize-mode t)

  :init
  (add-hook 'helm-mode-hook 'helm-autoresize-mode)
  )

(use-package helm-grep
  :bind (:map helm-grep-mode-map
              ("<return>" . helm-grep-mode-jump-other-window)
              ("n" . helm-grep-mode-jump-other-window-forward)
              ("p" . helm-grep-mode-jump-other-window-backward)
              )
  )

(use-package helm-utils
  :init
  (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)
  )

(use-package helm-man)

(use-package helm-help
  :init
  (add-to-list 'helm-sources-using-default-as-input
               'helm-source-man-pages)
  )

(use-package helm-files
  :config
  (setq ;; search for library in `require' and `declare-function'
   ;; sexp.
   ;; helm-ff-auto-update-initial-value t
   helm-ff-search-library-in-sexp t
   helm-ff-file-name-history-use-recentf t)
  )

(use-package helm-swoop
  :ensure t
  :bind ((:map helm-swoop-map ("M-i"
                              . helm-multi-swoop-all-from-helm-swoop))
         (:map isearch-mode-map ("M-i" . helm-swoop-from-isearch)))
  :config
  (setq helm-multi-swoop-edit-save t
        ;; If this value is t, split window inside the current window
        helm-swoop-split-with-multiple-windows t
        ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
        helm-swoop-split-direction 'split-window-vertically)
  )

(use-package helm-config )
;; rebihnd tab to do persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; make TAB works in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;; list actions ; using C-z
(define-key helm-map (kbd "C-z") 'helm-select-action)


(use-package helm-tramp
  :bind ("C-c s" . helm-tramp)
  :ensure t
  :config
  (setq tramp-default-method "ssh"))

;; (setq tramp-verbose 10)

;; (helm-mode t)
;; (helm-autoresize-mode t)

(use-package bbdb
  :defer t
  :ensure t
  :config
  (setq bbdb-file (expand-file-name "bbdb" user-emacs-directory)
        ;; bbdb-pop-up-window-size 0.15
        ;; bbdb-mua-pop-up-window-size 0.15
        bbdb-mua-update-interactive-p '(query . create)
        bbdb-mua-auto-update-p 'query
        bbdb-message-all-addresses t)
  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'message)
  )

(use-package gnus-art
  :defer t)
(use-package gnus-sum
  :defer t)
(use-package gnus-topic
  :defer t)
(use-package gnus-score
  :defer t)
(use-package gnus-salt
  :defer t)

(use-package gnus-topic
  :defer t)

(use-package gnus-async
  :defer t)

(use-package gnus-agent
  :defer t)

(use-package gnus
  :defer t
  :after bbdb emojify
  :bind (:map gnus-summary-mode-map
              (";" . bbdb-mua-edit-field))
  :config
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
        gnus-ignored-from-addresses "Martin Kjær Jørgensen")
  :init
  (add-hook 'gnus-summary-mode-hook 'emojify-mode)
  )

(use-package message
  :defer t
  :after gnus
  :config
  (use-package gnus)
  (setq user-mail-address "mkj@gotu.dk"
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
        )
  :init
  (add-hook 'message-send-hook
            (lambda ()
              (message-add-header "X-PGP-Key: fp=\"730C C366 E9E2 C833
  E62F B412  0D20 8662 8A3D 849A\"; id=\"0x8A3D849A\";
  get=<http://www.gotu.dk/8a3d849a.asc>; get=<hkp://pgp.mit.edu/pks/lookup?search=0x0D2086628A3D849A&op=get>; get=<https://keyserver.pgp.com/vkd/DownloadKey.event?keyid=0x0D2086628A3D849A>;")))
  )

(use-package nndraft
  :defer t
  :config
  (setq nndraft-directory (concat message-directory "drafts/"))
  )

(use-package nnfolder
  :defer t
  :config
  (setq nnfolder-directory (concat message-directory "archive/")
        nnfolder-active-file (concat message-directory "archive"))
  )

(use-package smtpmail
  :defer t
  :config
  (setq smtpmail-debug-info t)
  )

(use-package emojify
  :defer t
  :ensure t)


(use-package mml
  :defer t
  :after emojify
  :init
  (add-hook 'mail-mode-hook 'footnote-mode)
  (add-hook 'mail-mode-hook 'turn-on-flyspell)
  (add-hook 'mail-mode-hook 'turn-on-auto-fill)
  (add-hook 'message-mode-hook 'footnote-mode)
  (add-hook 'message-mode-hook 'turn-on-flyspell)
  (add-hook 'message-mode-hook 'turn-on-auto-fill)
  (add-hook 'message-mode-hook 'emojify-mode)
  )

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))


(use-package flycheck
  :defer t
  :defines flycheck-disabled-checkers
  :ensure t
  :config
  (setq flycheck-json-python-json-executable "python3"))

(use-package gud
  :defer t
  )

;; (require 'init-dev-common)

(use-package projectile
  :defer t
  :ensure t
  :config
  (setq projectile-globally-ignored-directories
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
  :init
  (add-hook 'after-init-hook 'projectile-mode)
  )

(use-package helm-projectile
  :defer t
  :ensure t
  :init
  (add-hook 'projectile-mode-hook #'helm-projectile-on)
  )

(use-package gdb-mi
  :defer
  :config
  (setq gdb-many-windows t
        gdb-show-main t))

(use-package systemd
  :defer t
  :ensure t)

(use-package rtags
  :defer t
  :ensure t)

(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony-c-headers))

(use-package irony
  :defer t
  :ensure t
  :after (projectile company-irony company-irony-c-headers)
)

(use-package flycheck-irony
  :defer t
  :ensure t
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package smartparens
  :defer t
  :ensure t
  :commands sp-with-modes sp-local-pair)

(use-package diff-hl
  :defer t
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package cmake-ide
  :defer t
  :ensure t)

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (add-hook 'c-mode-common-hook #'modern-c++-font-lock-mode))

(use-package cc-mode
  :defer t
  :defines c-mode-base-map
  :after (cmake-ide modern-cpp-font-lock)
  :bind (:map c-mode-base-map
              ("C-x C-m" . cmake-ide-run-cmake)
              ("C-c ." . rtags-find-symbol-at-point)
              ("C-c ," . rtags-find-references-at-point)
              ("C-c C-c" . cmake-ide-compile)
              ("C-c C-k" . comment-or-uncomment-region))
  :config
  (make-local-variable 'company-backends)
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-capf company-backends))
  (setq company-backends (delete 'company-etags company-backends))
  (setq-local c-default-style "stroustrup")
  (setq-local c-basic-offset 4)
  (setq-local tab-width 4)
  (setq-local fill-column 80)
  (setq-local indent-tabs-mode nil)
  (c-set-offset 'innamespace 0)
  (add-to-list 'company-backends '(company-irony-c-headers
                                   company-irony))
  ;; (irony-cdb-json-add-compile-commands-path (projectile-project-root)
  ;;                                           (concat (file-name-as-directory "build")
  ;;                                                   "compile_commands.json"))
  :init
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET"))))
  (add-hook 'c-mode-common-hook 'projectile-mode)
  (add-hook 'c-mode-common-hook 'smartparens-mode)
  (add-hook 'c-mode-common-hook 'irony-mode)
  (add-hook 'c-mode-common-hook 'flycheck-mode)
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  (add-hook 'c-mode-common-hook 'auto-fill-mode)
  (add-hook 'c-mode-common-hook #'modern-c++-font-lock-mode)
  (add-hook 'c-mode-common-hook #'cmake-ide-setup)
  (add-hook 'c-mode-common-hook 'irony-cdb-autosetup-compile-options)
)

(use-package cmake-font-lock
  :defer t
  :ensure t
  :init
  (add-hook 'cmake-mode-hook #'cmake-font-lock-activate))

(use-package cmake-mode
  :defer t
  :ensure t)

(use-package org
  :defer t
  :ensure t)

(use-package org-journal
  :defer t
  :ensure t
  :config
  (setq org-journal-dir "~/org/journal/"))

(use-package python
  :config
  (setq python-indent-guess-indent-offset nil
        python-indent-offset 4
        python-shell-interpreter "python3")
  )

(use-package python-environment
  ;; :defer t
  :ensure t
  :config
  (setq python-environment-directory "~/.virtualenvs")
  )

(use-package company-jedi
  :defer t
  :ensure t
  :config
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  ;; (add-to-list 'company-backends 'company-jedi)
  )

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(use-package jedi-core
  :after python-environment
  :defer t
  :ensure t
  :config
  (setq jedi:environment-root "default"
        jedi:server-command (list (concat python-environment-directory "/" jedi:environment-root "/bin/jediepcserver"))
        jedi:complete-on-dot t)
  )

(use-package python-mode
  :after python-environment company company-jedi python
  :bind (:map python-mode-map
              ("C-M-i" . company-complete)
              ("M-<tab>" . company-complete)
              ("C-c C-k" . comment-dwim))
  :defer t
  :ensure t
  :mode ("\\.py$" . python-mode)
  :config
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)
  (setq py-indent-tabs-mode nil
        py-auto-complete-p nil
        py-complete-function nil)
  (setq flycheck-python-pycompile-executable "python3"
        flycheck-python-pylint-executable (concat python-environment-directory "/default/bin/pylint"))
  :init
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'projectile-mode)
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  )


(use-package yasnippet
  :defer t
  :ensure t
  :config
  (yas-global-mode 1)
  (yas-load-directory (expand-file-name "snippets" user-emacs-directory))
  )

(use-package term
  :defer
  :config
  (setq yas-dont-activate-functions t)
  )

(use-package apache-mode
  :defer t
  :ensure t)

(use-package sql
  :defer t
  :init
  (add-hook 'sql-mode-hook 'toggle-truncate-lines)
  (add-hook 'sql-mode-hook 'flyspell-prog-mode)
  )

(use-package web-mode
  :bind (:map web-mode-map
              ("C-c C-k" . web-mode-comment-or-uncomment))
  :defer t
  :ensure t
  :mode ("\\.phtml\\'" . web-mode)
  :mode ("\\.tpl\\.php\\'" . web-mode)
  :mode ("\\.jsp\\'" . web-mode)
  :mode ("\\.as[cp]x\\'" . web-mode)
  :mode ("\\.erb\\'" . web-mode)
  :mode ("\\.mustache\\'" . web-mode)
  :mode ("\\.djhtml\\'" . web-mode)
  :mode ("\\.jhtml\\'" . web-mode)
  :mode ("\\.html?\\'" . web-mode)
  :mode ("\\.css?\\'" . web-mode)
  :mode ("\\.scss?\\'" . web-mode)
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-css-colorization t
        web-mode-enable-engine-detection t
        web-mode-enable-current-column-highlight nil
        web-mode-enable-current-element-highlight t
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (add-to-list 'company-backends 'company-web-html)
  ;; (add-to-list 'company-backends 'company-web-jade)
  ;; (add-to-list 'company-backends 'company-web-slim)
  (add-to-list 'company-backends 'company-yasnippet)
  )

(use-package sass-mode
  :defer t
  :ensure t
  :init
  (add-hook 'sass-mode-hook 'flycheck-mode)
)

(use-package lisp-mode
  :defer t
  :init
  (add-hook 'lisp-interaction-mode-hook 'turn-on-auto-fill)
  (add-hook 'lisp-interaction-mode-hook 'flycheck-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  )

(use-package emacs-lisp
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  )

(use-package company-auctex
  :ensure t
  :defer t)

(use-package pdf-tools
  :defer t
  :ensure t
  ;; :config
  ;; (setq pdf-info-epdfinfo-program (expand-file-name "epdfinfo" (expand-file-name "bin" user-emacs-directory)))
  )

(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-command-force "pdflatex"
        ;; dont ask for save
        TeX-view-program-selection '((output-pdf "pdf-tools"))
        TeX-view-program-list '(("pdf-tools"
                                 "TeX-pdf-tools-sync-view")))
  (pdf-tools-install)
  :init
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook 'company-auctex-init)
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  )


;; (use-package company-ghc
;;   :defer t
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-ghc)
;;   )

(use-package hindent
  :defer t
  :ensure t)

(use-package haskell-mode
  :defer t
  :ensure t
  :after hindent
  :after company
  :after rainbow-mode
  :after flycheck
  :mode ("\\.xmobarrc\\'" . haskell-mode)
  ;; :after company-ghc
  :init
  (add-hook 'haskell-mode-hook 'hindent-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'rainbow-mode)
  ;; (add-hook 'haskell-mode-hook '(lambda () (ghc-init) (hare-init)))
  )

(use-package js
  :defer t
  :bind (:map js-mode-map
              ("C-c C-i" . json-pretty-print-buffer))
  :ensure t
  :config
  (setq js-indent-level 2)
  :init
  (add-hook 'js-mode-hook 'flycheck-mode)
  )


(use-package ledger-mode
  :defer t
  :ensure t
  :init
  (add-hook 'ledger-mode-hook 'flycheck-mode)
  )

(use-package company-shell
  :ensure t)
(use-package flycheck-checkbashisms
  :defer t
  :ensure t)

(use-package shell
  :defer t
  :init
  (add-to-list 'company-backends '(company-shell company-shell-env))
  (add-hook 'sh-set-shell-hook 'flycheck-mode)
  (add-hook 'sh-set-shell-hook 'flycheck-checkbashisms-setup)
  (add-hook 'sh-set-shell-hook 'yas-minor-mode)
  )


(use-package html5-schema
  :defer t
  :ensure t)

(use-package nxml-mode
  :defer t
  :mode ("\\.xml\\'" . nxml-mode)
  :mode ("\\.scxml\\'" . nxml-mode)
  :mode ("\\.xsd\\'" . nxml-mode)
  :mode ("\\.sch\\'" . nxml-mode)
  :mode ("\\.rng\\'" . nxml-mode)
  :mode ("\\.xslt\\'" . nxml-mode)
  :mode ("\\.svg\\'" . nxml-mode)
  :mode ("\\.rss\\'" . nxml-mode)
  :bind (:map nxml-mode-map
              ("C-c C-i" . nxml-pretty-format))
  :config
  (setq-local nxml-slash-auto-complete-flag t)
  :init
  (add-hook 'nxml-mode-hook 'flycheck-mode)
  )


(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :ensure t
  :init
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (add-hook 'markdown-mode-hook 'flycheck-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  ;; (add-hook 'markdown-mode-hook 'markdown-live-preview-mode)
  :config
  (setq markdown-command "multimarkdown")
  )

(use-package company-dabbrev)

(use-package groovy-mode
  :defer t
  :ensure t
  :after company-dabbrev
  :config
  (setq-local company-dabbrev-downcase nil)
  (setq-local company-dabbrev-ignore-case t)
  :init
  (add-hook 'groovy-mode-hook 'flycheck-mode)
  (add-hook 'groovy-mode-hook 'groovy-electric-mode)
  )


(use-package tide
  :defer t
  :mode ("\\.ts\\'" . typescript-mode)
  :bind (:map tide-mode-map
              ("C-c ." . tide-jump-to-definition))
  :ensure t
  :config
  (setq-local flycheck-disabled-checkers
              (append flycheck-disabled-checkers '(json-jsonlist)))
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :init
  (add-hook 'typescript-mode-hook 'flycheck-mode)
  (add-hook 'typescript-mode-hook 'projectile-mode)
  (add-hook 'typescript-mode-hook 'tide-hl-identifier-mode)
  (add-hook 'typescript-mode-hook 'eldoc-mode)
  (add-hook 'typescript-mode-hook 'turn-on-auto-fill)
  (add-hook 'typescript-mode-hook #'tide-setup)
  (add-hook 'before-save-hook 'tide-format-before-save)
  )

(use-package js2-refactor
  :defer t
  :ensure t
  )

(use-package tern
  :defer t
  :ensure t
  :config
  (setq tern-command (append tern-command '("--no-port-file"))))

(use-package company-tern
  :defer t
  :ensure t)

(use-package js2-mode
  :after flycheck
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :ensure t
  :config
  (setq-local flycheck-jscsrc ".jscsrc.json")
  (setq-local flycheck-disabled-checkers
              (append flycheck-disabled-checkers '(json-jsonlist)))
  (add-to-list 'company-backends 'company-tern)
  :init
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'js2-mode-hook 'projectile-mode)
  (add-hook 'js2-mode-hook 'tern-mode)
  (add-hook 'js2-mode-hook 'turn-on-auto-fill)
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  )



(use-package ediff
  :defer t
  :ensure t
  :config
  (setq-local ediff-window-setup-function 'ediff-setup-windows-plain)
  )

(use-package git-commit
  :defer t
  :ensure t
  :init
  (add-hook 'git-commit-mode-hook 'flyspell-mode)
  (add-hook 'git-commit-mode-hook 'turn-on-auto-fill)
  )

(use-package flycheck-yamllint
  :defer t
  :ensure t
  )


(use-package yaml-mode
  :defer t
  :ensure t
  :after company flycheck flycheck-yamllint
  :init
  (add-hook 'yaml-mode-hook 'flycheck-mode)
  (add-hook 'yaml-mode-hook #'flycheck-yamllint-setup)
  )

(use-package meson-mode
  :defer t
  :ensure t)

(use-package dockerfile-mode
  :defer t
  :mode (("Dockerfile\\'" . dockerfile-mode))
  :mode (("\\.Dockerfile\\'" . dockerfile-mode))
  :ensure t)

(use-package docker-tramp
  :defer t
  :ensure t)

(use-package kubernetes
  :defer t
  :ensure t
  :commands (kubernetes-overview))

(use-package kubernetes-tramp
  :defer t
  :ensure t)

(use-package company-terraform
  :defer t
  :ensure t
  :config
  (add-to-list 'company-backends 'company-terraform))

(use-package terraform-mode
  :after company-terraform
  :defer t
  :ensure t
  :mode ("\\.tf\\'" . terraform-mode)
  :mode ("\\.tf.json\\'" . terraform-mode))

(use-package company-racer
  :defer t
  :ensure t
  )

(use-package flycheck-rust
  :defer t
  :ensure t)

(use-package racer
  :defer t
  :ensure t
  :after company-racer
  :config
  (add-to-list 'company-backends 'company-racer)
  :init
  (add-hook 'racer-mode-hook #'eldoc-mode)
)

(use-package rust-mode
  :defer t
  :ensure t
  :after racer-mode
  :after flycheck-rust
  :config
  (setq company-tooltip-align-annotations t)
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'smartparens-mode)
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'rust-mode-hook #'hs-minor-mode)
  (add-hook 'rust-mode-hook #'auto-fill-mode)
  )

(use-package go-add-tags
  :defer t
  :ensure t)

(use-package go-projectile
  :defer t
  :ensure t)

(use-package go-dlv
  :defer t
  :ensure t)

(use-package go-direx
  :defer t
  :ensure t)

(use-package go-eldoc
  :defer t
  :ensure t
)

(use-package go-guru
  :defer t
  :ensure t)

(use-package golint
  :defer t
  :ensure t)


(use-package go-mode
  :defer t
  :ensure t
  :after (go-guru go-eldoc company-mode go-dlv company-go go-add-tags go-projectile go-direx)
  :bind (:map go-mode-map
              ("C-c ." . godef-jump)
              ("C-c C-c" . compile)
              ("C-c C-r" . recompile))
  :config
  (add-to-list 'company-backends #'company-go)
  ;; (progn
  ;;   (use-package company-go
  ;;     ;; :config
  ;;     ;; (setq-local company-backends '(company-go))
  ;;     ;; (setq-local company-begin-commands '(self-insert-command))
  ;;     ;; (setq-local company-echo-delay 0)
  ;;     :config
  ;;     ;; (setq company-backends (delete 'company-capf company-backends))
  ;;     ;; (setq company-go-gocode-args "-f emacs")
  ;;     )
  ;;   )
  :init
  ;; (add-hook 'go-mode-hook #'projectile-mode)
  (add-hook 'go-mode-hook #'flycheck-mode)
  ;; (add-hook 'go-mode-hook #'auto-fill-mode)
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook #'go-eldoc-setup)
  )

(use-package realgud
  :defer t
  :ensure t)

(use-package company-lua
  :defer t
  :ensure t
  :config
  (add-to-list 'company-backends 'company-lua))

(use-package lua-mode
  :after company-lua
  :defer t
  :ensure t
  :mode (("\\.rockspec" . lua-mode)
         ("\\.busted" . lua-mode)
         ("\\.slua" . lua-mode))
  :init
  (add-hook 'lua-mode-hook #'projectile-mode)
  (add-hook 'lua-mode-hook #'flycheck-mode)
  (add-hook 'lua-mode-hook #'auto-fill-mode)
  )

(use-package pass
  :defer t
  :ensure t)

(use-package helm-pass
  :defer t
  :ensure t)

(use-package json-mode
  :defer t
  :ensure t
  :mode ("\\.json\\'" . json-mode))

(provide 'emacs.common)
;;; emacs.common.el ends here
