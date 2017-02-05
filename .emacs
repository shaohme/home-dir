;;; package --- main init file
;;; Commentary:
;;; Code:
(require 'package)

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(package-initialize)

(load-theme 'zenburn t)

(require 'mwheel)
(require 'tramp)
;; delete when upgrade to >=25.1
;; (require 'saveplace)
(require 'uniquify)
(require 'subr-x)
(require 'recentf)
(require 'helm-config)
(require 'helm-grep)
(require 'helm-swoop)
(require 'helm-files)
(require 'volatile-highlights)
(require 'ws-butler)
(require 'anzu)
(require 'semantic)
(require 'company)
(require 'company-rtags)
(require 'flycheck)
(require 'flycheck-ledger)
(require 'yasnippet)
(require 'jedi-core)
(require 'python-environment)
(require 'rtags)
(require 'flycheck-rtags)
(require 'sql)
(require 'flymake)
(require 'flyspell)
(require 'mailto)
(require 'bbdb)
(require 'gnus)
(require 'gnus-async)
(require 'gnus-agent)
(require 'gnus-art)
(require 'gnus-sum)
(require 'gnus-topic)
(require 'gnus-score)
(require 'gnus-salt)
(require 'message)
(require 'smime)
(require 'smtpmail)
(require 'nndraft)
(require 'nnfolder)
(require 'mml)
(require 'ispell)
(require 'gud)
(require 'python)
(require 'web-mode)
(require 'tex)
(require 'company-auctex)
(require 'nxml-mode)
(require 'gdb-mi)
(require 'ede)
(require 'browse-url)
(require 'eww)

;; ----- Basics -----
(defalias 'yes-or-no-p 'y-or-n-p)

;; (defvar theme-current nil "Current loaded theme.")

;; (defun set-theme(theme)
;;   (setq theme-current theme)
;;   (load-theme theme t))

;; (defun toggle-solarized()
;;   (interactive)
;;   (if (equal theme-current 'sanityinc-solarized-light)
;; 	  (set-theme 'sanityinc-solarized-dark)
;; 	(set-theme 'sanityinc-solarized-light)))

;; (defun file-contents (filename)
;;   "Return the contents of FILENAME."
;;   (with-temp-buffer
;;     (insert-file-contents filename)
;;     (buffer-string)))


;; ;; (when (display-graphic-p)
;; (let ((colorstate (string-trim (with-temp-buffer
;;                                  (insert-file-contents "~/.dynamic-colors/colorscheme")
;;                                  (buffer-string)))))
;;   (if (eq colorstate "solarized-light")
;;       (set-theme 'sanityinc-solarized-dark)
;;     (set-theme 'sanityinc-solarized-light)))

;;   ;; )

(setq-default frame-title-format '("%b [%m] %F")
;;; Disable tab-indentation, because it screws with web-mode offset's
              indent-tabs-mode nil
              save-place 1
              flycheck-emacs-lisp-load-path load-path)

(setq user-full-name "Martin Kjær Jørgensen"
      user-mail-address "mkj@gotu.dk"
      default-frame-alist '((font . "monospace 12"))
      browse-url-browser-function 'browse-url-firefox
      browse-url-new-window-flag t
      browse-url-firefox-new-window-is-tab t
      inhibit-startup-message t
      eww-download-directory "~/dwl/"
      password-cache t
      password-cache-expiry 86400
      mail-user-agent 'gnus-user-agent
      org-journal-dir "~/org/journal/"
      ispell-program-name "hunspell"
      ispell-dictionary "da_DK"
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
      ;; mouse-sel-retain-highlight t
      ;; scroll 4 lines other window using M-<next>/M-<prior>
      helm-scroll-amount 4
      ;; search for library in `require' and `declare-function' sexp.
      helm-ff-search-library-in-sexp t
      ;; open helm buffer inside current window, not occupy whole other window
      helm-split-window-in-side-p t
      ;; limit the number of displayed canidates
      helm-candidate-number-limit 500
      helm-ff-file-name-history-use-recentf t
      ;; move to end or beginning of source when reaching top or bottom of source.
      helm-move-to-line-cycle-in-source t
      ;; fuzzy matching buffer names when non-nil
      ;; useful in helm-mini that lists buffers
      ;; helm-buffers-fuzzy-matching t
      helm-autoresize-max-height 40
      helm-autoresize-min-height 20
      helm-multi-swoop-edit-save t
      ;; If this value is t, split window inside the current window
      helm-swoop-split-with-multiple-windows t
      ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
      helm-swoop-split-direction 'split-window-vertically
      company-idle-delay 0.2
      ;; company-emacs-eclim-ignore-case t
      bbdb-file (expand-file-name "bbdb" user-emacs-directory)
      bbdb-pop-up-window-size 0.15
      bbdb-mua-pop-up-window-size 0.15
      bbdb-mua-update-interactive-p '(query . create)
      bbdb-message-all-addresses t
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
      ;; dont autosave
      message-auto-save-directory nil
      smime-CA-directory "/etc/ssl/certs/"
      smime-CA-file "/etc/ssl/certs/ca-certificates.crt"
      smime-certificate-directory "~/.certs/"
      smime-crl-check "-crl_check"
      smtpmail-debug-info t
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
      gnus-asynchronous t
      gnus-directory "~/.emacs.d/gnus/News/"
      gnus-read-newsrc-file nil
      gnus-save-newsrc-file nil
      gnus-cache-directory (concat gnus-directory "cache/")
      gnus-cache-active-file (concat gnus-directory "cache/active") ; no slash! this is a file, not a directory!
      gnus-article-save-directory (concat gnus-directory "save/")
      gnus-kill-files-directory (concat gnus-directory "killfiles/")
      gnus-message-archive-group "Sent"

      ;; gnus-select-method '(nnimap "imap.gotu.dk"
      ;; 							   (nnimap-stream starttls)
      ;; 							   (nnmail-expire "nnimap:Trash")
      ;; 							   (nnmail-expiry-wait 90))

      ;; gnus-message-archive-method '(nnimap "imap.gotu.dk"
      ;;                                      (nnimap-stream starttls)
      ;;                                      (nnmail-expire "nnimap:Trash")
      ;;                                      (nnmail-expiry-wait 90))

      gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed")
      ;; gnus-mime-view-all-parts t
      ;; this so ido doesnt interfere with gnus own ido completion
      ;; gnus-completing-read-function 'gnus-ido-completing-read
      ;; dont ask for reading newsrc
      gnus-always-read-dribble-file t
      gnus-novice-user nil
      gnus-interactive-exit nil
      gnus-gcc-mark-as-read t
      ;; gnus-summary-line-format "%U%R%d %-5,5L %-20,20n %B%-80,80S\n"
      gnus-summary-line-format "%U%R%d%z%I%(%[%4L: %-23,23f%]%) %s\n"
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
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
      gnus-agent-directory (concat gnus-directory "agent/")
      gnus-agent t
      ;; gnus-agent-prompt-send-queue nil
      ;; Send mail immediately
      ;; gnus-agent-queue-mail nil
      ;; gnus-agent-synchronize-flags t
      ;; gnus-agent-cache t
      gnus-ignored-from-addresses "Martin Kjær Jørgensen"
      gnus-extra-headers (quote (To Cc Newsgroups))
      nnmail-crosspost nil
      nnmail-extra-headers gnus-extra-headers
      nndraft-directory (concat message-directory "drafts/")
      nnfolder-directory (concat message-directory "archive/")
      nnfolder-active-file (concat message-directory "archive")
      compilation-read-command t
      python-indent-guess-indent-offset nil
      python-indent-offset 4
      python-environment-directory "~/.virtualenvs"
      python-shell-interpreter "python3"
      ;; python-python-command (concat python-shell-interpreter " -i")
      gud-pdb-command-name (concat python-shell-interpreter " -m pdb")
      jedi:environment-root "default"
      jedi:server-command (list (concat python-environment-directory "/" jedi:environment-root "/bin/jediepcserver"))
      jedi:complete-on-dot t
      rtags-autostart-diagnostics t
      rtags-completions-enabled t
      rtags-spellcheck-enabled nil
      )

(define-key 'help-command (kbd "C-f") 'helm-apropos)
(define-key 'help-command (kbd "r") 'helm-info-emacs)
(define-key 'help-command (kbd "C-l") 'helm-locate-library)
;; show minibuffer history with Helm
(define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
(define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)
(define-key global-map [remap find-tag] 'helm-etags-select)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

(global-set-key (kbd "C-c C-k") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x t") 'beginning-of-buffer)
(global-set-key (kbd "C-x e") 'end-of-buffer)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "C-x C-b") nil)
(global-set-key (kbd "<f8>") 'switch-dictionary)
(global-set-key (kbd "M-<f12>") 'toggle-solarized)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h C-c w") 'helm-wikipedia-suggest)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "M-<backspace>") 'backward-delete-word)
(global-set-key (kbd "C-c h o") 'helm-swoop)
(global-set-key (kbd "C-c s") 'helm-multi-swoop-all)
(global-set-key (kbd "C-M-i") 'company-complete)

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
(helm-autoresize-mode 1)
(helm-mode 1)
(bbdb-initialize 'gnus 'message)
(gdb-many-windows t)
;;; (message-wash-subject t)
;;; (bbdb-mua-auto-update-init 'gnus 'message)

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



(add-hook 'term-mode-hook (lambda() (setq yas-dont-activate-functions t)))
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)


(defun init-mml-mode()
  ;; (notmuch-address-setup)
  (footnote-mode t)
  ;; (turn-on-orgtbl)
  ;; (turn-on-orgstruct++)
  (turn-on-flyspell)
  (turn-on-auto-fill))


(add-hook 'mail-mode-hook 'init-mml-mode)
(add-hook 'message-mode-hook 'init-mml-mode)

(add-hook
 'gnus-summary-mode-hook
 (lambda ()
    (define-key gnus-summary-mode-map (kbd ";") 'bbdb-mua-edit-field)))

;; use mml-secure-message-sign-pgpmime to sign whole message
;; (add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
(add-hook 'message-send-hook
          (lambda ()
            (message-add-header "X-PGP-Key: fp=\"730C C366 E9E2 C833 E62F B412  0D20 8662 8A3D 849A\"; id=\"0x8A3D849A\"; get=<http://www.gotu.dk/8a3d849a.asc>; get=<hkp://pgp.mit.edu/pks/lookup?search=0x0D2086628A3D849A&op=get>; get=<https://keyserver.pgp.com/vkd/DownloadKey.event?keyid=0x0D2086628A3D849A>;")))
;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)


;;; Apache mode
(defun init-apache-mode()
  (local-set-key (kbd "C-c C-i") 'apache-ident-line))

(add-hook 'apache-mode-hook 'init-apache-mode)

;;; Git mode
(defun init-git-mode()
  (flyspell-mode 1)
  (turn-on-auto-fill))

(add-hook 'git-commit-mode-hook 'init-git-mode)



;;; C mode
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
  (setq-default c-basic-offset 4
                c-default-style "gnu"
                tab-width 4
                indent-tabs-mode t)
  (rtags-start-process-unless-running)
  (rtags-diagnostics)
  (company-mode t)
  (projectile-mode t)
  (flycheck-mode t)
  (flycheck-select-checker 'rtags)
  (local-set-key (kbd "C-c ;") 'iedit-mode)
  (local-set-key (kbd "C-c C-j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c C-s") 'semantic-ia-show-summary)
  (local-set-key (kbd "C-x C-m") 'cmake-ide-run-cmake)
  (local-set-key (kbd "C-c .") 'rtags-find-symbol-at-point)
  (local-set-key (kbd "C-c ,") 'rtags-find-references-at-point)
  ;; (local-set-key (kbd "TAB") 'company-complete-selection)
  (local-set-key (kbd "C-c C-c") (lambda ()
                                   (interactive)
                                   (setq-local compilation-read-command nil)
                                   (call-interactively 'compile-pkg)))
  ;; (rtags-enable-standard-keybindings)
  (hs-minor-mode t))

(defun init-cmake-mode()
  (company-mode t)
  )

(add-hook 'cmake-mode-hook 'init-cmake-mode)

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-selection)
     (define-key company-active-map [tab] 'company-complete-selection)))

(add-hook 'c-mode-hook 'init-c-mode)
(add-hook 'c++-mode-hook 'init-c-mode)

(defun init-cmake-mode()
  (company-mode t)
  )

(add-hook 'cmake-mode-hook 'init-cmake-mode)

;;; SQL mode
(defun init-sql-mode()
  ;; (require 'auto-complete)
  ;; (require 'auto-complete-config)
  ;; (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary
  ;;  ac-source-words-in-same-mode-buffers))
  ;; (setq-local ac-ignore-case t)
  ;; (auto-complete-mode t)
  ;; (ac-flyspell-workaround)
  (flyspell-prog-mode))

(add-hook 'sql-mode-hook 'init-sql-mode)

(defun init-sql-interactive-mode()
  (toggle-truncate-lines t))

(add-hook 'sql-interactive-mode-hook 'init-sql-interactive-mode)


;;; Web mode
(setq safe-local-variable-values (quote ((engine . jinja))))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))

(defun flymake-scss-init ()
  "Flymake support for SCSS files."
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "scss" (list "--check" local-file))))

(push '(".+\\.scss$" flymake-scss-init) flymake-allowed-file-name-masks)

(push '("on line \\([0-9]+\\) of \\([^ ]+\\)$" 2 1 nil 2) flymake-err-line-patterns)


(defun init-web-mode()
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-css-colorization t
        web-mode-enable-engine-detection t
        web-mode-enable-current-column-highlight nil
        web-mode-enable-current-element-highlight t
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (local-set-key (kbd "C-c C-k") 'web-mode-comment-or-uncomment)
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-yasnippet)
  (company-mode t)
  ;; (flymake-mode t)
  )

(add-hook 'web-mode-hook 'init-web-mode)

;; (require 'scss-mode)

;; (defun init-scss-mode()
;;   ;; scss compile-command seems to override other major modes. c-mode
;;   ;; for instance
;;   (make-local-variable 'compile-command)
;;   (require 'auto-complete)
;;   (add-to-list 'ac-sources 'ac-source-dictionary)
;;   (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
;;   (add-to-list 'ac-sources 'ac-source-css-property)
;;   (setq css-indent-offset 2)
;;   (auto-complete-mode t)
;;   (flymake-mode t)

;;   )

;; (add-hook 'scss-mode-hook 'init-scss-mode)


;;; Sass mode
(defun init-sass-mode()
  ;; (require 'auto-complete)
  ;; (require 'auto-complete-config)
  (flycheck-mode t)
  (company-mode t)
  ;; (require 'flymake-cursor)
  ;; (require 'flymake-sass)
  ;; (add-to-list 'ac-sources 'ac-source-css-property)
  ;; (add-to-list 'ac-modes 'sass-mode)
  ;; (auto-complete-mode 1)
  ;; (flymake-sass-load))
  )
(add-hook 'sass-mode-hook 'init-sass-mode)


;;; Python mode
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(defun init-python-mode()
  (jedi:setup)
  (add-to-list 'company-backends 'company-jedi)
  (company-mode t)
  (flycheck-mode t)
  ;; (eldoc-mode t)
  (projectile-mode t))

(add-hook 'python-mode-hook 'init-python-mode)


;;; Lisp
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

;;; Latex mode

(defun init-latex-mode ()
  "."
  (setq TeX-command-force "pdflatex"
        ;; dont ask for save
        TeX-view-program-selection '((output-pdf "pdf-tools"))
        TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))
        )
  (pdf-tools-install)
  (TeX-PDF-mode t)
  (company-auctex-init)
  (auto-fill-mode)
)

(add-hook 'LaTeX-mode-hook 'init-latex-mode)
(add-hook 'pdf-view-mode-hook 'auto-revert-mode)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)


;;; Sieve mode
(defun init-sieve-mode()
  (setq flycheck-disabled-checkers (append '(rtags) flycheck-disabled-checkers)))

(add-hook 'sieve-mode-hook 'init-sieve-mode)

;;; Java mode
(defun init-java-mode()
  ;; (require 'eclimd)
  ;; (setq jdee-server-dir
  ;;       "~/dev/jdee-server/target/jdee-1.1-SNAPSHOT.jar")
  ;; (require 'jdee)
  ;; (eclim-mode)
  ;; (company-emacs-eclim-setup)

  )

(add-hook 'java-mode-hook 'init-java-mode)


;;; Ledger mode-line
(defun init-ledger-mode()
  (flycheck-mode t)
  )

(add-hook 'ledger-mode-hook 'init-ledger-mode)

;;; Shell script
(defun init-shell-script-mode()
  (flycheck-mode t)
  ;; (auto-complete-mode t)
  )

(add-hook 'sh-set-shell-hook 'init-shell-script-mode)


;;; XML Mode
(defun nxml-pretty-format ()
  "."
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
    (nxml-mode)
    (indent-region 0 (count-lines (point-min) (point-max))))
  )


(defun init-nxml-mode()
  "."
  (setq nxml-slash-auto-complete-flag t)
  (flycheck-mode t)
  (local-set-key (kbd "C-c C-i") 'nxml-pretty-format)
  )

(add-hook 'nxml-mode-hook 'init-nxml-mode)


(defun init-mark-down()
  (flycheck-mode t)
  (auto-fill-mode t)
  )

(add-hook 'markdown-mode-hook 'init-mark-down)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(package-selected-packages
   (quote
    (zenburn-theme magit company-emacs-eclim company-jedi markdown-mode markdown-preview-mode yaml-mode rainbow-mode cmake-ide cmake-project projectile flycheck-ledger auto-complete company yasnippet flycheck helm jedi-core zygospore xterm-color ws-butler web-mode volatile-highlights undo-tree solarized-theme smartparens skewer-mode sed-mode scss-mode scala-mode sass-mode rtags po-mode pdf-tools password-store org-journal org-doing org-beautify-theme org-ac org nyan-mode notmuch nginx-mode magit-svn magit-gitflow magit-gerrit magit-find-file magit-annex lua-mode log4j-mode json-mode jedi iedit html5-schema ht helm-swoop helm-projectile helm-gtags haxor-mode haskell-mode guru-mode groovy-mode grails-mode gradle-mode go-mode gitignore-mode gitconfig-mode ggtags flymake-sass flymake-ruby flymake-php flymake-less flymake-json flymake-jslint flymake-css flycheck-irony ess ecb direx-grep diredful dired-toggle-sudo dired-single dired-atool d-mode csharp-mode company-irony-c-headers company-irony color-theme-sanityinc-solarized cmake-mode clojure-mode cil-mode bash-completion babel autopair auto-indent-mode auto-complete-nxml auto-complete-clang-async auctex apples-mode anzu anything angular-mode android-mode aggressive-indent ada-mode ac-php ac-octave ac-ispell ac-html ac-dcd ac-clang)))
 '(safe-local-variable-values
   (quote
    ((eval setq-local jedi:server-command
           (list
            (concat python-environment-directory "/" jedi:environment-root "/bin/jediepcserver")))
     (eval setq-local gud-pdb-command-name
           (concat python-shell-interpreter " -m pdb"))
     (eval setq-local python-python-command
           (concat python-shell-interpreter " -i"))
     (python-shell-interpreter . "python3")
     (jedi:environment-root . "snakeeyes")
     (engine . jinja)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eclim-problems-highlight-error-face ((t (:underline (:color "red" :style wave))))))

;;; .emacs ends here
