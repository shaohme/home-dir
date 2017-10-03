;;; package --- main init file
;;; Commentary:
;;; Code:
(require 'package)

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(package-initialize)

(require 'init-common)
(require 'init-helm)
(require 'init-mail)
(require 'init-dev-common)
(require 'init-c-common)
(require 'init-cmake)
(require 'init-java)
(require 'init-org)
(require 'init-python)
(require 'init-term)
(require 'init-apache)
(require 'init-sql)
(require 'init-web)
(require 'init-lisp)
(require 'init-latex)
(require 'init-haskell)
(require 'init-json)
(require 'init-ledger)
(require 'init-shell-script)
(require 'init-xml)
(require 'init-markdown)
(require 'init-javascript)
(require 'init-groovy)
(require 'init-typescript)
(require 'init-yaml)
(require 'init-sourcecontrol)

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
    (modern-cpp-font-lock js2-refactor json-navigator tide flycheck-yamllint company-shell company-tern cl-lib angular-snippets diff-hl ctags-update csv-mode realgud hydra elpy js2-mode xkcd flycheck-rtags helm-ghc company-ghc irony flycheck-haskell ghc ghc-imported-from haskell-snippets haskell-tab-indent company-quickhelp company-web zenburn-theme company-emacs-eclim company-jedi markdown-mode markdown-preview-mode yaml-mode rainbow-mode cmake-ide cmake-project projectile flycheck-ledger auto-complete company yasnippet flycheck helm jedi-core zygospore xterm-color ws-butler web-mode volatile-highlights undo-tree solarized-theme smartparens skewer-mode sed-mode scss-mode scala-mode sass-mode rtags po-mode pdf-tools password-store org-journal org-doing org-beautify-theme org-ac org nyan-mode notmuch nginx-mode magit-svn magit-gitflow magit-gerrit magit-find-file magit-annex lua-mode log4j-mode json-mode jedi iedit html5-schema ht helm-swoop helm-projectile helm-gtags haxor-mode guru-mode groovy-mode grails-mode gradle-mode go-mode gitignore-mode gitconfig-mode ggtags flymake-sass flymake-ruby flymake-php flymake-less flymake-json flymake-jslint flymake-css ess ecb direx-grep diredful dired-toggle-sudo dired-single dired-atool d-mode csharp-mode company-irony-c-headers company-irony color-theme-sanityinc-solarized cmake-mode clojure-mode cil-mode bash-completion babel autopair auto-indent-mode auto-complete-nxml auto-complete-clang-async auctex apples-mode anzu anything angular-mode android-mode aggressive-indent ada-mode ac-php ac-octave ac-ispell ac-html ac-dcd ac-clang)))
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
     (engine . jinja)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eclim-problems-highlight-error-face ((t (:underline (:color "red" :style wave)))))
 '(popup-face ((t (:background "gray24" :foreground "gray63")))))

;;; .emacs ends here
