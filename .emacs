;;; package --- local init file
;;; Commentary:
;;; Code:

(require 'package)

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

(package-initialize)

(load (expand-file-name "emacs.common.el" user-emacs-directory))
(load (expand-file-name "emacs.local.el" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "6ebdb33507c7db94b28d7787f802f38ac8d2b8cd08506797b3af6cdfd80632e0" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(dired-mode-hook
   (cons
    (quote tramp-theme-hook-function)
    (delete
     (quote tramp-theme-hook-function)
     dired-mode-hook)))
 '(eshell-directory-change-hook
   (cons
    (quote tramp-theme-hook-function)
    (delete
     (quote tramp-theme-hook-function)
     eshell-directory-change-hook)))
 '(find-file-hook
   (cons
    (quote tramp-theme-hook-function)
    (delete
     (quote tramp-theme-hook-function)
     find-file-hook)))
 '(mode-line-buffer-identification
   (quote
    (:eval
     (tramp-theme-mode-line-buffer-identification))) t)
 '(package-selected-packages
   (quote
    (company-lua company-go company-terraform emojify flycheck-checkbashisms flycheck-irony magit modern-cpp-font-lock ledger-mode bbdb use-package levenshtein systemd js2-refactor json-navigator tide flycheck-yamllint company-shell company-tern cl-lib angular-snippets diff-hl ctags-update csv-mode realgud hydra elpy js2-mode xkcd helm-ghc company-ghc irony flycheck-haskell ghc ghc-imported-from haskell-snippets haskell-tab-indent company-quickhelp company-web zenburn-theme company-jedi markdown-mode markdown-preview-mode yaml-mode rainbow-mode cmake-ide cmake-project projectile flycheck-ledger auto-complete company yasnippet flycheck helm jedi-core zygospore xterm-color ws-butler web-mode volatile-highlights undo-tree solarized-theme smartparens skewer-mode sed-mode scss-mode scala-mode sass-mode rtags po-mode pdf-tools password-store org-journal org-doing org-beautify-theme org-ac org nyan-mode notmuch nginx-mode magit-svn magit-gitflow magit-gerrit magit-find-file magit-annex lua-mode log4j-mode json-mode jedi iedit html5-schema ht helm-swoop helm-projectile helm-gtags haxor-mode guru-mode groovy-mode grails-mode gradle-mode go-mode gitignore-mode gitconfig-mode ggtags flymake-sass flymake-ruby flymake-php flymake-less flymake-json flymake-jslint flymake-css ess ecb direx-grep diredful dired-toggle-sudo dired-single dired-atool d-mode csharp-mode company-irony-c-headers company-irony color-theme-sanityinc-solarized clojure-mode cil-mode bash-completion babel autopair auto-indent-mode auto-complete-nxml auto-complete-clang-async auctex apples-mode anzu anything angular-mode android-mode aggressive-indent ada-mode ac-php ac-octave ac-ispell ac-html ac-dcd ac-clang)))
 '(safe-local-variable-values
   (quote
    ((flycheck-gcc-language-standard . "c++14")
     (flycheck-clang-language-standard . "c++14")
     (cmake-ide-cmake-opts . "-DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=dist")
     (cmake-ide-make-command . "make -j8 install --no-print-directory")
     (cmake-ide-project-dir . "/home/martin/pikes")
     (cmake-ide-build-dir . "/home/martin/pikes/build")
     (irony-cdb-json-add-compile-commands-path . "build/compile_commands.json")
     (c-default-style . "stroustrup")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eclim-problems-highlight-error-face ((t (:underline (:color "red" :style wave)))))
 '(popup-face ((t (:background "gray24" :foreground "gray63")))))

;;; .emacs ends here
