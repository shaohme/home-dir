;;; package --- main init file
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

;;; .emacs ends here
