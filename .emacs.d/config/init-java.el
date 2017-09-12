;;; package -- Summary
;;; Commentary:
;;; Python settings
;;; Code:

(require 'eclim)
(require 'eclimd)
(require 'company-emacs-eclim)

(setq
      eclim-executable "~/share/eclipse/eclim"
      eclimd-executable "~/share/eclipse/eclimd"
      company-emacs-eclim-ignore-case t
      )

(defun init-java-mode()
  ;; (require 'eclimd)
  ;; (setq jdee-server-dir
  ;;       "~/dev/jdee-server/target/jdee-1.1-SNAPSHOT.jar")
  ;; (require 'jdee)
  (eclim-mode t)
  (company-mode t)
  (gradle-mode t)
  (company-emacs-eclim-setup)
  (projectile-mode t)
  )

(add-hook 'java-mode-hook 'init-java-mode)

(provide 'init-java)
;;; init-java.el ends here
