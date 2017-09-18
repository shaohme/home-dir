;;; package -- Summary
;;; Commentary:
;;; Python settings
;;; Code:

(require 'eclim)
(require 'eclimd)
(require 'company-emacs-eclim)
(require 'gradle-mode)


(defun init-java-mode()
(setq eclim-executable "~/eclipse/eclim"
      eclimd-executable "~/eclipse/eclimd"
      company-emacs-eclim-ignore-case t
      )
  ;; (require 'eclimd)
  ;; (setq jdee-server-dir
  ;;       "~/dev/jdee-server/target/jdee-1.1-SNAPSHOT.jar")
  ;; (require 'jdee)
  (eclim-mode t)
  (company-emacs-eclim-setup)
  (company-mode t)
  (gradle-mode t)
  (projectile-mode t)
  )

(add-hook 'java-mode-hook 'init-java-mode)

(provide 'init-java)
;;; init-java.el ends here
