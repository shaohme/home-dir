;;; package -- Summary
;;; Commentary:
;;; Web development settings
;;; Code:

(require 'web-mode)
(require 'company)
(require 'flymake)

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
  (add-to-list 'company-backends 'company-web-slim)
  (add-to-list 'company-backends 'company-yasnippet)
  (company-mode t)
  ;; (flymake-mode t)
  )

(add-hook 'web-mode-hook 'init-web-mode)

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


(provide 'init-web)
;;; init-web.el ends here
