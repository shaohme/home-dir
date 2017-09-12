;;; package -- Summary
;;; Commentary:
;;; LaTeX development settings
;;; Code:

(require 'tex)

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


(provide 'init-latex)
;;; init-latex.el ends here
