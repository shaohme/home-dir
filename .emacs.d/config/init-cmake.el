;;; package -- Summary
;;; Commentary:
;;; CMAKE development settings
;;; Code:

(require 'company)

(defun init-cmake-mode()
  (company-mode t)
  )

(add-hook 'cmake-mode-hook 'init-cmake-mode)

(provide 'init-cmake)
;;; init-cmake.el ends here
