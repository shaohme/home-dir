;;; package -- Summary
;;; Commentary:
;;; Helm settings
;;; Code:

(require 'helm-config)
(require 'helm-grep)
(require 'helm-swoop)
(require 'helm-files)

(setq
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
      compilation-read-command t
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

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h C-c w") 'helm-wikipedia-suggest)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h o") 'helm-swoop)
(global-set-key (kbd "C-c s") 'helm-multi-swoop-all)

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(helm-autoresize-mode 1)
(helm-mode 1)




(provide 'init-helm)
;;; init-helm.el ends here
