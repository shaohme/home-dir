;;; package -- Summary
;;; Commentary:
;;; Mail and gnus settings settings
;;; Code:

(require 'gnus)
(require 'gnus-async)
(require 'gnus-agent)
(require 'gnus-art)
(require 'gnus-sum)
(require 'gnus-topic)
(require 'gnus-score)
(require 'gnus-salt)
(require 'mml)
(require 'bbdb)
(require 'smtpmail)
(require 'mailto)
(require 'message)
(require 'smime)
(require 'nndraft)
(require 'nnfolder)
(require 'flycheck)

(setq user-mail-address "mkj@gotu.dk"
      mail-user-agent 'gnus-user-agent
      bbdb-file (expand-file-name "bbdb" user-emacs-directory)
      ;; bbdb-pop-up-window-size 0.15
      ;; bbdb-mua-pop-up-window-size 0.15
      bbdb-mua-update-interactive-p '(query . create)
      bbdb-mua-auto-update-p 'query
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
      gnus-agent t
      gnus-agent-directory (concat gnus-directory "agent/")
      gnus-agent-fetch-session 20000000
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
      )

(bbdb-initialize 'gnus 'message)
;;; (message-wash-subject t)
(bbdb-mua-auto-update-init 'message)

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


(defun init-sieve-mode()
  (setq flycheck-disabled-checkers (append '(rtags) flycheck-disabled-checkers)))

(add-hook 'sieve-mode-hook 'init-sieve-mode)


(provide 'init-mail)
;;; init-mail.el ends here
