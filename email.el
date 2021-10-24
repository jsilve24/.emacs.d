;;; email.el --- mu4e setup -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 23, 2021
;; Modified: October 23, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/email
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  mu4e setup
;;
;;; Code:

(straight-use-package '(mu4e
  :files (:defaults "/usr/local/share/emacs/site-lisp/mu4e/*.el")))
(use-package mu4e
  :commands mu4e mu4e-compose-new
  :init
  (provide 'html2text) ;; disable obsolete package
  :config
  (setq mu4e-get-mail-command "mbsync -a"
        mu4e-change-file-names-when-moving t
        mu4e-compose-context-policy 'ask ;; help separate accounts
        mu4e-context-policy 'pick-first
        mu4e-compose-in-new-frame t
        mu4e-sent-messages-behavior 'delete ;; servers take care of this
        mu4e-update-interval 360
        mu4e-headers-include-related nil
        mu4e-confirm-quit nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-use-maildirs-extension nil
        mu4e-attachment-dir "~/Downloads"
        mu4e-enable-async-operations t
        message-kill-on-buffer-exit t  ;; don't keep meesge buffers around
        mu4e-compose-dont-reply-to-self t
        mu4e-view-show-addresses t
        mu4e-hide-index-message t
        mu4e-view-show-images t  ;; try to show images
        mu4e-view-use-gnus t
        message-send-mail-function #'smtpmail-send-it
        smtpmail-stream-type 'starttls
        mu4e-completing-read-function #'completing-read
        mu4e-confirm-quit nil

        ;; set user-agent
        mail-user-agent 'mu4e-user-agent
        message-mail-user-agent 'mu4e-user-agent

        ;; visual niceties
        mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
        mu4e-headers-thread-orphan-prefix '("┬>" . "┬▶ ")
        mu4e-headers-thread-last-child-prefix '("└>" . "╰▶")
        mu4e-headers-thread-child-prefix '("├>" . "├▶")
        mu4e-headers-thread-connection-prefix '("│" . "│ "))

    (setq mu4e-view-actions '(("capture message" . mu4e-action-capture-message)
                            ("browser view" . mu4e-action-view-in-browser)
                            ("pdf view" . mu4e-action-view-as-pdf)
                            ("thread view" . mu4e-action-show-thread)))
    ;;
    ;; No line breaks in compose
    (defun no-auto-fill ()
      "Turn off auto-fill-mode."
      (progn
        (auto-fill-mode -1)
        (setq truncate-lines nil)))
    ;; Turn off 80-character auto-wrap
    (add-hook 'mu4e-compose-mode-hook #'no-auto-fill)

    (setq mu4e-bookmarks
          '(( :name  "Unread messages"
              :query "flag:unread AND NOT flag:trashed AND (maildir:/gmail/INBOX OR maildir:/psu/INBOX)"
              :key ?u)
            ( :name "Today's messages"
              :query "date:today..now AND NOT flag:trashed AND (maildir:/gmail/INBOX OR maildir:/psu/INBOX)"
              :key ?t)
            ( :name "Last 7 days"
              :query "date:7d..now AND NOT flag:trashed AND (maildir:/gmail/INBOX OR maildir:/psu/INBOX)"
              :hide-unread t
              :key ?w)
            ( :name "Messages with images"
              :query "mime:image/*"
              :key ?p)))

    ;; stolen from doom
    ;; Detect empty subjects, and give users an opotunity to fill something in
    (defun +mu4e-check-for-subject ()
      "Check that a subject is present, and prompt for a subject if not."
      (save-excursion
        (goto-char (point-min))
        (search-forward "--text follows this line--")
        (re-search-backward "^Subject:") ; this should be present no matter what
        (let ((subject (string-trim (substring (thing-at-point 'line) 8))))
          (when (string-empty-p subject)
            (end-of-line)
            (insert (read-string "Subject (optional): "))
            (message "Sending...")))))

    (add-hook 'message-send-hook #'+mu4e-check-for-subject)

;;; Add My Email Accounts
    (set-email-account! "gmail"
                        `((mu4e-sent-folder . "/gmail/Gmail.Sent")
                          (mu4e-drafts-folder . "/gmail/Gmail.Drafts")
                          (mu4e-trash-folder . "/gmail/Gmail.Trash")
                          (mu4e-refile-folder . "/gmail/Gmail.store")
                          ;; Gmail expects me to change labels rather than move stuff?
                          (smtpmail-smtp-server . "smtp.gmail.com")
                          (smtpmail-smtp-service . 587)
                          (smtpmail-stream-type . starttls)
                          (mu4e-sent-messages-behavior . delete)
                          (user-mail-address . "jsilve24@gmail.com")
                          (user-full-name . "Justin Silverman")
                          (smtpmail-smtp-user . "jsilve24@gmail.com")
                          (smtpmail-debug-info t)))


    (set-email-account! "psu"
                        `((mu4e-sent-folder . "/psu/Sent")
                          (mu4e-drafts-folder . "/psu/Drafts")
                          (mu4e-trash-folder . "/psu/Trash")
                          (mu4e-refile-folder . "/psu/Archive")
                          (user-mail-address . "jds6696@psu.edu")
                          (user-full-name . "Justin Silverman")
                          (smtpmail-smtp-user . "jds6696@psu.edu")
                          (smtpmail-smtp-server . "localhost")
                          (smtpmail-stream-type . plain)
                          (smtpmail-smtp-service . 1025)
                          (mu4e-sent-messages-behavior . delete)
                          (smtpmail-debug-info t)))


    ;; turn off mu4e in doom modeline
    (setq doom-modeline-mu4e nil)

    ;; setup calendar
    (setq mu4e-view-use-gnus t)
    (require 'mu4e-icalendar)
    (mu4e-icalendar-setup)
    (setq gnus-icalendar-org-capture-file "~/Dropbox/org/calendar.org")
    (setq gnus-icalendar-org-capture-headline '("Calendar")) ;;make sure to create Calendar heading first
    (gnus-icalendar-org-setup))



;;; setup org-msg
(use-package org-msg
  :straight t
  :after mu4e
  :config
  (setq
  org-msg-options "html-postamble:nil num:nil ^:{} toc:nil author:nil email:nil \\n:t"
  org-msg-startup "hidestars indent inlineimages"
  org-msg-default-alternatives '((new . (text html))
                                 (reply-to-html . (text html))
                                 (reply-to-text . (text)))
  org-msg-convert-citation t
  ;; The default attachment matcher gives too many false positives,
  ;; it's better to be more conservative. See https://regex101.com/r/EtaiSP/4.
  org-msg-attached-file-reference
  "see[ \t\n]\\(?:the[ \t\n]\\)?\\(?:\\w+[ \t\n]\\)\\{0,3\\}\\(?:attached\\|enclosed\\)\\|\
(\\(?:attached\\|enclosed\\))\\|\
\\(?:attached\\|enclosed\\)[ \t\n]\\(?:for\\|is\\)[ \t\n]")
  (org-msg-mode))




;;; bindings

(jds/localleader-def
 :keymaps '(mu4e-compose-mode-map)
 "\\"     '(org-ctrl-c-ctrl-c :which-key "send-message")
 "gs"     '(message-goto-subject :which-key "goto subject")
 "gc"     '(message-goto-cc :which-key "goto cc")
 "gt"     '(message-goto-to :which-key "goto to")
 "k"      '(message-kill-buffer :which-key "kill message")
 "gb"     '(message-goto-body :which-key "goto body"))

(jds/localleader-def
 :keymaps '(org-msg-edit-mode-map)
 "\\"     '(org-ctrl-c-ctrl-c :which-key "send-message")
 "gs"     '(message-goto-subject :which-key "goto subject")
 "gc"     '(message-goto-cc :which-key "goto cc")
 "gt"     '(message-goto-to :which-key "goto to")
 "k"      '(message-kill-buffer :which-key "kill message")
 "gp"     '(jds/org-msg-goto-properties :which-key "goto properties")
 "gb"     '(jds/org-msg-goto-body :which-key "goto body")
 "v"      '(org-msg-preview :which-key "org-msg-preview")
 "t"      '(:ignore t :which-key "toggle")
 "i"      '(jds/org-msg-add-inlineimages :which-key "inlineimages")
 "m"      '(jds/org-msg-add-text2png :which-key "tex2png"))

(jds/localleader-def
 :keymaps '(mu4e-view-mode-map mu4e-headers-mode-map)
 "U"      '(mu4e-update-mail-and-index :which-key "update mail and index")
 "s"      '(mu4e-view-save-attachments))

;; (eval-after-load 'evil-collection-mu4e
;;   (general-define-key
;;    :keymaps ' 
;;    :states       'n
;;    "T"          #'mu4e-headers-mark-thread
;;    "l"          #'+mu4e-capture-msg-to-agenda))


;; ;;; setup thread-folding
;; (use-package mu4e-thread-folding
;;   :straight (mu4e-thread-folding :type git :host github :repo "rougier/mu4e-thread-folding" :branch "master")
;;   :defer t
;;   :config
;;   (add-to-list 'mu4e-header-info-custom
;;                '(:empty . (:name "Empty"
;;                            :shortname ""
;;                            :function (lambda (msg) "  "))))
;;   (setq mu4e-headers-fields '((:empty         .    2)
;;                               (:human-date    .   12)
;;                               (:flags         .    6)
;;                               (:mailing-list  .   10)
;;                               (:from          .   22)
;;                               (:subject       .   nil)))
;; (define-key mu4e-headers-mode-map (kbd "<tab>")     'mu4e-headers-toggle-at-point)
;; (define-key mu4e-headers-mode-map (kbd "<left>")    'mu4e-headers-fold-at-point)
;; (define-key mu4e-headers-mode-map (kbd "<S-left>")  'mu4e-headers-fold-all)
;; (define-key mu4e-headers-mode-map (kbd "<right>")   'mu4e-headers-unfold-at-point)
;; (define-key mu4e-headers-mode-map (kbd "<S-right>") 'mu4e-headers-unfold-all)
;; )



(provide 'email)
;;; email.el ends here
