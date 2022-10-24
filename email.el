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

;; (straight-use-package '(mu4e
;;   ;; :files (:defaults "/usr/local/share/emacs/site-lisp/mu4e/*.el")))
;;   :files (:defaults "mu4e/*.el")))
(use-package mu4e
  :straight (:local-repo "/usr/share/emacs/site-lisp/mu4e"
			 :pre-build ())
  ;; :commands mu4e mu4e-compose-new mu4e-headers-search-bookmark mu4e-get-bookmark-query mu4e~start
  :ensure t
  :init
  (provide 'html2text) ;; disable obsolete package
  :config
  (setq mu4e-get-mail-command "mbsync -a"
	mu4e-change-file-names-when-moving t
	mu4e-compose-context-policy 'ask ;; help separate accounts
	mu4e-context-policy 'pick-first
	;; mu4e-compose-in-new-frame t
	mu4e-sent-messages-behavior 'delete ;; servers take care of this
	mu4e-update-interval 360
	mu4e-headers-include-related nil
	mu4e-confirm-quit nil
	mu4e-view-show-images t
	mu4e-view-show-addresses t
	mu4e-use-maildirs-extension nil
	mu4e-attachment-dir "~/Downloads"
	mu4e-enable-async-operations t
	message-kill-on-buffer-exit t ;; don't keep meesge buffers around
	mu4e-compose-dont-reply-to-self t
	mu4e-view-show-addresses t
	mu4e-hide-index-message t
	mu4e-view-show-images t ;; try to show images
	mu4e-view-use-gnus t
	message-send-mail-function #'smtpmail-send-it
	smtpmail-stream-type 'starttls
	mu4e-completing-read-function #'completing-read
	mu4e-confirm-quit nil

	;; set user-agent
	mail-user-agent 'mu4e-user-agent
	message-mail-user-agent 'mu4e-user-agent

	;; don't show mu4e-main buffer at the same time as messages
	mu4e-split-view 'single-window

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


  ;; nicer header view
  (setq mu4e-headers-fields
	'((:human-date . 12)
	  (:flags . 6)
	  (:from . 22)
	  (:subject . 60)))

  (setq mu4e-bookmarks
	'((:name "Unread messages"
		 :query "flag:unread AND NOT flag:trashed AND (maildir:/gmail/INBOX OR maildir:/psu/INBOX)"
		 :key ?u)
	  (:name "Today's messages"
		 :query "date:today..now AND NOT flag:trashed AND (maildir:/gmail/INBOX OR maildir:/psu/INBOX)"
		 :key ?t)
	  (:name "Last 7 days"
		 :query "date:7d..now AND NOT flag:trashed AND (maildir:/gmail/INBOX OR maildir:/psu/INBOX)"
		 :hide-unread t
		 :key ?w)
	  (:name "Messages with images"
		 :query "mime:image/*"
		 :key ?p)))

  ;; stolen from doom
  ;; Detect empty subjects, and give users an opotunity to fill something in
  (defun +mu4e-check-for-subject ()
    "Check that a subject is present, and prompt for a subject if not."
    (save-excursion
      (goto-char (point-min))
      (search-forward "--text follows this line--")
      (re-search-backward "^Subject:")	; this should be present no matter what
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
  (gnus-icalendar-org-setup)

  ;; simple rebinding
  (evil-collection-define-key 'normal 'mu4e-headers-mode-map
    "F" 'link-hint-open-link)

  ;; better html message handling, make it easier to view by converting to text
  (require 'mu4e-contrib)
  (setq mu4e-html2text-command 'mu4e-shr2text
	shr-color-visible-luminance-min 60
	shr-color-visible-distance-min 5
	shr-use-fonts nil
	shr-use-colors nil)
  (advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))

  ;; clean up modeline
  ;; (defun jds~turn-off-mu4e-modeline ()
  ;; (setq mu4e-context-in-modeline nil
  ;; mu4e~headers-mode-line-label ""))
  ;; (add-hook 'mu4e-headers-mode-hook 'jds~turn-off-mu4e-modeline)

  ;; completely disable mu4e-specific mode-line components
  (defun mu4e~headers-update-mode-line ())
  (defun mu4e-context-in-modeline ())

  ;; start mu4e in background
  ;; (mu4e)
  (mu4e 4))

;;; setup org-msg
(use-package org-msg
  :straight t
  :after mu4e
  :config
  (setq
   org-msg-options "html-postamble:nil num:nil ^:{} toc:nil author:nil email:nil \\n:t tex:dvipng eval:nil"
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

;;;###autoload
(defun jds/mu4e-goto-todays-headers ()
    "Simply function to jump to my ?t (todays messages) headers view."
    (interactive)
    (if (get-buffer "*mu4e-headers*")
	(switch-to-buffer "*mu4e-headers*"))
    (mu4e-headers-search-bookmark
     (mu4e-get-bookmark-query ?t)))


;;; dired - embark -- attach files to messages
(autoload 'gnus-dired-attach "gnus-dired")
(jds/localleader-def
  :keymaps 'dired-mode-map
  "a" #'gnus-dired-attach)

;;;###autoload
(defun embark-attach-file (file)
  "Attach FILE to an  email message.
The message to which FILE is attached is chosen as for `gnus-dired-attach`,
that is: if no message buffers are found a new email is started; if some
message buffer exist you are asked whether you want to start a new email
anyway, if you say no and there is only one message buffer the attachements
are place there, otherwise you are prompted for a message buffer."
  (interactive "fAttach: ")
  (gnus-dired-attach (list file)))

(general-define-key
 :keymaps 'embark-file-map
 "a" #'embark-attach-file)


;;; bindings

(jds/localleader-def
 :keymaps '(mu4e-compose-mode-map)
 "m"     '(org-ctrl-c-ctrl-c :which-key "send-message")
 "gs"     '(message-goto-subject :which-key "goto subject")
 "gc"     '(message-goto-cc :which-key "goto cc")
 "gt"     '(message-goto-to :which-key "goto to")
 "k"      '(message-kill-buffer :which-key "kill message")
 "gb"     '(message-goto-body :which-key "goto body"))

(jds/localleader-def
  :keymaps '(org-msg-edit-mode-map)
  "m" '(org-ctrl-c-ctrl-c :which-key "send-message")
  "C-m" '(org-ctrl-c-ctrl-c :which-key "send-message")
  "gs" '(message-goto-subject :which-key "goto subject")
  "gc" '(message-goto-cc :which-key "goto cc")
  "gt" '(message-goto-to :which-key "goto to")
  "k" '(message-kill-buffer :which-key "kill message")
  "gp" '(jds/org-msg-goto-properties :which-key "goto properties")
  "gb" '(jds/org-msg-goto-body :which-key "goto body")
  "v" 'org-msg-preview
  "t" '(:ignore t :which-key "toggle")
  "i" '(jds/org-msg-add-inlineimages :which-key "inlineimages")
  ;; "m" '(jds/org-msg-add-text2png :which-key "tex2png")
  )

(jds/localleader-def
 :keymaps '(mu4e-view-mode-map mu4e-headers-mode-map)
 "u"      '(mu4e-update-mail-and-index :which-key "update mail and index")
 "s"      '(mu4e-view-save-attachments :which-key "save-attachments")
 "T"          #'mu4e-headers-mark-thread
 "l"          #'+mu4e/capture-msg-to-agenda)

(with-eval-after-load 'link-hint

  ;; :next function should not move the point
  ;;    it should talk one optional argument that is an end bound
  (defun jds~mu4e-headers-next-for-link-hint (&optional bound jump)
    "Function wrapping mu4e-headers-next and mathing spect for link-hint :next function. If `jump` then will also move to next message."
    (let ((ret (save-excursion
		 (progn
		   (mu4e-headers-next)
		   (if (or (and bound
				(> (point) bound))
			   (progn
			     (beginning-of-line)
			     (looking-at-p "^End of search results.*")))
		       nil
		     (point))))))
      (if jump
	  (if ret
	      (goto-char ret))
	ret)))

  (defun jds~mu4e-at-point-p ()
    "Function suitable for :at-point-p in link-hint for mu4e. Return message id to pass to
mu4e-headers-goto-message-id."
    (let* ((map (and
		 (pos-visible-in-window-p)
		 (mu4e-message-at-point t)))
	   (msgid (if (not map)
		      nil
		    (plist-get map :message-id))))
      msgid))

  (link-hint-define-type 'mu4e-message
    :next #'jds~mu4e-headers-next-for-link-hint
    :at-point-p #'jds~mu4e-at-point-p
    :open #'mu4e-headers-view-message
    :copy #'mu4e-copy-message-path
    :vars '(mu4e-headers-mode))
  (push 'link-hint-mu4e-message link-hint-types)

  (defun jds~avy-mu4e-header-cands ()
    (interactive)
    (save-excursion
      (save-restriction
	(narrow-to-region (window-start) (window-end (selected-window) t))
	(goto-char (point-min))
	(setq pt (point))
	(jds~mu4e-headers-next-for-link-hint nil t)
	(beginning-of-line)
	(let ((candidates (list (cons (point) (selected-window)))))
	  (while (not (equal pt (point)))
	    (setq pt (point))
	    (end-of-line)
	    (jds~mu4e-headers-next-for-link-hint nil t)
	    (beginning-of-line)
	    (push (cons (point) (selected-window)) candidates))
	  (nreverse candidates)))))

  (defun jds/avy-mu4e-header ()
    "Goto a visible message header in mu4e using avy."
    (interactive)
    (avy-action-goto (avy-with jds/avy-ibuffer
		       (avy-process (jds~avy-mu4e-header-cands))))))

(general-define-key
 :states '(n v m)
 :keymaps 'mu4e-headers-mode-map
 "f" #'jds/avy-mu4e-header
 "F" #'link-hint-open-link)


(provide 'email)
;;; email.el ends here

