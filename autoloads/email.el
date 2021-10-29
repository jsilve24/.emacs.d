;;; email.el --- mostly stolen from doom -*- lexical-binding: t; -*-
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
;;  mostly stolen from doom
;;
;;; Code:

;; stolen from doom
;;;###autodef
(defun set-email-account! (label letvars &optional default-p)
  "Registers an email address for mu4e. The LABEL is a string. LETVARS are a
list of cons cells (VARIABLE . VALUE) -- you may want to modify:
 + `user-full-name' (used to populate the FROM field when composing mail)
 + `user-mail-address' (required in mu4e < 1.4)
 + `smtpmail-smtp-user' (required for sending mail from Emacs)
OPTIONAL:
 + `mu4e-sent-folder'
 + `mu4e-drafts-folder'
 + `mu4e-trash-folder'
 + `mu4e-refile-folder'
 + `mu4e-compose-signature'
 + `+mu4e-personal-addresses'
DEFAULT-P is a boolean. If non-nil, it marks that email account as the
default/fallback account."
  (eval-after-load 'mu4e
    (progn
    (when (version< mu4e-mu-version "1.4")
      (when-let (address (cdr (assq 'user-mail-address letvars)))
        (add-to-list 'mu4e-user-mail-address-list address)))
    ;; remove existing context with same label
    (setq mu4e-contexts
          (cl-loop for context in mu4e-contexts
                   unless (string= (mu4e-context-name context) label)
                   collect context))
    (let ((context (make-mu4e-context
                    :name label
                    :enter-func
                    (lambda () (mu4e-message "Switched to %s" label))
                    :leave-func
                    (lambda () (progn (setq +mu4e-personal-addresses nil)
                                      (mu4e-clear-caches)))
                    :match-func
                    (lambda (msg)
                      (when msg
                        (string-prefix-p (format "/%s" label)
                                         (mu4e-message-field msg :maildir) t)))
                    :vars letvars)))
      (add-to-list 'mu4e-contexts context (not default-p))
      context))))



;; Adding emails to the agenda
;; Perfect for when you see an email you want to reply to
;; later, but don't want to forget about
;;;###autoload
(defun +mu4e/capture-msg-to-agenda (arg)
  "Refile a message and add a entry in `+org-capture-emails-file' with a
deadline.  Default deadline is today.  With one prefix, deadline
is tomorrow.  With two prefixes, select the deadline."
  (interactive "p")
  (let ((sec "^* Email")
        (msg (mu4e-message-at-point)))
    (when msg
      ;; put the message in the agenda
      (with-current-buffer (find-file-noselect
                            (expand-file-name +org-capture-emails-file org-directory))
        (save-excursion
          ;; find header section
          (goto-char (point-min))
          (when (re-search-forward sec nil t)
            (let (org-M-RET-may-split-line
                  (lev (org-outline-level))
                  (folded-p (invisible-p (point-at-eol)))
                  (from (plist-get msg :from)))
              ;; place the subheader
              (when folded-p (show-branches))    ; unfold if necessary
              (org-end-of-meta-data) ; skip property drawer
              (org-insert-todo-heading 1)        ; insert a todo heading
              (when (= (org-outline-level) lev)  ; demote if necessary
                (org-do-demote))
              ;; insert message and add deadline
              (insert (concat " Respond to "
                              "[[mu4e:msgid:"
                              (plist-get msg :message-id) "]["
                              (truncate-string-to-width
                               (or (caar from) (cadr from)) 25 nil nil t)
                              " - "
                              (truncate-string-to-width
                               (plist-get msg :subject) 40 nil nil t)
                              "]] "))
              (org-deadline nil
                            (cond ((= arg 1) (format-time-string "%Y-%m-%d"))
                                  ((= arg 4) "+1d")))

              (org-update-parent-todo-statistics)

              ;; refold as necessary
              (if folded-p
                  (progn
                    (org-up-heading-safe)
                    (hide-subtree))
                (hide-entry))))))
      ;; refile the message and update
      ;; (cond ((eq major-mode 'mu4e-view-mode)
      ;;        (mu4e-view-mark-for-refile))
      ;;       ((eq major-mode 'mu4e-headers-mode)
      ;;        (mu4e-headers-mark-for-refile)))
      (message "Refiled \"%s\" and added to the agenda for %s"
               (truncate-string-to-width
                (plist-get msg :subject) 40 nil nil t)
               (cond ((= arg 1) "today")
                     ((= arg 4) "tomorrow")
                     (t         "later"))))))




;;; not stolen from doom

;; better handling of mu4e links in org

;; better handling of mu4e links
;;;###autoload
;;https://emacs.stackexchange.com/questions/54500/how-to-add-a-locally-override-the-message-function
(defmacro jds~with-temp-advice (fn-orig where fn-advice &rest body)
  "Execute BODY with advice temporarily enabled."
  `(let ((fn-advice-var ,fn-advice))
    (unwind-protect
      (progn
        (advice-add ,fn-orig ,where fn-advice-var)
        ,@body)
      (advice-remove ,fn-orig fn-advice-var))))


;;https://mu-discuss.narkive.com/GxjFjuhz/open-org-mode-link-to-mu4e-message-in-new-frame
;;;###autoload
(defun jds/mu4e-follow-link-new-frame-window ()
  "Follow mu4e link in org-mode open in new frame.
   Open in new window if universal prefix passed."
  (interactive)
  (if current-prefix-arg
      (jds~with-temp-advice 'mu4e-view-message-with-message-id
                            :before (lambda (MSGID) (select-window (split-window-sensibly)))
                            (+org/dwim-at-point))
      (jds~with-temp-advice 'mu4e-view-message-with-message-id
                            :before (lambda (MSGID) (select-frame (make-frame)))
                            (+org/dwim-at-point))))

;;;###autoload
(defun jds/mu4e-compose-goto-to ()
  "Open new compose window and goto to field."
  (interactive)
  (progn (mu4e-compose-new)
         (message-goto-to)))

;;;###autoload
(defun jds/message-goto-in-body (regex &optional next)
  "Call message-goto-body if NEXT not nil then move to line after REGEX. Otherwise move to begginning of matching line."
  (progn (beginning-of-buffer)
         (message-goto-body)
         (re-search-forward  regex  nil t)
         (if (not (equal next nil)) (next-line) (beginning-of-line))))

;;;###autoload
(defun jds/org-msg-goto-body ()
  "Call message-goto-body then move to next line after :END:."
  (interactive)
  (jds/message-goto-in-body ":END:" t))

;;;###autoload
(defun jds/org-msg-goto-properties ()
  "Call message-goto-body then move to start of :PROPERTIES: line."
  (interactive)
  (jds/message-goto-in-body ":PROPERTIES:" nil))

;;;###autoload
(defun jds/org-msg-add-tex2png ()
  "Add tex:dvipng option to org-msg buffer."
  (interactive)
  (save-excursion
    (jds/message-goto-in-body "#\+OPTIONS:" nil)
    (end-of-line)
    (insert " tex:dvipng")))


;;;###autoload
(defun jds/org-msg-add-inlineimages ()
  "Add tex:dvipng option to org-msg buffer."
  (interactive)
  (save-excursion
    (jds/message-goto-in-body "#\+OPTIONS:" nil)
    (end-of-line)
    (insert " inlineimages")))

;;;###autoload
(defun jds/open-mu4e-new-frame ()
  (interactive)
  (select-frame (make-frame))
  (mu4e))

(provide 'email)
;;; email.el ends here
