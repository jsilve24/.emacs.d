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


(provide 'email)
;;; email.el ends here
