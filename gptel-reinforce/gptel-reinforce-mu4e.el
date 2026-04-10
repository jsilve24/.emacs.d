;;; gptel-reinforce-mu4e.el --- Mu4e integration for gptel-reinforce -*- lexical-binding: t; -*-

;;; Commentary:

;; Predefined mu4e database and artifact setup for `gptel-reinforce'.

;;; Code:

(require 'subr-x)
(require 'gptel-reinforce)

(declare-function mu4e-message-at-point "mu4e" ())
(declare-function mu4e-message-field "mu4e" (msg field))

(defun gptel-reinforce-mu4e-context ()
  "Return the current mu4e message as reinforcement context."
  (when (and (featurep 'mu4e)
             (derived-mode-p 'mu4e-view-mode 'mu4e-headers-mode))
    (when-let* ((msg (ignore-errors (mu4e-message-at-point))))
      (let ((subject (or (mu4e-message-field msg :subject) ""))
            (from (mu4e-message-field msg :from))
            (mailing-list (mu4e-message-field msg :mailing-list))
            (date (mu4e-message-field msg :date))
            (snippet (or (mu4e-message-field msg :body-txt)
                         (mu4e-message-field msg :snippet)
                         "")))
        (list :item-key (or (mu4e-message-field msg :message-id)
                            (mu4e-message-field msg :maildir))
              :title subject
              :primary-text snippet
              :meta (list :from from
                          :mailing-list mailing-list
                          :date (format "%s" date)
                          :maildir (mu4e-message-field msg :maildir)))))))

(defun gptel-reinforce-register-mu4e-module ()
  "Register the predefined mu4e database and triage-prompt artifact.

Creates a database named \"mu4e-triage\" with a context function that works
in `mu4e-view-mode' and `mu4e-headers-mode'.

Creates an artifact named \"collaborator-triage\" of type \"prompt\".  This
is a passive artifact — the text lives in current.org and is used externally
(e.g. copied into a gptel system prompt).  No hooks are registered."
  (interactive)
  (gptel-reinforce-register-database
   :name "mu4e-triage"
   :context-fn #'gptel-reinforce-mu4e-context
   :db-path (expand-file-name "var/gptel-reinforce/mu4e-triage.sqlite" user-emacs-directory)
   :root-dir (expand-file-name "var/gptel-reinforce/mu4e-triage/" user-emacs-directory)
   :legacy-root-dir "mu4e-triage")
  (gptel-reinforce-register-artifact
   :name "collaborator-triage"
   :database "mu4e-triage"
   :type "prompt"
   :auto-update nil
   :summarizer-system-prompt gptel-reinforce-default-summarizer-prompt
   :summarizer-user-prompt
   "Pay extra attention to collaborators, grants, deadlines, and research planning."
   :updater-system-prompt gptel-reinforce-default-updater-prompt
   :updater-user-prompt
   "Prefer small edits. Preserve the prompt's structure and tone."))

(provide 'gptel-reinforce-mu4e)

;;; gptel-reinforce-mu4e.el ends here
