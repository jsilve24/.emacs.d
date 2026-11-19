;;; email-helpers.el --- email helper commands -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defvar mu4e-compose-mode-hook nil)
(defvar mu4e-compose-parent-message nil)
(defvar message-forward-as-mime)
(defvar message-forward-show-mml)
(defvar message-forward-ignored-headers)
(defvar message-forward-included-headers)
(declare-function mu4e-compose-forward "mu4e-compose")

;; keep byte-compiler and autoloaded helpers happy when this file loads before mu4e
(declare-function mu4e-message-at-point "mu4e-message")
(declare-function mu4e-headers-mark-for-move "mu4e-headers")

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
      (when-let* ((address (cdr (assq 'user-mail-address letvars))))
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
                                      ;; (mu4e-clear-caches)
				      ))
                    :match-func
                    (lambda (msg)
                      (when msg
                        (string-prefix-p (format "/%s" label)
                                         (mu4e-message-field msg :maildir) t)))
                    :vars letvars)))
      (add-to-list 'mu4e-contexts context (not default-p))
      context))))


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
(defun jds/mu4e--junk-target-for-message (msg)
  "Return the account-specific junk maildir for MSG, or nil."
  (let ((maildir (plist-get msg :maildir)))
    (cond
     ((and (stringp maildir) (string-prefix-p "/gmail/" maildir))
      "/gmail/Gmail.spam")
     ((and (stringp maildir) (string-prefix-p "/psu/" maildir))
      "/psu/Junk")
     (t nil))))

;;;###autoload
(defun jds/mu4e-mark-junk ()
  "Mark the current message for move to its account-specific junk folder.
Gmail messages go to `/gmail/Gmail.spam` and PSU messages go to
`/psu/Junk`. This only creates a move mark; Mu4e still asks for
confirmation when the marks are executed. Using a move mark keeps the
server-side move visible to IMAP sync, which can help train spam filters."
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (target (and msg (jds/mu4e--junk-target-for-message msg))))
    (unless target
      (user-error "No junk-folder rule for %s"
                  (or (plist-get msg :maildir) "this message")))
    (when (string= (plist-get msg :maildir) target)
      (user-error "Message is already in %s" target))
    (cl-letf (((symbol-function 'mu4e--mark-get-move-target)
               (lambda () target)))
      (mu4e-headers-mark-for-move))
    (message "Marked for move to %s" target)))



;; below is stolen from doom
(defvar +org-capture-emails-file "mail.org"
  "Default target for storing mu4e emails captured from within mu4e.
Requires a \"* Email\" heading be present in the file.")

;; Adding emails to the agenda
;; Perfect for when you see an email you want to reply to
;; later, but don't want to forget about
;;;###autoload
(defun +mu4e/capture-msg-to-agenda (arg)
  "Refile a message and add a entry in `+org-capture-emails-file' with a
Scheduled timestamp.  Default schedule is today.  With one prefix, schedule for
for tomorrow.  With two prefixes, select the schedule date."
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
              (insert (concat "Respond to "
                              "[[mu4e:msgid:"
                              (plist-get msg :message-id) "]["
                              (truncate-string-to-width
			       ;; following line was modified by JDS after upgrading mu4e
                               (or (plist-get (car from) :name) (plist-get (car from) :email)) 25 nil nil t)
                              " - "
                              (truncate-string-to-width
                               (plist-get msg :subject) 38 nil nil t)
                              "]] "))
	      ;; can just change this to org-deadline to change to deadline from scheduled
              (org-schedule nil
                            (cond ((= arg 1) (format-time-string "%Y-%m-%d"))
                                  ((= arg 4) "+1d")))

              (org-update-parent-todo-statistics)
              (jds/org-capture-set-last-stored-marker)

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





;;; org-agenda reply helper -------------------------------------------------

(defvar jds~mu4e-reply-pending nil
  "When non-nil, compose a reply as soon as the next mu4e view buffer is ready.
Set by `jds/org-agenda-reply-to-email-and-done'.")

;;;###autoload
(defun jds~mu4e-maybe-compose-reply-hook ()
  "Compose a reply if `jds~mu4e-reply-pending' is set.
Intended for `mu4e-view-mode-hook'."
  (when jds~mu4e-reply-pending
    (setq jds~mu4e-reply-pending nil)
    (run-with-idle-timer 0.1 nil #'jds/mu4e-compose-reply)))

;;;###autoload
(defun jds/mu4e-compose-reply (&optional no-wide)
  "Reply to the message at point.
If NO-WIDE is nil, make it a \"wide\" reply (a.k.a.
\"reply-to-all\")."
  (interactive "P")
  (if no-wide
      (mu4e-compose-reply-to nil nil)
    (mu4e-compose-reply-to nil t))
  )

;;; Maple delegation ------------------------------------------------------

(defconst jds/mu4e-maple-address "maple.silverman.assistant@gmail.com"
  "Address of Maple, Justin's digital assistant.")

(defvar jds/mu4e-preserve-forward-mime nil
  "Non-nil skips the rendered-text forwarding workaround for one forward.")

;;;###autoload
(defun jds/mu4e-maple-add-cc ()
  "Add Maple to the current draft's Cc without sending it.
Preserve other recipients and do nothing if Maple is already in To or Cc.
From headers or message view, start a reply-all draft with Maple in Cc."
  (interactive)
  (if (derived-mode-p 'mu4e-headers-mode 'mu4e-view-mode)
      (jds/mu4e-maple-reply)
    (unless (derived-mode-p 'message-mode 'org-msg-edit-mode)
      (user-error "Use this command in a mail composition buffer"))
    (require 'message)
    (require 'mail-extr)
    (save-excursion
      (save-restriction
        (widen)
        (let* ((cc (save-restriction
                     (message-narrow-to-headers)
                     (message-field-value "Cc" t)))
               (addresses
                (save-restriction
                  (message-narrow-to-headers)
                  (mail-extract-address-components
                   (string-join (delq nil (list (message-field-value "To" t) cc)) ", ")
                   t))))
          (if (cl-some (lambda (address)
                         (string-equal (downcase (or (cadr address) ""))
                                       jds/mu4e-maple-address))
                       addresses)
              (message "Maple is already a recipient")
            ;; message-replace-header locates the header separator itself.
            (message-replace-header
             "Cc" (concat (unless (string-empty-p (or cc "")) (concat cc ", "))
                          "Maple <" jds/mu4e-maple-address ">"))
            (message "Added Maple to Cc; draft not sent")))))))

;;;###autoload
(defun jds/mu4e-maple-reply ()
  "Open a reply-all draft to the message at point and Cc Maple.
Use the existing reply and account-selection flow; never send automatically.
In a composition buffer, only add Maple to Cc, retaining the draft and its
current recipients rather than rebuilding a reply or discarding edits."
  (interactive)
  (if (derived-mode-p 'message-mode 'org-msg-edit-mode)
      (jds/mu4e-maple-add-cc)
    (unless (derived-mode-p 'mu4e-headers-mode 'mu4e-view-mode)
      (user-error "Use this command in Mu4e headers or message view"))
    (let ((draft (jds/mu4e-compose-reply)))
      (unless (buffer-live-p draft)
	(user-error "Mu4e did not return a reply draft; no message was sent"))
      (with-current-buffer draft
	(jds/mu4e-maple-add-cc)))))

(defun jds/mu4e-maple-format-contacts (contacts)
  "Format Mu4e CONTACTS for the visible delegation summary."
  (mapconcat (lambda (contact)
               (let ((name (plist-get contact :name))
                     (email (or (plist-get contact :email) "")))
                 (if (and name (not (string-empty-p name)))
                     (format "%s <%s>" name email)
                   email)))
             contacts ", "))

;;;###autoload
(defun jds/mu4e-maple-forward ()
  "Open a forward to Maple, preserving the original message as MIME.
The draft remains editable and unsent.  Unlike the ordinary text-only
forward workaround, this retains original headers and attachments.  This
one draft uses plain Message composition, not OrgMsg, so MIME markers stay
intact.  From an existing reply/forward draft, forward its original message
in a new buffer and leave the unsent draft unchanged.  A new draft without
an identified original cannot be forwarded with this command.  Existing account selection and ordinary forwards are unchanged."
  (interactive)
  (unless (derived-mode-p 'mu4e-headers-mode 'mu4e-view-mode
                          'message-mode 'org-msg-edit-mode)
    (user-error "Use this command in Mu4e or a mail composition buffer"))
  (require 'message)
  (let* ((composing (derived-mode-p 'message-mode 'org-msg-edit-mode))
         (parent (if composing
                     (and (boundp 'mu4e-compose-parent-message)
                          mu4e-compose-parent-message)
                   (mu4e-message-at-point)))
         (_ (unless (and parent (plist-get parent :message-id))
              (user-error "No original message identified; current draft left unchanged")))
         (jds/mu4e-preserve-forward-mime t)
         (message-forward-as-mime t)
         (message-forward-show-mml nil)
         (message-forward-ignored-headers nil)
         (message-forward-included-headers nil)
         ;; A dynamic hook binding affects only this synchronous compose call.
         ;; Do not toggle global OrgMsg mode or change unrelated drafts.
         (mu4e-compose-mode-hook
          (remove #'org-msg-post-setup mu4e-compose-mode-hook))
         ;; In a draft, forward its original message, never the unsent edits.
         ;; Mu4e's public forward command gets that source via message-at-point.
         (draft (if composing
                    (cl-letf (((symbol-function 'mu4e-message-at-point)
                               (lambda (&rest _args) parent)))
                      (mu4e-compose-forward))
                  (mu4e-compose-forward))))
    (unless (buffer-live-p draft)
      (user-error "Mu4e did not return a forward draft; no message was sent"))
    (with-current-buffer draft
      (save-restriction
        (widen)
        (save-excursion
          (message-replace-header "To" (concat "Maple <" jds/mu4e-maple-address ">"))
          (save-restriction
            (message-narrow-to-headers)
            (message-remove-header "Cc")
            (message-remove-header "Bcc")))
        (message-goto-body)
        (let ((instructions (point)))
          (insert "\n\nOriginal message (full email and attachments forwarded below):\n"
                  "From: " (jds/mu4e-maple-format-contacts (plist-get parent :from)) "\n"
                  "To: " (jds/mu4e-maple-format-contacts (plist-get parent :to)) "\n"
                  "Cc: " (jds/mu4e-maple-format-contacts (plist-get parent :cc)) "\n"
                  "Date: " (if-let ((date (plist-get parent :date)))
                               (format-time-string "%a, %d %b %Y %T %z" date) "") "\n"
                  "Subject: " (or (plist-get parent :subject) "") "\n"
                  "Message-ID: " (or (plist-get parent :message-id) "") "\n\n")
          (goto-char instructions))))
    (message "Forward to Maple ready; add instructions, review, then send")
    draft))

(provide 'email-helpers)
