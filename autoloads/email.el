;;; email.el --- mostly stolen from doom -*- lexical-binding: t; -*-

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


;; ;;;###autoload
;; (defun jds/capture-mu4e-message ()
;;   "Quickly open email capture template when looking at a mu4e message."
;;   (interactive)
;;   (call-interactively 'org-store-link)
;;   (org-capture nil "e"))


;;; not stolen from doom

;; better handling of mu4e links in org

;; better handling of mu4e links
;;https://mu-discuss.narkive.com/GxjFjuhz/open-org-mode-link-to-mu4e-message-in-new-frame
;; ;;;###autoload
;; (defun jds/mu4e-follow-link-new-frame-window ()
;;   "Follow mu4e link in org-mode open in new frame.
;;    Open in new window if universal prefix passed."
;;   (interactive)
;;   (if current-prefix-arg
;;       (jds~with-temp-advice 'mu4e-view-message-with-message-id
;;                             :before (lambda (MSGID) (select-window (split-window-sensibly)))
;;                             (+org/dwim-at-point))
;;       (jds~with-temp-advice 'mu4e-view-message-with-message-id
;;                             :before (lambda (MSGID) (select-frame (make-frame)))
;;                             (+org/dwim-at-point))))

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
              (insert (concat " Respond to "
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



;;; mu4e-view-save-all-attachments.el -- savel all attachments from view mode
;; Stephen J Eglen 2021


;; I've created this based on the work of Phil Jackson that required
;; an older version of mu4e.  This version requires the GNUS article
;; code for reading mu4e messages.
;; https://gist.github.com/philjackson/aecfab1706f05079aec7000e328fd183

;; Suggested keybinding
;;  mnemnonic: > is to redirect the files to output everything.
;; (define-key mu4e-view-mode-map ">" 'mu4e-view-save-all-attachments)

(with-eval-after-load 'mu4e
 (defvar bulk-saved-attachments-dir mu4e-attachment-dir))

;;;###autoload
(defun cleanse-subject (sub)
  (replace-regexp-in-string
   "[^A-Z0-9]+"
   "-"
   (downcase sub)))

;;;###autoload
(defun mu4e-view-save-all-attachments (&optional arg)
  "Save all MIME parts from current mu4e gnus view buffer."
  ;; Copied from mu4e-view-save-attachments
  (interactive "P")
  (cl-assert (and (eq major-mode 'mu4e-view-mode)
                  (derived-mode-p 'gnus-article-mode)))
  (let* ((msg (mu4e-message-at-point))
         (id (cleanse-subject (mu4e-message-field msg :subject)))
         (attachdir (concat bulk-saved-attachments-dir "/" id))
	 (parts (mu4e~view-gather-mime-parts))
         (handles '())
         (files '())
         dir)
    (mkdir attachdir t)
    (dolist (part parts)
      (let ((fname (or 
		    (cdr (assoc 'filename (assoc "attachment" (cdr part))))
                    (seq-find #'stringp
                              (mapcar (lambda (item) (cdr (assoc 'name item)))
                                      (seq-filter 'listp (cdr part)))))))
        (when fname
          (push `(,fname . ,(cdr part)) handles)
          (push fname files))))
    (if files
        (progn
          (setq dir
		(if arg (read-directory-name "Save to directory: ")
		  attachdir))
          (cl-loop for (f . h) in handles
                   when (member f files)
                   do (mm-save-part-to-file h
					    (sje-next-free
					     (expand-file-name f dir)))))
      (mu4e-message "No attached files found"))))


;;;###autoload
(defun jds/mu4e-compose-reply (&optional no-wide)
  "Reply to the message at point.
If NO-WIDE is nil, make it a \"wide\" reply (a.k.a.
\"reply-to-all\")."
  (interactive "P")
  (mu4e--compose-setup
   'reply
   (lambda (parent)
     (insert (mu4e--decoded-message parent 'headers-only))
     (if no-wide
	 (setq wide nil)
       (setq wide t))
     (message-reply nil wide)
     (message-goto-body)
     (insert (mu4e--compose-cite parent)))))


;;;###autoload
(defun sje-next-free (file)
  "Return name of next unique 'free' FILE.
If /tmp/foo.txt and /tmp/foo-1.txt exist, when this is called
with /tmp/foo.txt, return /tmp/foo-2.txt.  See
`sje-test-next-free' for a test case.  This is not very efficient
if there are a large number of files already in the directory
with the same base name, as it simply starts searching from 1
each time until it finds a gap.  An alternative might be to do a
wildcard search for all the filenames, extract the highest number
and then increment it."
  ;; base case is easy; does file exist already?
  (if (not  (file-exists-p file))
      file
    ;; othwerwise need to iterate through f-1.pdf
    ;; f-2.pdf, f-3.pdf ... until we no longer find a file.
    (let ((prefix (file-name-sans-extension file))
	  (suffix (file-name-extension file))
	  (looking t)
	  (n 0)
	  (f)
	  )
      (while looking
	(setq n (1+ n))
	(setq f (concat prefix "-" (number-to-string n) "." suffix))
	(setq looking (file-exists-p f)))
      f
      )))


;;;###autoload
(defun sje-test-next-free ()
  (let (f)
    (dotimes (i 100)
      (setq f (sje-next-free "/tmp/rabbit.txt"))
      (write-region "hello" nil f)
      )))
;; (sje-test-next-free)
