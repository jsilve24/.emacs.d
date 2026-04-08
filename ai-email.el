;;; ai-email.el --- AI-powered email drafting via gptel -*- lexical-binding: t -*-

;;; Calendar search tool ---------------------------------------------------

(defun jds/org-calendar-search (start-date end-date)
  "Search org-agenda-files for active timestamps between START-DATE and END-DATE (YYYY-MM-DD).
Returns entries with full heading path so AI can assess priority and movability."
  (require 'org-agenda)
  (let ((all-results '())
        (start-time (date-to-time (concat start-date " 00:00:00")))
        (end-time   (date-to-time (concat end-date   " 23:59:59"))))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        ;; Reuse already-open buffer to avoid redundant file I/O
        (let ((buf (or (find-buffer-visiting file)
                       (find-file-noselect file t))))
          (with-current-buffer buf
            (let ((seen '()))
              (org-with-wide-buffer
               (goto-char (point-min))
               (while (re-search-forward org-ts-regexp nil t)
                 (let* ((ts      (match-string 0))
                        (ts-time (ignore-errors (org-time-string-to-time ts))))
                   (when (and ts-time
                              (not (time-less-p ts-time start-time))
                              (not (time-less-p end-time ts-time)))
                     (ignore-errors
                       (org-back-to-heading t)
                       (let ((hpos (point)))
                         (unless (member hpos seen)
                           (push hpos seen)
                           (let* ((heading  (org-get-heading t t t t))
                                  (todo     (org-get-todo-state))
                                  (path     (org-get-outline-path))
                                  (fname    (file-name-nondirectory file))
                                  (path-str (if path
                                               (mapconcat #'identity path " > ")
                                             fname)))
                             (push (format "- [%s] %s%s | %s | path: %s"
                                           ts
                                           (if todo (concat todo ": ") "")
                                           heading
                                           fname
                                           path-str)
                                   all-results)))))))))))))
    (if all-results
        (concat (format "Calendar entries %s → %s:\n" start-date end-date)
                (mapconcat #'identity (nreverse all-results) "\n"))
      (format "No calendar entries found from %s to %s." start-date end-date))))

(defvar jds~gptel-calendar-tool
  (gptel-make-tool
   :name "search_calendar"
   :description "Search the org-mode calendar for scheduled events, meetings, and deadlines within a date range. Each result includes the event title, timestamp, source file, and full heading path (breadcrumb) to help assess importance and movability."
   :args (list '(:name "start_date" :type string :description "Start date in YYYY-MM-DD format")
               '(:name "end_date"   :type string :description "End date in YYYY-MM-DD format"))
   :function #'jds/org-calendar-search)
  "GPtel tool for searching the org-mode calendar.")


;;; AI email reply ---------------------------------------------------------

(defun jds/mu4e-ai-draft-reply ()
  "Reply to message at point with an AI-drafted body."
  (interactive)
  (let* ((msg      (mu4e-message-at-point))
         (from-c   (car (mu4e-message-field msg :from)))
         (from-str (or (plist-get from-c :name) (plist-get from-c :email) "Unknown"))
         (subject  (or (mu4e-message-field msg :subject) "(no subject)"))
         hook-fn)
    (setq hook-fn
          (lambda ()
            (remove-hook 'mu4e-compose-mode-hook hook-fn)
            (let* ((buf     (current-buffer))
                   (content (buffer-substring-no-properties (point-min) (point-max)))
                   (prompt  (format "Draft a professional reply to this email from %s (subject: \"%s\").\nCompose buffer (includes quoted original):\n\n%s\n\nReturn only the reply body text."
                                    from-str subject content)))
              (message-goto-body)
              (let ((pos (copy-marker (point))))
                (gptel-request prompt
                  :system "You are a professional email assistant. Write clear, concise replies. Return only the reply body — no subject line, no explanatory text."
                  :buffer buf
                  :callback (lambda (response info)
                              (if (not response)
                                  (message "gptel error: %s" (plist-get info :status))
                                (with-current-buffer buf
                                  (goto-char pos)
                                  (insert response "\n\n")
                                  (set-marker pos nil)))))))))
    (add-hook 'mu4e-compose-mode-hook hook-fn)
    (jds/mu4e-compose-reply)))


;;; AI scheduling reply ----------------------------------------------------

(defun jds/mu4e-ai-scheduling-reply ()
  "Reply to message at point with an AI-drafted scheduling response.
Prompts for custom context. Uses the org calendar tool to check availability."
  (interactive)
  (let* ((ctx      (read-string "Scheduling context (priority, duration, notes): "))
         (msg      (mu4e-message-at-point))
         (from-c   (car (mu4e-message-field msg :from)))
         (from-str (or (plist-get from-c :name) (plist-get from-c :email) "Unknown"))
         (subject  (or (mu4e-message-field msg :subject) "(no subject)"))
         (today    (format-time-string "%Y-%m-%d (%A, %B %d, %Y)"))
         (system   (concat "You are a scheduling assistant helping to respond to emails about meetings.\n"
                           "Today is " today ".\n\n"
                           "My scheduling preferences:\n"
                           "- Tuesdays & Thursdays: on campus — prefer in-person meetings.\n"
                           "- Mondays, Wednesdays & Fridays: remote — prefer Zoom meetings.\n\n"
                           "Use the search_calendar tool to check for existing commitments before proposing times.\n"
                           "Return only the email body text, professional and concise."))
         hook-fn)
    (setq hook-fn
          (lambda ()
            (remove-hook 'mu4e-compose-mode-hook hook-fn)
            (let* ((buf     (current-buffer))
                   (content (buffer-substring-no-properties (point-min) (point-max)))
                   (prompt  (format "Draft a scheduling reply for this email from %s (subject: \"%s\").%s\nCompose buffer (includes quoted original):\n\n%s\n\nReturn only the reply body."
                                    from-str subject
                                    (if (string-empty-p ctx) ""
                                      (format "\nContext: %s" ctx))
                                    content)))
              (message-goto-body)
              (let ((pos (copy-marker (point))))
                (let ((gptel-use-tools t))
                  (gptel-request prompt
                    :system system
                    :tools  (list jds~gptel-calendar-tool)
                    :buffer buf
                    :callback (lambda (response info)
                                (if (not response)
                                    (message "gptel error: %s" (plist-get info :status))
                                  (with-current-buffer buf
                                    (goto-char pos)
                                    (insert response "\n\n")
                                    (set-marker pos nil))))))))))
    (add-hook 'mu4e-compose-mode-hook hook-fn)
    (jds/mu4e-compose-reply)))


;;; Keybindings ------------------------------------------------------------

;; ca = compose with AI draft
;; cA = compose with AI scheduling assistant
(evil-collection-define-key 'normal 'mu4e-headers-mode-map
  "ca" #'jds/mu4e-ai-draft-reply
  "cA" #'jds/mu4e-ai-scheduling-reply)

(evil-collection-define-key 'normal 'mu4e-view-mode-map
  "ca" #'jds/mu4e-ai-draft-reply
  "cA" #'jds/mu4e-ai-scheduling-reply)

(provide 'ai-email)
;;; ai-email.el ends here
