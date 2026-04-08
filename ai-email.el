;;; ai-email.el --- AI-powered email drafting via gptel -*- lexical-binding: t -*-

;;; Calendar search tool ---------------------------------------------------

(require 'org)
(require 'org-element)
(require 'subr-x)
(require 'cl-lib)

(defvar jds/org-calendar-cache (make-hash-table :test #'equal)
  "Cache of parsed calendar events by file and modification time.")

(defun jds/org-calendar--file-mtime (file)
  (file-attribute-modification-time (file-attributes file)))

(defun jds/org-calendar--timestamp-to-time-range (ts)
  "Return (START . END) time values for org-element timestamp TS."
  (let* ((syear  (org-element-property :year-start ts))
         (smonth (org-element-property :month-start ts))
         (sday   (org-element-property :day-start ts))
         (shour  (or (org-element-property :hour-start ts) 0))
         (smin   (or (org-element-property :minute-start ts) 0))
         (eyear  (or (org-element-property :year-end ts) syear))
         (emonth (or (org-element-property :month-end ts) smonth))
         (eday   (or (org-element-property :day-end ts) sday))
         (ehour  (or (org-element-property :hour-end ts) shour))
         (emin   (or (org-element-property :minute-end ts) smin)))
    (cons (encode-time 0 smin shour sday smonth syear)
          (encode-time 0 emin ehour eday emonth eyear))))

(defun jds/org-calendar--time-overlaps-p (a-start a-end b-start b-end)
  "Whether [A-START, A-END] overlaps [B-START, B-END]."
  (not (or (time-less-p a-end b-start)
           (time-less-p b-end a-start))))

(defun jds/org-calendar--headline-path (hl)
  "Return outline path string for headline element HL."
  (let ((titles '())
        (cur hl))
    (while cur
      (when (eq (org-element-type cur) 'headline)
        (push (org-element-property :raw-value cur) titles))
      (setq cur (org-element-property :parent cur)))
    (string-join titles " > ")))

(defun jds/org-calendar--parse-file-events (file)
  "Return cached list of calendar event plists for FILE."
  (let* ((mtime  (jds/org-calendar--file-mtime file))
         (key    (list file mtime))
         (cached (gethash key jds/org-calendar-cache 'missing)))
    (if (not (eq cached 'missing))
        cached
      ;; Invalidate older entries for this file
      (let ((stale-keys nil))
        (maphash (lambda (k _) (when (equal (car k) file) (push k stale-keys)))
                 jds/org-calendar-cache)
        (dolist (k stale-keys) (remhash k jds/org-calendar-cache)))
      (let ((events nil))
        (with-current-buffer (or (find-buffer-visiting file)
                                 (find-file-noselect file))
          (unless (derived-mode-p 'org-mode)
            (org-mode))
          (org-with-wide-buffer
           (let ((ast (org-element-parse-buffer)))
             (org-element-map ast 'timestamp
               (lambda (ts)
                 (let* ((ts-type  (org-element-property :type ts))
                        (parent   (org-element-property :parent ts))
                        (headline (org-element-lineage ts '(headline))))
                   (when (and headline
                              (memq ts-type '(active active-range))
                              (not (eq (org-element-type parent) 'planning)))
                     (pcase-let ((`(,start . ,end)
                                  (jds/org-calendar--timestamp-to-time-range ts)))
                       (push (list :file-name  (file-name-nondirectory file)
                                   :title      (org-element-property :raw-value headline)
                                   :todo       (org-element-property :todo-keyword headline)
                                   :path       (jds/org-calendar--headline-path headline)
                                   :timestamp  (org-element-property :raw-value ts)
                                   :start      start
                                   :end        end)
                             events)))))))))
        (puthash key events jds/org-calendar-cache)
        events))))

(defun jds/org-calendar-search (start-date end-date)
  "Search org-agenda-files for active timestamped events between START-DATE and END-DATE.
Ignores SCHEDULED/DEADLINE planning timestamps."
  (let* ((start-time (date-to-time (concat start-date " 00:00:00")))
         (end-time   (date-to-time (concat end-date   " 23:59:59")))
         (results nil))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (dolist (ev (jds/org-calendar--parse-file-events file))
          (when (jds/org-calendar--time-overlaps-p
                 (plist-get ev :start) (plist-get ev :end)
                 start-time end-time)
            (push ev results)))))
    (if results
        (concat
         (format "Calendar events %s → %s:\n" start-date end-date)
         (mapconcat
          (lambda (ev)
            (format "- [%s] %s%s | %s | path: %s"
                    (plist-get ev :timestamp)
                    (if-let ((todo (plist-get ev :todo))) (concat todo ": ") "")
                    (plist-get ev :title)
                    (plist-get ev :file-name)
                    (plist-get ev :path)))
          (nreverse results)
          "\n"))
      (format "No calendar events found from %s to %s." start-date end-date))))

(defvar jds~gptel-calendar-tool
  (gptel-make-tool
   :name "search_calendar"
   :description "Search the user's org-mode calendar for timestamped events within a date range. Excludes SCHEDULED and DEADLINE planning timestamps. Each result includes the event title, timestamp, source file, and heading path."
   :args (list '(:name "start_date" :type string :description "Start date in YYYY-MM-DD format")
               '(:name "end_date"   :type string :description "End date in YYYY-MM-DD format"))
   :function #'jds/org-calendar-search)
  "GPtel tool for searching the org-mode calendar.")


;;; Reply extraction -------------------------------------------------------

(defun jds/ai-email--strip-code-fences (text)
  "Remove leading and trailing Markdown code fences from TEXT."
  (let ((clean (string-trim (or text ""))))
    (setq clean (replace-regexp-in-string
                 "\\`[`][`][`][[:alpha:]-]*[ \t]*\n?" "" clean))
    (setq clean (replace-regexp-in-string
                 "\n?```[ \t]*\\'" "" clean))
    (string-trim clean)))

(defun jds/ai-email--extract-tagged-reply (text)
  "Extract a reply body wrapped in <reply> tags from TEXT."
  (when (string-match "<reply>\\([[:ascii:][:nonascii:]\n\r\t[:space:]]*?\\)</reply>" text)
    (string-trim (match-string 1 text))))

(defun jds/ai-email--strip-internal-preface (text)
  "Strip common AI planning prefaces from TEXT."
  (let ((clean (string-trim text)))
    (dolist (pattern '("\\`I['’]ll help you draft[^.\n]*\\(?:[.\n]+\\)"
                       "\\`Let me [^.\n]*\\(?:[.\n]+\\)"
                       "\\`First,? I['’]ll [^.\n]*\\(?:[.\n]+\\)"
                       "\\`I['’]ll [^.\n]*check your calendar[^.\n]*\\(?:[.\n]+\\)"
                       "\\`I(?: am|'m) [^.\n]*check[^.\n]*\\(?:[.\n]+\\)"))
      (setq clean (replace-regexp-in-string pattern "" clean)))
    (string-trim clean)))

(defun jds/ai-email--sanitize-response (response)
  "Return only the intended email body from RESPONSE."
  (let* ((clean  (jds/ai-email--strip-code-fences response))
         (tagged (jds/ai-email--extract-tagged-reply clean)))
    (setq clean (or tagged clean))
    (setq clean (jds/ai-email--strip-internal-preface clean))
    (string-trim clean)))

(defun jds/ai-email--planning-response-p (text)
  "Whether TEXT looks like internal planning text rather than an email reply."
  (let ((clean (downcase (string-trim (or text "")))))
    (or (string-empty-p clean)
        (string-match-p
         (rx string-start
             (or "i'll check"
                 "i will check"
                 "let me check"
                 "i'll look"
                 "i will look"
                 "let me look")
             (+ nonl))
         clean)
        (string-match-p
         (rx string-start
             (or "i'll review"
                 "i will review"
                 "let me review")
             (+ nonl))
         clean)
        (string-match-p
         (rx string-start
             (or "checking the calendar"
                 "reviewing the calendar"
                 "looking at the calendar"))
         clean))))

(defun jds/ai-email--insert-if-final-string (response info buf pos)
  "Insert RESPONSE into BUF at POS when RESPONSE is the final string result."
  (cond
   ((not response)
    (message "gptel error: %s" (plist-get info :status)))
   ((stringp response)
    (with-current-buffer buf
      (goto-char pos)
      (insert (jds/ai-email--sanitize-response response) "\n\n")
      (set-marker pos nil)))
   ;; Ignore intermediate tool-use, tool-result, reasoning and abort payloads.
   (t nil)))

(defun jds/ai-email--handle-scheduling-response
    (response info prompt system buf pos &optional retries-left)
  "Handle scheduling RESPONSE, retrying once if the model returns planning text."
  (cond
   ((not response)
    (message "gptel error: %s" (plist-get info :status)))
   ((stringp response)
    (let ((clean (jds/ai-email--sanitize-response response)))
      (if (and (> (or retries-left 0) 0)
               (jds/ai-email--planning-response-p clean))
          (let ((gptel-use-tools t)
                (gptel-tools (list jds~gptel-calendar-tool)))
            (gptel-request
             prompt
             :system (concat
                      system
                      "\nYour previous response was invalid because it described what you would do instead of drafting the email.\n"
                      "You have already checked the calendar. Do not say that you will check availability.\n"
                      "Write the email that should be sent now, with concrete proposed times or a direct request for the recipient's availability.")
             :stream nil
             :buffer buf
             :callback
             (lambda (response info)
               (jds/ai-email--handle-scheduling-response
                response info prompt system buf pos (1- retries-left)))))
        (if (jds/ai-email--planning-response-p clean)
            (message "Scheduling draft suppressed: model returned planning text instead of an email.")
          (with-current-buffer buf
            (goto-char pos)
            (insert clean "\n\n")
            (set-marker pos nil))))))
   (t nil)))


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
                               :system (concat
                                        "You are a professional email assistant. Write clear, concise replies.\n"
                                        "Return only the reply body text.\n"
                                        "Do not include a subject line, commentary, reasoning, tool narration, or code fences.")
                               :stream nil
                               :buffer buf
                               :callback
                               (lambda (response info)
                                 (jds/ai-email--insert-if-final-string
                                  response info buf pos)))))))
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
                           "If asked to schedule a meeting, propose concrete meeting times when possible.\n"
                           "Return only the reply body text.\n"
                           "Do not include a subject line, commentary, reasoning, tool narration, or code fences."))
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
                (let ((gptel-use-tools t)
                      (gptel-tools (list jds~gptel-calendar-tool)))
                  (gptel-request prompt
                                 :system system
                                 :stream nil
                                 :buffer buf
                                 :callback
                                 (lambda (response info)
                                   (jds/ai-email--handle-scheduling-response
                                    response info prompt system buf pos 1))))))))
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
