;;; capture.el --- AI email capture workflow -*- lexical-binding: t; -*-

;;; AI capture from email --------------------------------------------------

(defvar jds/ai-email-capture-inbox-file "~/Dropbox/org/inbox.org"
  "Target file for AI-extracted todo captures.")

(defvar jds/ai-email-capture-calendar-file "~/Dropbox/org/calendar.org"
  "Target file for AI-extracted calendar captures.")

(defvar jds/ai-email-capture-calendar-headline "Calendar"
  "Headline in `jds/ai-email-capture-calendar-file' for event captures.")

(defvar-local jds/ai-email-capture-review-message-link nil
  "mu4e message link associated with the current AI capture review buffer.")

(defvar-local jds/ai-email-capture-review-source-scope nil
  "Whether the current AI capture review buffer came from a region or full message.")

(defvar jds/ai-email-capture-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "C-c C-c") #'jds/ai-email-capture-review-finalize)
    (define-key map (kbd "C-c C-k") #'jds/ai-email-capture-review-abort)
    map)
  "Keymap for `jds/ai-email-capture-review-mode'.")

(define-derived-mode jds/ai-email-capture-review-mode org-mode "AI-Email-Capture"
  "Major mode for reviewing AI-extracted email capture candidates."
  (setq-local header-line-format
              "Edit or delete headings, then C-c C-c to capture the rest. C-c C-k cancels."))

(defun jds/ai-email--string-or-nil (value)
  "Return VALUE as a trimmed string, or nil."
  (when value
    (let ((text (string-trim (format "%s" value))))
      (unless (string-empty-p text)
        text))))

(defun jds/ai-email--sanitize-heading-text (text)
  "Return TEXT normalized for use in an Org heading."
  (replace-regexp-in-string
   "[ \t]+"
   " "
   (string-trim (replace-regexp-in-string "[\n\r]+" " " (or text "")))))

(defun jds/ai-email--extract-json-object (text)
  "Extract the outermost JSON object from TEXT."
  (let* ((clean (jds/ai-email--strip-code-fences text))
         (start (and clean (string-match "{" clean)))
         (end   (and clean (string-match "}[^}]*\\'" clean))))
    (if (and start end)
        (substring clean start (1+ end))
      clean)))

(defun jds/ai-email--parse-json-response (response)
  "Parse RESPONSE as JSON and return an alist."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-false nil)
        (json-null nil))
    (json-read-from-string (jds/ai-email--extract-json-object response))))

(defun jds/ai-email--message-link (msg)
  "Return a mu4e link for MSG."
  (format "[[mu4e:msgid:%s][email]]"
          (plist-get msg :message-id)))

(defun jds/ai-email--inactive-now-string ()
  "Return an inactive Org timestamp for the current time."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun jds/ai-email--parse-iso-local-time (value)
  "Parse VALUE in YYYY-MM-DD or YYYY-MM-DDTHH:MM[:SS] form."
  (when-let ((text (jds/ai-email--string-or-nil value)))
    (cond
     ((string-match
       "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'" text)
      (encode-time 0 0 0
                   (string-to-number (match-string 3 text))
                   (string-to-number (match-string 2 text))
                   (string-to-number (match-string 1 text))))
     ((string-match
       "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)[T ]\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\(?::\\([0-9]\\{2\\}\\)\\)?\\'" text)
      (encode-time (string-to-number (or (match-string 6 text) "0"))
                   (string-to-number (match-string 5 text))
                   (string-to-number (match-string 4 text))
                   (string-to-number (match-string 3 text))
                   (string-to-number (match-string 2 text))
                   (string-to-number (match-string 1 text))))
     ;; ISO 8601 with timezone: YYYY-MM-DDTHH:MM[:SS](Z|+HH:MM|-HH:MM)
     ((string-match
       (concat "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
               "[T ]\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\(?::\\([0-9]\\{2\\}\\)\\)?"
               "\\(Z\\|[+-][0-9]\\{2\\}:[0-9]\\{2\\}\\)\\'")
       text)
      (let* ((tz-str (match-string 7 text))
             (tz-offset-secs
              (if (equal tz-str "Z") 0
                (let* ((sign (if (string-prefix-p "-" tz-str) -1 1))
                       (hh   (string-to-number (substring tz-str 1 3)))
                       (mm   (string-to-number (substring tz-str 4 6))))
                  (* sign (+ (* hh 3600) (* mm 60))))))
             (utc-epoch (- (float-time
                            (encode-time
                             (string-to-number (or (match-string 6 text) "0"))
                             (string-to-number (match-string 5 text))
                             (string-to-number (match-string 4 text))
                             (string-to-number (match-string 3 text))
                             (string-to-number (match-string 2 text))
                             (string-to-number (match-string 1 text))
                             0))
                           tz-offset-secs)))
        (encode-time (decode-time utc-epoch))))
     (t nil))))

(defun jds/ai-email--all-day-range-string (start end)
  "Return an Org all-day timestamp or range covering START through END."
  (let* ((start-day (jds/org-calendar--day-start start))
         (end-day   (and end (jds/org-calendar--day-start end))))
    (if (or (not end-day)
            (equal start-day end-day))
        (format-time-string "<%Y-%m-%d %a>" start-day)
      (format "%s--%s"
              (format-time-string "<%Y-%m-%d %a>" start-day)
              (format-time-string "<%Y-%m-%d %a>" end-day)))))

(defun jds/ai-email--timed-range-string (start end)
  "Return an Org timed timestamp or range from START to END."
  (let ((end-time (or end (time-add start (seconds-to-time (* 30 60))))))
    (if (equal (jds/org-calendar--day-start start)
               (jds/org-calendar--day-start end-time))
        (format "<%s-%s>"
                (format-time-string "%Y-%m-%d %a %H:%M" start)
                (format-time-string "%H:%M" end-time))
      (format "%s--%s"
              (format-time-string "<%Y-%m-%d %a %H:%M>" start)
              (format-time-string "<%Y-%m-%d %a %H:%M>" end-time)))))

(defun jds/ai-email--event-timestamp-string (start end all-day)
  "Return an Org timestamp string for START, END, and ALL-DAY."
  (let* ((start-time (jds/ai-email--parse-iso-local-time start))
         (end-time   (jds/ai-email--parse-iso-local-time end)))
    (unless start-time
      (user-error "Event is missing a valid start time"))
    (if all-day
        (jds/ai-email--all-day-range-string start-time end-time)
      (jds/ai-email--timed-range-string start-time end-time))))

(defun jds/ai-email--review-body (location url modality notes)
  "Build initial editable review body from LOCATION, URL, MODALITY, and NOTES."
  (string-join
   (delq nil
         (list (when-let ((loc (jds/ai-email--string-or-nil location)))
                 (format "Location: %s" loc))
               (when-let ((link (jds/ai-email--string-or-nil url)))
                 (format "%s: %s"
                         (if (equal (jds/ai-email--string-or-nil modality) "zoom")
                             "Zoom"
                           "Meeting link")
                         link))
               (jds/ai-email--string-or-nil notes)))
   "\n"))

(defun jds/ai-email--first-subtree-timestamp ()
  "Return the first Org timestamp in the current subtree, or nil."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t) (point))))
      (forward-line 1)
      (when (re-search-forward org-tsr-regexp-both end t)
        (match-string-no-properties 0)))))

(defun jds/ai-email--review-buffer-name (msg)
  "Return a review buffer name for MSG."
  (format "*AI Email Capture: %s*"
          (truncate-string-to-width
           (or (plist-get msg :subject) "(no subject)") 50 nil nil t)))

(defun jds/ai-email--insert-review-candidate (item type)
  "Insert ITEM of TYPE into the current review buffer."
  (let* ((title (jds/ai-email--sanitize-heading-text
                 (or (alist-get 'title item) "Untitled")))
         (notes (jds/ai-email--string-or-nil (alist-get 'notes item)))
         (location (jds/ai-email--string-or-nil (alist-get 'location item)))
         (url (or (jds/ai-email--string-or-nil (alist-get 'conference_url item))
                  (jds/ai-email--string-or-nil (alist-get 'zoom_link item))
                  (jds/ai-email--string-or-nil (alist-get 'url item))))
         (modality (jds/ai-email--string-or-nil (alist-get 'modality item)))
         (evidence (jds/ai-email--string-or-nil (alist-get 'evidence item)))
         (review-body (if (equal type "event")
                          (jds/ai-email--review-body location url modality notes)
                        (or notes ""))))
    (insert (if (equal type "todo")
                (format "* TODO %s\n" title)
              (format "* %s\n" title)))
    (insert ":PROPERTIES:\n")
    (insert (format ":AI_CAPTURE_TYPE: %s\n" type))
    (when-let ((start (jds/ai-email--string-or-nil (alist-get 'start item))))
      (insert (format ":AI_CAPTURE_START: %s\n" start)))
    (when-let ((end (jds/ai-email--string-or-nil (alist-get 'end item))))
      (insert (format ":AI_CAPTURE_END: %s\n" end)))
    (when (equal type "event")
      (insert (format ":AI_CAPTURE_ALL_DAY: %s\n"
                      (if (alist-get 'all_day item) "t" "nil"))))
    (when modality
      (insert (format ":AI_CAPTURE_MODALITY: %s\n" modality)))
    (when location
      (insert (format ":AI_CAPTURE_LOCATION: %s\n" location)))
    (when url
      (insert (format ":AI_CAPTURE_URL: %s\n" url)))
    (insert ":END:\n")
    (when (and (equal type "event")
               (jds/ai-email--string-or-nil (alist-get 'start item)))
      (insert (jds/ai-email--event-timestamp-string
               (alist-get 'start item)
               (alist-get 'end item)
               (alist-get 'all_day item))
              "\n"))
    (when (not (string-empty-p review-body))
      (insert review-body "\n"))
    (when evidence
      (insert "# Evidence: "
              (replace-regexp-in-string "[\n\r]+" " " evidence)
              "\n"))
    (insert "\n")))

(defun jds/ai-email--create-review-buffer (msg parsed source-scope)
  "Create and populate a review buffer for MSG from PARSED extraction results."
  (let* ((todos  (or (alist-get 'todos parsed) '()))
         (events (or (alist-get 'events parsed) '()))
         (count  (+ (length todos) (length events))))
    (if (= count 0)
        (message "AI capture found no todo or calendar candidates")
      (let ((buf (generate-new-buffer (jds/ai-email--review-buffer-name msg))))
        (with-current-buffer buf
          (jds/ai-email-capture-review-mode)
          (setq-local jds/ai-email-capture-review-message-link
                      (jds/ai-email--message-link msg))
          (setq-local jds/ai-email-capture-review-source-scope source-scope)
          (insert "#+TITLE: AI Email Capture Review\n")
          (insert "#+STARTUP: showall\n\n")
          (insert (format "Source: %s\n"
                          (if (eq source-scope 'region)
                              "selected region only"
                            "full message")))
          (insert "Delete any heading you do not want to capture. Edit headings and body text freely, then press C-c C-c.\n\n")
          (dolist (event events)
            (jds/ai-email--insert-review-candidate event "event"))
          (dolist (todo todos)
            (jds/ai-email--insert-review-candidate todo "todo"))
          (goto-char (point-min))
          (search-forward-regexp "^\\*" nil t)
          (org-fold-show-all))
        (pop-to-buffer buf)))))

(defun jds/ai-email--review-entry-body ()
  "Return the editable body of the current review entry."
  (save-excursion
    (org-back-to-heading t)
    (let* ((body-start (save-excursion (org-end-of-meta-data t) (point)))
           (subtree-end (save-excursion (org-end-of-subtree t t) (point)))
           (start (max (point-min) (min body-start (point-max))))
           (end   (max start (min subtree-end (point-max))))
           (body-text (buffer-substring-no-properties start end)))
      (with-temp-buffer
        (insert body-text)
        (goto-char (point-min))
        (skip-chars-forward "\n\t ")
        (let ((line-beg (line-beginning-position))
              (line-end (line-end-position)))
          (when (save-excursion
                  (goto-char line-beg)
                  (looking-at-p (concat "[ \t]*" org-tsr-regexp-both "[ \t]*$")))
            (delete-region line-beg (min (point-max) (1+ line-end)))))
        (string-trim (buffer-string))))))

(defun jds/ai-email--append-entry-to-file (file entry &optional headline)
  "Append ENTRY to FILE, optionally as a child of HEADLINE.
ENTRY may be a string or a function (STARS) -> string, where STARS is a
string of `*' characters at the correct child heading level.
Return a marker at the inserted heading."
  (with-current-buffer (find-file-noselect (expand-file-name file))
    (unless (derived-mode-p 'org-mode)
      (org-mode))
    (save-excursion
      (let (marker)
        (if headline
            (let* ((target (or (org-find-exact-headline-in-buffer headline)
                               (save-excursion
                                 (goto-char (point-max))
                                 (unless (bolp) (insert "\n"))
                                 (insert "* " headline "\n")
                                 (org-find-exact-headline-in-buffer headline))))
                   (child-stars (save-excursion
                                  (goto-char target)
                                  (make-string (1+ (org-outline-level)) ?*)))
                   (resolved-entry (if (functionp entry)
                                       (funcall entry child-stars)
                                     entry)))
              (goto-char target)
              (org-end-of-subtree t t)
              (unless (bolp) (insert "\n"))
              (setq marker (copy-marker (point) t))
              (insert resolved-entry))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (setq marker (copy-marker (point) t))
          (insert (if (functionp entry) (funcall entry "*") entry)))
        (save-buffer)
        marker))))

(defun jds/ai-email--capture-todo-entry (title body message-link)
  "Capture a todo with TITLE, BODY, and MESSAGE-LINK."
  (setq org-capture-last-stored-marker
        (jds/ai-email--append-entry-to-file
         jds/ai-email-capture-inbox-file
         (concat "* TODO " (jds/ai-email--sanitize-heading-text title) "\n"
                 " " (jds/ai-email--inactive-now-string) "\n"
                 (if (string-empty-p body) "" (concat body "\n"))
                 " " message-link "\n"))))

(defun jds/ai-email--capture-event-entry (title start end all-day body message-link)
  "Capture an event with TITLE, START, END, ALL-DAY, BODY, and MESSAGE-LINK."
  (setq org-capture-last-stored-marker
        (jds/ai-email--append-entry-to-file
         jds/ai-email-capture-calendar-file
         (lambda (stars)
           (concat stars " " (jds/ai-email--sanitize-heading-text title) "\n"
                   " " (jds/ai-email--event-timestamp-string start end all-day) "\n"
                   (if (string-empty-p body) "" (concat body "\n"))
                   " " message-link "\n"))
         jds/ai-email-capture-calendar-headline)))

(defun jds/ai-email--review-event-start-end ()
  "Return event timing info for the current review entry."
  (let* ((start-prop (org-entry-get (point) "AI_CAPTURE_START"))
         (end-prop   (org-entry-get (point) "AI_CAPTURE_END"))
         (all-day    (equal (org-entry-get (point) "AI_CAPTURE_ALL_DAY") "t"))
         (timestamp  (jds/ai-email--first-subtree-timestamp)))
    (cond
     (timestamp
      (let ((el (with-temp-buffer
                  (org-mode)
                  (insert timestamp)
                  (goto-char (point-min))
                  (car (org-element-map (org-element-parse-buffer) 'timestamp #'identity)))))
        (list :start (format "%04d-%02d-%02d%s"
                             (org-element-property :year-start el)
                             (org-element-property :month-start el)
                             (org-element-property :day-start el)
                             (if (org-element-property :hour-start el)
                                 (format "T%02d:%02d"
                                         (org-element-property :hour-start el)
                                         (org-element-property :minute-start el))
                               ""))
              :end (when (org-element-property :year-end el)
                     (format "%04d-%02d-%02d%s"
                             (org-element-property :year-end el)
                             (org-element-property :month-end el)
                             (org-element-property :day-end el)
                             (if (org-element-property :hour-end el)
                                 (format "T%02d:%02d"
                                         (org-element-property :hour-end el)
                                         (org-element-property :minute-end el))
                               "")))
              :all-day (not (org-element-property :hour-start el)))))
     (t
      (list :start start-prop :end end-prop :all-day all-day)))))

(defun jds/ai-email-capture-review-finalize ()
  "Capture remaining AI review entries into Org files."
  (interactive)
  (unless (derived-mode-p 'jds/ai-email-capture-review-mode)
    (user-error "Not in an AI email capture review buffer"))
  (let ((message-link jds/ai-email-capture-review-message-link)
        (todo-count 0)
        (event-count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (goto-char (match-beginning 0))
        (when-let ((type (org-entry-get (point) "AI_CAPTURE_TYPE")))
          (let ((title (jds/ai-email--sanitize-heading-text
                        (org-get-heading t t t t)))
                (body  (jds/ai-email--review-entry-body)))
            (pcase type
              ("todo"
               (jds/ai-email--capture-todo-entry title body message-link)
               (setq todo-count (1+ todo-count)))
              ("event"
               (pcase-let ((`(:start ,start :end ,end :all-day ,all-day)
                            (jds/ai-email--review-event-start-end)))
                 (jds/ai-email--capture-event-entry
                  title start end all-day body message-link)
                 (setq event-count (1+ event-count)))))))
        (org-end-of-subtree t t)))
    (kill-buffer (current-buffer))
    (message "Captured %d event(s) and %d todo(s)"
             event-count todo-count)))

(defun jds/ai-email-capture-review-abort ()
  "Abort the current AI email capture review."
  (interactive)
  (unless (derived-mode-p 'jds/ai-email-capture-review-mode)
    (user-error "Not in an AI email capture review buffer"))
  (when (y-or-n-p "Discard this AI capture review buffer? ")
    (kill-buffer (current-buffer))
    (message "AI email capture review discarded")))

(defun jds/ai-email--capture-source (msg)
  "Return (TEXT . SCOPE) for MSG based on active region or full message."
  (if (use-region-p)
      (cons (buffer-substring-no-properties (region-beginning) (region-end))
            'region)
    (cons (mu4e-view-message-text msg) 'message)))

(defun jds/ai-email--capture-system-prompt (today scope)
  "Return the extraction system prompt for TODAY and SCOPE.
The static rules portion is read from the live capture artifact when available,
falling back to the initial text defined in `jds/ai-email--reinforce-capture-initial-text'."
  (let ((base (or (and (fboundp 'jds/ai-email--capture-artifact-text)
                       (jds/ai-email--capture-artifact-text))
                  jds/ai-email--reinforce-capture-initial-text)))
    (concat "Today is " today ".\n"
            base "\n"
            (if (eq scope 'region)
                "Only extract candidates supported by the selected region. Ignore any context outside it.\n"
              "Treat the newest unquoted portion of the message as primary and older quoted text as supporting context only.\n"))))

(defun jds/mu4e-ai-extract-captures (&optional msg)
  "Extract todo and calendar capture candidates from MSG."
  (interactive)
  (let* ((message (or msg (mu4e-message-at-point t))))
    (unless message
      (user-error "No message at point"))
    (pcase-let* ((`(,source-text . ,source-scope) (jds/ai-email--capture-source message))
                 (today (format-time-string "%Y-%m-%d (%A, %B %d, %Y)"))
                 (system (jds/ai-email--capture-system-prompt today source-scope))
                 (prompt (format
                          (concat
                           "Extract capture candidates from this %s.\n"
                           "Email from: %s\n"
                           "Subject: %s\n\n"
                           "Text:\n\n%s")
                          (if (eq source-scope 'region) "selected email region" "email message")
                          (or (plist-get (car (plist-get message :from)) :name)
                              (plist-get (car (plist-get message :from)) :email)
                              "Unknown")
                          (or (plist-get message :subject) "(no subject)")
                          source-text)))
      (let ((gptel-include-reasoning nil)
            (origin (current-buffer)))
        (gptel-request
         prompt
         :system system
         :stream nil
         :buffer origin
         :callback
         (lambda (response info)
           (if (not response)
               (message "gptel error: %s" (plist-get info :status))
             (condition-case err
                 (jds/ai-email--create-review-buffer
                  message
                  (jds/ai-email--parse-json-response response)
                  source-scope)
               (error
                (message "AI capture parse error: %s" (error-message-string err)))))))))))

