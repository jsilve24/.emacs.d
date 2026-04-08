;;; ai-email.el --- AI-powered email drafting via gptel -*- lexical-binding: t -*-

;;; Calendar availability tool ---------------------------------------------

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-element)
(require 'subr-x)

(defvar jds/org-calendar-cache (make-hash-table :test #'equal)
  "Cache of parsed calendar events by file and modification time.")

(defvar jds/scheduling-workday-start-hour 9
  "Hour when the scheduling workday begins.")

(defvar jds/scheduling-workday-end-hour 17
  "Hour when the scheduling workday ends.")

(defvar jds/scheduling-slot-increment-minutes 30
  "Granularity in minutes for suggested meeting starts.")

(defvar jds/scheduling-default-slot-count 8
  "Default number of meeting slots to return.")

(defvar jds/scheduling-default-search-days 14
  "Default scheduling search horizon in days when the model needs a fallback window.")

(defun jds/org-calendar--file-mtime (file)
  (file-attribute-modification-time (file-attributes file)))

(defun jds/org-calendar--parse-date (date-string)
  "Return a time value for DATE-STRING in YYYY-MM-DD format."
  (date-to-time (concat date-string " 00:00:00")))

(defun jds/org-calendar--format-date (time)
  "Return TIME as YYYY-MM-DD."
  (format-time-string "%Y-%m-%d" time))

(defun jds/org-calendar--day-start (time)
  "Return the start of TIME's day."
  (pcase-let* ((`(,_sec ,_min ,_hour ,day ,month ,year . ,_)
                (decode-time time)))
    (encode-time 0 0 0 day month year)))

(defun jds/org-calendar--at-hour (time hour)
  "Return TIME's date at HOUR:00."
  (pcase-let* ((`(,_sec ,_min ,_hour ,day ,month ,year . ,_)
                (decode-time time)))
    (encode-time 0 0 hour day month year)))

(defun jds/org-calendar--days-between (start end)
  "Return number of whole days from START to END, inclusive."
  (1+ (- (time-to-days (jds/org-calendar--day-start end))
         (time-to-days (jds/org-calendar--day-start start)))))

(defun jds/org-calendar--next-day (time)
  "Return the start of the day after TIME."
  (time-add (jds/org-calendar--day-start time) (days-to-time 1)))

(defun jds/org-calendar--timestamp-to-time-range (ts)
  "Return (START . END) time values for org-element timestamp TS."
  (let* ((syear  (org-element-property :year-start ts))
         (smonth (org-element-property :month-start ts))
         (sday   (org-element-property :day-start ts))
         (shour  (org-element-property :hour-start ts))
         (smin   (org-element-property :minute-start ts))
         (eyear  (or (org-element-property :year-end ts) syear))
         (emonth (or (org-element-property :month-end ts) smonth))
         (eday   (or (org-element-property :day-end ts) sday))
         (ehour  (or (org-element-property :hour-end ts) shour))
         (emin   (or (org-element-property :minute-end ts) smin))
         (ts-type (org-element-property :type ts)))
    (cond
     ((and shour smin ehour emin)
      (cons (encode-time 0 smin shour sday smonth syear)
            (encode-time 0 emin ehour eday emonth eyear)))
     ;; Treat all-day active ranges like <2026-04-09 Thu>--<2026-04-12 Sun>
     ;; as blocking the full span of covered days.
     ((eq ts-type 'active-range)
      (let ((start (encode-time 0 0 0 sday smonth syear))
            (end   (jds/org-calendar--next-day
                    (encode-time 0 0 0 eday emonth eyear))))
        (cons start end))))))

(defun jds/org-calendar--time-overlaps-p (a-start a-end b-start b-end)
  "Whether [A-START, A-END) overlaps [B-START, B-END)."
  (and (time-less-p a-start b-end)
       (time-less-p b-start a-end)))

(defun jds/org-calendar--headline-path (hl)
  "Return outline path string for headline element HL."
  (let ((titles '())
        (cur hl))
    (while cur
      (when (eq (org-element-type cur) 'headline)
        (push (org-element-property :raw-value cur) titles))
      (setq cur (org-element-property :parent cur)))
    (string-join titles " > ")))

(defun jds/org-calendar--excluded-event-p (headline)
  "Whether HEADLINE should be excluded from meeting availability."
  (let* ((title (downcase (or (org-element-property :raw-value headline) "")))
         (todo  (downcase (or (org-element-property :todo-keyword headline) "")))
         (tags  (mapcar #'downcase (or (org-element-property :tags headline) '())))
         (text  (string-join (append (list title todo) tags) " ")))
    (or (string-match-p
         (rx word-start
             (or "cancelled" "canceled" "cancel" "reminder" "birthday"
                 "anniversary" "holiday" "ooo" "vacation")
             word-end)
         text)
        (member todo '("cancelled" "canceled"))
        (cl-some (lambda (tag)
                   (member tag '("cancelled" "canceled" "reminder" "personal")))
                 tags))))

(defun jds/org-calendar--parse-file-events (file)
  "Return cached list of timed calendar event plists for FILE."
  (let* ((mtime  (jds/org-calendar--file-mtime file))
         (key    (list file mtime))
         (cached (gethash key jds/org-calendar-cache 'missing)))
    (if (not (eq cached 'missing))
        cached
      (let ((stale-keys nil))
        (maphash (lambda (k _)
                   (when (equal (car k) file)
                     (push k stale-keys)))
                 jds/org-calendar-cache)
        (dolist (k stale-keys)
          (remhash k jds/org-calendar-cache)))
      (let (events)
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
                        (headline (org-element-lineage ts '(headline)))
                        (range    (and headline
                                       (memq ts-type '(active active-range))
                                       (not (eq (org-element-type parent) 'planning))
                                       (not (jds/org-calendar--excluded-event-p headline))
                                       (jds/org-calendar--timestamp-to-time-range ts))))
                   (when range
                     (pcase-let ((`(,start . ,end) range))
                       (when (time-less-p start end)
                         (push (list :file-name  (file-name-nondirectory file)
                                     :title      (org-element-property :raw-value headline)
                                     :todo       (org-element-property :todo-keyword headline)
                                     :path       (jds/org-calendar--headline-path headline)
                                     :timestamp  (org-element-property :raw-value ts)
                                     :start      start
                                     :end        end)
                               events)))))))))
        (puthash key events jds/org-calendar-cache)
        events)))))

(defun jds/org-calendar--events-in-range (start-time end-time)
  "Return timed org events overlapping START-TIME to END-TIME."
  (let (results)
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (dolist (ev (jds/org-calendar--parse-file-events file))
          (when (jds/org-calendar--time-overlaps-p
                 (plist-get ev :start) (plist-get ev :end)
                 start-time end-time)
            (push ev results)))))
    results))

(defun jds/org-calendar--clip-interval (start end clip-start clip-end)
  "Return interval within CLIP-START and CLIP-END, or nil."
  (let ((new-start (if (time-less-p start clip-start) clip-start start))
        (new-end   (if (time-less-p clip-end end) clip-end end)))
    (when (time-less-p new-start new-end)
      (cons new-start new-end))))

(defun jds/org-calendar--merge-intervals (intervals)
  "Merge overlapping or adjacent INTERVALS."
  (let ((sorted (sort (copy-sequence intervals)
                      (lambda (a b)
                        (time-less-p (car a) (car b)))))
        merged)
    (dolist (interval sorted)
      (if (null merged)
          (push interval merged)
        (let* ((current (car merged))
               (cur-start (car current))
               (cur-end (cdr current))
               (next-start (car interval))
               (next-end (cdr interval)))
          (if (or (time-less-p next-start cur-end)
                  (equal next-start cur-end))
              (setcar merged
                      (cons cur-start
                            (if (time-less-p cur-end next-end) next-end cur-end)))
            (push interval merged)))))
    (nreverse merged)))

(defun jds/org-calendar--day-mode (day-time)
  "Return preferred meeting mode for DAY-TIME."
  (pcase (decoded-time-weekday (decode-time day-time))
    ((or 2 4) "in_person")
    ((or 1 3 5) "zoom")
    (_ "either")))

(defun jds/org-calendar--mode-label (mode)
  "Return human-readable label for MODE."
  (pcase mode
    ("zoom" "via Zoom")
    ("in_person" "in person")
    (_ "meeting")))

(defun jds/org-calendar--mode-matches-p (day-mode preference)
  "Whether DAY-MODE satisfies PREFERENCE."
  (or (equal preference "either")
      (equal day-mode preference)))

(defun jds/org-calendar--time-of-day-matches-p (time preference)
  "Whether TIME matches time-of-day PREFERENCE."
  (let ((hour (decoded-time-hour (decode-time time))))
    (pcase preference
      ("morning" (< hour 12))
      ("afternoon" (>= hour 12))
      (_ t))))

(defun jds/org-calendar--iso-datetime (time)
  "Return TIME as an ISO 8601 local datetime string."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z" time))

(defun jds/org-calendar--display-time (time)
  "Return TIME as a human-readable clock string."
  (string-trim (format-time-string "%l:%M %p" time)))

(defun jds/org-calendar--display-date (time)
  "Return TIME as a human-readable date string."
  (replace-regexp-in-string
   " +"
   " "
   (string-trim (format-time-string "%A, %B %e" time))))

(defun jds/org-calendar--workday-p (day-time)
  "Whether DAY-TIME is a weekday."
  (pcase (decoded-time-weekday (decode-time day-time))
    ((or 0 6) nil)
    (_ t)))

(defun jds/org-calendar--slot-display (start mode)
  "Return exact display string for a proposed slot."
  (format "%s at %s (%s)"
          (jds/org-calendar--display-date start)
          (jds/org-calendar--display-time start)
          (jds/org-calendar--mode-label mode)))

(defun jds/org-calendar--window-display (start end mode)
  "Return exact display string for an availability window."
  (format "%s between %s and %s (%s)"
          (jds/org-calendar--display-date start)
          (jds/org-calendar--display-time start)
          (jds/org-calendar--display-time end)
          (jds/org-calendar--mode-label mode)))

(defun jds/org-calendar--generate-slots-for-window
    (window-start window-end duration-seconds increment-seconds mode time-pref)
  "Return candidate slots within WINDOW-START and WINDOW-END."
  (let ((slots nil)
        (slot-start window-start))
    (while (not (time-less-p window-end
                             (time-add slot-start duration-seconds)))
      (when (jds/org-calendar--time-of-day-matches-p slot-start time-pref)
        (let ((slot-end (time-add slot-start duration-seconds)))
          (push `(("start" . ,(jds/org-calendar--iso-datetime slot-start))
                  ("end" . ,(jds/org-calendar--iso-datetime slot-end))
                  ("mode" . ,mode)
                  ("display" . ,(jds/org-calendar--slot-display slot-start mode)))
                slots)))
      (setq slot-start (time-add slot-start increment-seconds)))
    (nreverse slots)))

(defun jds/org-calendar--free-windows-for-day
    (day-start busy-intervals duration-seconds mode-preference time-preference)
  "Return free windows and candidate slots for a single day."
  (let* ((work-start (jds/org-calendar--at-hour day-start jds/scheduling-workday-start-hour))
         (work-end   (jds/org-calendar--at-hour day-start jds/scheduling-workday-end-hour))
         (day-mode   (jds/org-calendar--day-mode day-start))
         (intervals  nil)
         (cursor     work-start)
         windows
         candidates
         summaries)
    (when (and (jds/org-calendar--mode-matches-p day-mode mode-preference)
               (jds/org-calendar--workday-p day-start)
               (time-less-p work-start work-end))
      (dolist (busy busy-intervals)
        (when-let ((clipped (jds/org-calendar--clip-interval
                             (car busy) (cdr busy) work-start work-end)))
          (push clipped intervals)))
      (setq intervals (jds/org-calendar--merge-intervals intervals))
      (dolist (busy intervals)
        (when (time-less-p cursor (car busy))
          (push (cons cursor (car busy)) windows))
        (setq cursor (if (time-less-p cursor (cdr busy)) (cdr busy) cursor)))
      (when (time-less-p cursor work-end)
        (push (cons cursor work-end) windows))
      (setq windows (nreverse windows))
      (dolist (window windows)
        (when (not (time-less-p (cdr window)
                                (time-add (car window) duration-seconds)))
          (push `(("start" . ,(jds/org-calendar--iso-datetime (car window)))
                  ("end" . ,(jds/org-calendar--iso-datetime (cdr window)))
                  ("mode" . ,day-mode)
                  ("display" . ,(jds/org-calendar--window-display
                                 (car window) (cdr window) day-mode)))
                summaries)
          (setq candidates
                (nconc candidates
                       (jds/org-calendar--generate-slots-for-window
                        (car window)
                        (cdr window)
                        duration-seconds
                        (* 60 jds/scheduling-slot-increment-minutes)
                        day-mode
                        time-preference))))))
    (list :windows (nreverse summaries)
          :candidates candidates)))

(defun jds/org-calendar--take-distributed-slots (candidates-by-day count)
  "Return up to COUNT slots, distributed across days when possible."
  (let ((selected nil)
        (remaining (copy-tree candidates-by-day)))
    (while (and (> count 0)
                (cl-some #'identity remaining))
      (setq remaining
            (mapcar (lambda (slots)
                      (when (and slots (> count 0))
                        (push (car slots) selected)
                        (setq count (1- count))
                        (cdr slots)))
                    remaining)))
    (nreverse selected)))

(defun jds/find-free-times
    (start_date end_date duration_minutes &optional count mode_preference time_of_day_preference)
  "Return validated free meeting slots as structured JSON."
  (let* ((start-time (jds/org-calendar--day-start
                      (jds/org-calendar--parse-date start_date)))
         (end-time   (jds/org-calendar--at-hour
                      (jds/org-calendar--parse-date end_date) 23))
         (duration   (max 1 (truncate duration_minutes)))
         (slot-count (max 1 (truncate (or count jds/scheduling-default-slot-count))))
         (mode-pref  (or mode_preference "either"))
         (time-pref  (or time_of_day_preference "either"))
         (duration-seconds (* 60 duration))
         (events (jds/org-calendar--events-in-range start-time end-time))
         (busy-intervals (mapcar (lambda (ev)
                                   (cons (plist-get ev :start) (plist-get ev :end)))
                                 events))
         (days (jds/org-calendar--days-between start-time end-time))
         (candidates-by-day nil)
         (window-summaries nil)
         candidates)
    (dotimes (offset days)
      (let* ((day-start (time-add start-time (days-to-time offset)))
             (day-availability
              (jds/org-calendar--free-windows-for-day
               day-start busy-intervals duration-seconds mode-pref time-pref)))
        (push (plist-get day-availability :candidates) candidates-by-day)
        (setq window-summaries
              (nconc window-summaries
                     (plist-get day-availability :windows)))))
    (setq candidates
          (jds/org-calendar--take-distributed-slots
           (nreverse candidates-by-day) slot-count))
    (json-encode
     `(("start_date" . ,start_date)
       ("end_date" . ,end_date)
       ("duration_minutes" . ,duration)
       ("count_requested" . ,slot-count)
       ("count_returned" . ,(length candidates))
       ("mode_preference" . ,mode-pref)
       ("time_of_day_preference" . ,time-pref)
       ("availability_windows" . ,(vconcat window-summaries))
       ("candidates" . ,(vconcat candidates))))))

(defvar jds~gptel-find-free-times-tool
  (gptel-make-tool
   :name "find_free_times"
   :description
   "Find validated free meeting slots from the user's org calendar. Searches only timed active Org timestamps, ignores SCHEDULED and DEADLINE planning entries, filters obvious canceled/reminder items, merges overlapping busy intervals, applies workday bounds, and returns structured JSON with exact Lisp-generated display strings for both availability windows and candidate meeting slots. The model must use only the returned display strings verbatim when proposing times."
   :args (list
          '(:name "start_date" :type string :description "Start date in YYYY-MM-DD format")
          '(:name "end_date" :type string :description "End date in YYYY-MM-DD format")
          '(:name "duration_minutes" :type number :description "Requested meeting length in minutes")
          '(:name "count" :type number :optional t :description "Maximum number of candidate slots to return")
          '(:name "mode_preference" :type string :optional t :description "Meeting mode preference: zoom, in_person, or either")
          '(:name "time_of_day_preference" :type string :optional t :description "Time preference: morning, afternoon, or either"))
   :function #'jds/find-free-times)
  "Gptel tool for scheduling against validated org calendar availability.")


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
         clean)
        (string-match-p
         (rx string-start
             (or "i'll search"
                 "i will search"
                 "let me search"
                 "searching for available"
                 "searching for"
                 "i'll find"
                 "i will find"
                 "finding available")
             (+ nonl))
         clean))))

(defun jds/ai-email--live-insertion-point (buf pos)
  "Return a safe insertion point in BUF based on marker POS."
  (with-current-buffer buf
    (cond
     ((and (markerp pos)
           (marker-buffer pos)
           (eq (marker-buffer pos) buf))
      (marker-position pos))
     (t
      (save-excursion
        (message-goto-body)
        (point))))))

(defun jds/ai-email--insert-response-at-point (buf pos text)
  "Insert TEXT into BUF at POS, tolerating stale markers."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (goto-char (jds/ai-email--live-insertion-point buf pos))
      (insert text "\n\n")
      (when (and (markerp pos)
                 (marker-buffer pos))
        (set-marker pos nil)))))

(defun jds/ai-email--delete-leading-planning-text (buf pos)
  "Delete any model-inserted planning text starting at POS in BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((start (jds/ai-email--live-insertion-point buf pos))
             (end (save-excursion
                    (goto-char start)
                    (if (search-forward "\n\n" nil t)
                        (match-beginning 0)
                      (point-max)))))
        (when (< start end)
          (let ((candidate (string-trim
                            (buffer-substring-no-properties start end))))
            (when (jds/ai-email--planning-response-p candidate)
              (delete-region start end)
              (goto-char start)
              (when (looking-at-p "\n\n")
                (delete-region (point) (min (point-max) (+ (point) 2)))))))))))

(defun jds/ai-email--insert-if-final-string (response info buf pos)
  "Insert RESPONSE into BUF at POS when RESPONSE is the final string result."
  (cond
   ((not response)
    (message "gptel error: %s" (plist-get info :status)))
   ((stringp response)
    (jds/ai-email--insert-response-at-point
     buf pos (jds/ai-email--sanitize-response response)))
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
          (let ((gptel-include-reasoning nil)
                (gptel-use-tools t)
                (gptel-tools (list jds~gptel-find-free-times-tool)))
            (jds/ai-email--delete-leading-planning-text buf pos)
            (gptel-request
             prompt
             :system (concat
                      system
                      "\nYour previous response was invalid because it described what you would do instead of drafting the email.\n"
                      "You have already checked availability.\n"
                      "Write the email now.\n"
                      "If you propose times, they must be exact slot display strings returned by find_free_times and copied verbatim.")
             :stream nil
             :buffer buf
             :callback
             (lambda (response info)
               (jds/ai-email--handle-scheduling-response
               response info prompt system buf pos (1- retries-left)))))
        (if (jds/ai-email--planning-response-p clean)
            (progn
              (jds/ai-email--delete-leading-planning-text buf pos)
              (message "Scheduling draft suppressed: model returned planning text instead of an email."))
          (jds/ai-email--insert-response-at-point buf pos clean)))))
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
                (let ((gptel-include-reasoning nil))
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
                                    response info buf pos))))))))
    (add-hook 'mu4e-compose-mode-hook hook-fn)
    (jds/mu4e-compose-reply)))


;;; AI scheduling reply ----------------------------------------------------

(defun jds/mu4e-ai-scheduling-reply ()
  "Reply to message at point with an AI-drafted scheduling response.
Prompts for custom context. Uses validated availability slots rather than raw calendar dumps."
  (interactive)
  (let* ((ctx      (read-string "Scheduling context (priority, duration, notes): "))
         (msg      (mu4e-message-at-point))
         (from-c   (car (mu4e-message-field msg :from)))
         (from-str (or (plist-get from-c :name) (plist-get from-c :email) "Unknown"))
         (subject  (or (mu4e-message-field msg :subject) "(no subject)"))
         (today-time (current-time))
         (today    (format-time-string "%Y-%m-%d (%A, %B %d, %Y)" today-time))
         (default-start (jds/org-calendar--format-date today-time))
         (default-end (jds/org-calendar--format-date
                       (time-add today-time
                                 (days-to-time jds/scheduling-default-search-days))))
         (system   (concat
                    "You are a scheduling assistant helping draft meeting emails.\n"
                    "Today is " today ".\n"
                    "If the thread gives no date range, use "
                    default-start " through " default-end ".\n"
                    "If the meeting length is unspecified, assume 30 minutes.\n\n"
                    "Scheduling preferences:\n"
                    "- Tuesdays & Thursdays: prefer in-person meetings.\n"
                    "- Mondays, Wednesdays & Fridays: prefer Zoom meetings.\n\n"
                    "Before proposing times, you must call find_free_times.\n"
                    "Prefer availability_windows when they give a clearer summary than isolated slots.\n"
                    "If one day has several adjacent openings, summarize them as windows.\n"
                    "When listing availability windows, group them by day using one bullet per day and at most 2 windows per bullet.\n"
                    "Format exactly like this: Thursday, April 9: 9:00--10:00 AM; 10:30 AM--1:00 PM\n"
                    "Do not repeat the date within a bullet, use the word \"between,\" add prose inside bullets, or make bullets longer than one line.\n"
                    "For these grouped availability bullets only, you may reformat availability_windows into that layout, but preserve the returned day/date text and exact start/end times.\n"
                    "When offering options, prefer coverage across multiple days instead of concentrating everything on one day.\n"
                    "Otherwise use returned display strings verbatim. Do not infer weekdays, do date arithmetic, invent times, invent meeting modes, or restate returned times in different words.\n"
                    "Return only the reply body text. No subject line, commentary, reasoning, tool narration, or code fences."))
         hook-fn)
    (setq hook-fn
          (lambda ()
            (remove-hook 'mu4e-compose-mode-hook hook-fn)
            (let* ((buf     (current-buffer))
                   (content (buffer-substring-no-properties (point-min) (point-max)))
                   (prompt  (format
                             (concat
                              "Draft a scheduling reply for this email from %s (subject: \"%s\").%s\n"
                              "Compose buffer (includes quoted original):\n\n%s\n\n"
                              "If you propose times, call find_free_times first.\n"
                              "Prefer summarizing returned availability windows verbatim, and include multiple days when possible.\n"
                              "When listing availability windows, group them by day using one bullet per day, at most 2 windows per bullet, in this exact format: Thursday, April 9: 9:00--10:00 AM; 10:30 AM--1:00 PM. Do not repeat the date within a bullet, do not use the word \"between\", do not add prose inside bullets, and keep each bullet to one line.\n"
                              "Return only the reply body.")
                             from-str subject
                             (if (string-empty-p ctx) ""
                               (format "\nContext: %s" ctx))
                             content)))
              (message-goto-body)
              (let ((pos (copy-marker (point))))
                (let ((gptel-include-reasoning nil)
                      (gptel-use-tools t)
                      (gptel-tools (list jds~gptel-find-free-times-tool)))
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
