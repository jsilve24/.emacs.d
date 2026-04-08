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

(defun jds/ai-email--request-inserting-response
    (prompt system buf callback &optional tools)
  "Run `gptel-request' for PROMPT and insert into BUF via CALLBACK.
TOOLS, when non-nil, are bound for the request."
  (let ((gptel-include-reasoning nil)
        (gptel-use-tools (and tools t))
        (gptel-tools tools))
    (gptel-request prompt
                   :system system
                   :stream nil
                   :buffer buf
                   :callback callback)))

(defun jds/ai-email--handle-retrying-insert-response
    (response info prompt system buf pos retries-left retry-system-suffix invalid-message
              &optional tools)
  "Handle RESPONSE for insert-at-point flows that may need one retry.
RETRY-SYSTEM-SUFFIX is appended to SYSTEM for the retry request when the model
returns planning text. INVALID-MESSAGE is shown if the model still fails after
retries. TOOLS, when non-nil, are provided on the retry request."
  (cond
   ((not response)
    (message "gptel error: %s" (plist-get info :status)))
   ((stringp response)
    (let ((clean (jds/ai-email--sanitize-response response)))
      (if (and (> (or retries-left 0) 0)
               (jds/ai-email--planning-response-p clean))
          (progn
            (jds/ai-email--delete-leading-planning-text buf pos)
            (jds/ai-email--request-inserting-response
             prompt
             (concat system retry-system-suffix)
             buf
             (lambda (response info)
               (jds/ai-email--handle-retrying-insert-response
                response info prompt system buf pos (1- retries-left)
                retry-system-suffix invalid-message tools))
             tools))
        (if (jds/ai-email--planning-response-p clean)
            (progn
              (jds/ai-email--delete-leading-planning-text buf pos)
              (message "%s" invalid-message))
          (jds/ai-email--insert-response-at-point buf pos clean)))))
   (t nil)))

(defun jds/ai-email--mu4e-message-metadata (&optional msg)
  "Return plist with sender and subject metadata for MSG at point."
  (let* ((message (or msg (mu4e-message-at-point)))
         (from-c (car (mu4e-message-field message :from))))
    (list :message message
          :from (or (plist-get from-c :name)
                    (plist-get from-c :email)
                    "Unknown")
          :subject (or (mu4e-message-field message :subject) "(no subject)"))))

(defun jds/ai-email--compose-mu4e-reply-with-ai (prompt-builder system callback &optional tools)
  "Compose a mu4e reply and invoke AI with PROMPT-BUILDER and CALLBACK.
PROMPT-BUILDER is called with the compose buffer contents and should return the
prompt text. CALLBACK receives RESPONSE, INFO, prompt, SYSTEM, buffer and
marker. TOOLS, when non-nil, are passed to the request."
  (let (hook-fn)
    (setq hook-fn
          (lambda ()
            (remove-hook 'mu4e-compose-mode-hook hook-fn)
            (let* ((buf (current-buffer))
                   (content (buffer-substring-no-properties (point-min) (point-max)))
                   (prompt (funcall prompt-builder content)))
              (message-goto-body)
              (let ((pos (copy-marker (point))))
                (jds/ai-email--request-inserting-response
                 prompt system buf
                 (lambda (response info)
                   (funcall callback response info prompt system buf pos))
                 tools)))))
    (add-hook 'mu4e-compose-mode-hook hook-fn)
    (jds/mu4e-compose-reply)))


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
Return a marker at the inserted heading."
  (with-current-buffer (find-file-noselect (expand-file-name file))
    (unless (derived-mode-p 'org-mode)
      (org-mode))
    (save-excursion
      (let (marker)
        (if headline
            (let ((target (or (org-find-exact-headline-in-buffer headline)
                              (save-excursion
                                (goto-char (point-max))
                                (unless (bolp) (insert "\n"))
                                (insert "* " headline "\n")
                                (org-find-exact-headline-in-buffer headline)))))
              (goto-char target)
              (org-end-of-subtree t t)
              (unless (bolp) (insert "\n"))
              (setq marker (copy-marker (point) t))
              (insert entry))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (setq marker (copy-marker (point) t))
          (insert entry))
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
         (concat "** " (jds/ai-email--sanitize-heading-text title) "\n"
                 " " (jds/ai-email--event-timestamp-string start end all-day) "\n"
                 (if (string-empty-p body) "" (concat body "\n"))
                 " " message-link "\n")
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
  "Return the extraction system prompt for TODAY and SCOPE."
  (concat
   "You extract Org capture candidates from email text.\n"
   "Today is " today ".\n"
   "Return JSON only. No commentary, no markdown code fences.\n"
   "Use the selected text or message body as the primary source.\n"
   "If quoted prior-thread context is present, use it only when the current message clearly depends on it to confirm or clarify a todo or event.\n"
   "Ignore dates, times, and deadlines mentioned in earlier emails when they are not materially relevant to the actionable item in the current message.\n"
   "For events, only return candidates that are concrete enough to belong on a calendar. A mere proposal without clear acceptance is usually not enough.\n"
   "For todos, only return actionable items for the user or explicit follow-up commitments by the user.\n"
   "If the current message says something like \"that time works\" or \"let's do the second option,\" you may use prior quoted context to resolve the accepted event.\n"
   "If the current message does not affirmatively confirm or commit to an earlier proposed event, do not capture it.\n"
   "If a physical location or meeting link is explicitly present, include it.\n"
   "For event titles, add a suffix like \"(zoom)\" or \"(in person)\" only when it adds useful context.\n"
   "The JSON schema is:\n"
   "{\n"
   "  \"events\": [{\"title\": string, \"start\": \"YYYY-MM-DDTHH:MM\" or \"YYYY-MM-DD\", \"end\": optional same format, \"all_day\": boolean, \"modality\": optional \"zoom\"|\"in_person\"|\"unknown\", \"location\": optional string, \"conference_url\": optional string, \"notes\": optional string, \"evidence\": short string, \"uses_prior_context\": boolean}],\n"
   "  \"todos\": [{\"title\": string, \"notes\": optional string, \"evidence\": short string}]\n"
   "}\n"
   (if (eq scope 'region)
       "Only extract candidates supported by the selected region. Ignore any context outside it.\n"
     "Treat the newest unquoted portion of the message as primary and older quoted text as supporting context only.\n")
   "When unsure, return fewer candidates, not more."))

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


;;; AI email reply ---------------------------------------------------------

(defvar jds/ai-email-reply-instruction-history nil
  "Minibuffer history for custom AI email reply instructions.")

(defun jds/ai-email--read-reply-instructions ()
  "Return optional custom instructions for an AI email reply."
  (let ((instructions
         (read-string "Reply instructions (optional): "
                      nil
                      'jds/ai-email-reply-instruction-history)))
    (unless (string-empty-p (string-trim instructions))
      (string-trim instructions))))

(defun jds/mu4e-ai-draft-reply (&optional custom-instructions)
  "Reply to message at point with an AI-drafted body.

When CUSTOM-INSTRUCTIONS is non-nil, include it as extra drafting guidance."
  (interactive (list (jds/ai-email--read-reply-instructions)))
  (pcase-let* ((`(:from ,from-str :subject ,subject . ,_) (jds/ai-email--mu4e-message-metadata))
               (system (concat
                        "You are a professional email assistant. Write clear, concise replies.\n"
                        "Return only the reply body text.\n"
                        "Do not include a subject line, commentary, reasoning, tool narration, or code fences.")))
    (jds/ai-email--compose-mu4e-reply-with-ai
     (lambda (content)
       (format
        "Draft a professional reply to this email from %s (subject: \"%s\").%s\nCompose buffer (includes quoted original):\n\n%s\n\nReturn only the reply body text."
        from-str
        subject
        (if custom-instructions
            (format "\nAdditional instructions: %s" custom-instructions)
          "")
        content))
     system
     (lambda (response info _prompt _system buf pos)
       (jds/ai-email--insert-if-final-string response info buf pos)))))


;;; AI scheduling reply ----------------------------------------------------

(defun jds/ai-email--read-scheduling-context ()
  "Prompt for scheduling context shared by scheduling helpers."
  (read-string "Scheduling context (priority, duration, notes): "))

(defun jds/ai-email--scheduling-system-prompt ()
  "Return the shared system prompt for scheduling-related assistants."
  (let* ((today-time (current-time))
         (today (format-time-string "%Y-%m-%d (%A, %B %d, %Y)" today-time))
         (default-start (jds/org-calendar--format-date today-time))
         (default-end (jds/org-calendar--format-date
                       (time-add today-time
                                 (days-to-time jds/scheduling-default-search-days)))))
    (concat
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
     "Otherwise use returned display strings verbatim. Do not infer weekdays, do date arithmetic, invent times, invent meeting modes, or restate returned times in different words.\n")))

(defun jds/ai-email--scheduling-context-suffix (ctx)
  "Return a formatted context suffix for scheduling prompt text from CTX."
  (if (string-empty-p ctx)
      ""
    (format "\nContext: %s" ctx)))

(defun jds/ai-email--availability-snippet-prompt (ctx)
  "Return the standalone availability prompt text for CTX."
  (format
   (concat
    "Write a concise availability snippet I can paste into a message.%s\n"
    "Call find_free_times before answering.\n"
    "Prefer summarizing returned availability windows verbatim, and include multiple days when possible.\n"
    "When listing availability windows, group them by day using one bullet per day, at most 2 windows per bullet, in this exact format: Thursday, April 9: 9:00--10:00 AM; 10:30 AM--1:00 PM. Do not repeat the date within a bullet, do not use the word \"between\", do not add prose inside bullets, and keep each bullet to one line.\n"
    "Return only the availability snippet.")
   (jds/ai-email--scheduling-context-suffix ctx)))

(defun jds/ai-email--handle-availability-snippet-response
    (response info prompt system buf pos &optional retries-left)
  "Handle standalone availability snippet RESPONSE."
  (jds/ai-email--handle-retrying-insert-response
   response info prompt system buf pos retries-left
   (concat
    "Return only the availability snippet. No greeting, commentary, reasoning, tool narration, or code fences.\n"
    "Your previous response was invalid because it described what you would do instead of returning the snippet.\n"
    "You have already checked availability.\n"
    "Write the snippet now.\n"
    "If you list times, they must be exact slot display strings returned by find_free_times or grouped availability windows that preserve the returned day/date text and exact start/end times.")
   "Availability snippet suppressed: model returned planning text instead of the snippet."
   (list jds~gptel-find-free-times-tool)))

(defun jds/ai-email-insert-availability-snippet ()
  "Insert a formatted availability snippet at point."
  (interactive)
  (let* ((ctx (jds/ai-email--read-scheduling-context))
         (buf (current-buffer))
         (pos (copy-marker (point)))
         (prompt (jds/ai-email--availability-snippet-prompt ctx))
         (system (concat
                  (jds/ai-email--scheduling-system-prompt)
                  "Return only the availability snippet. No greeting, commentary, reasoning, tool narration, or code fences.\n")))
    (jds/ai-email--request-inserting-response
     prompt system buf
     (lambda (response info)
       (jds/ai-email--handle-availability-snippet-response
        response info prompt system buf pos 1))
     (list jds~gptel-find-free-times-tool))))

(defun jds/mu4e-ai-scheduling-reply ()
  "Reply to message at point with an AI-drafted scheduling response.
Prompts for custom context. Uses validated availability slots rather than raw calendar dumps."
  (interactive)
  (let* ((ctx      (jds/ai-email--read-scheduling-context))
         (meta     (jds/ai-email--mu4e-message-metadata))
         (from-str (plist-get meta :from))
         (subject  (plist-get meta :subject))
         (system   (concat
                    (jds/ai-email--scheduling-system-prompt)
                    "Return only the reply body text. No subject line, commentary, reasoning, tool narration, or code fences.")))
    (jds/ai-email--compose-mu4e-reply-with-ai
     (lambda (content)
       (format
        (concat
         "Draft a scheduling reply for this email from %s (subject: \"%s\").%s\n"
         "Compose buffer (includes quoted original):\n\n%s\n\n"
         "If you propose times, call find_free_times first.\n"
         "Prefer summarizing returned availability windows verbatim, and include multiple days when possible.\n"
         "When listing availability windows, group them by day using one bullet per day, at most 2 windows per bullet, in this exact format: Thursday, April 9: 9:00--10:00 AM; 10:30 AM--1:00 PM. Do not repeat the date within a bullet, do not use the word \"between\", do not add prose inside bullets, and keep each bullet to one line.\n"
         "Return only the reply body.")
        from-str subject
        (jds/ai-email--scheduling-context-suffix ctx)
        content))
     system
     (lambda (response info prompt system buf pos)
       (jds/ai-email--handle-retrying-insert-response
        response info prompt system buf pos 1
        (concat
         "\nYour previous response was invalid because it described what you would do instead of drafting the email.\n"
         "You have already checked availability.\n"
         "Write the email now.\n"
         "If you propose times, they must be exact slot display strings returned by find_free_times and copied verbatim.")
        "Scheduling draft suppressed: model returned planning text instead of an email."
        (list jds~gptel-find-free-times-tool)))
     (list jds~gptel-find-free-times-tool))))


;;; Keybindings ------------------------------------------------------------

;; ca = compose with AI draft
;; cA = compose with AI scheduling assistant
(evil-collection-define-key 'normal 'mu4e-headers-mode-map
  "ca" #'jds/mu4e-ai-draft-reply
  "cA" #'jds/mu4e-ai-scheduling-reply
  "cl" #'jds/mu4e-ai-extract-captures)

(evil-collection-define-key 'normal 'mu4e-view-mode-map
  "ca" #'jds/mu4e-ai-draft-reply
  "cA" #'jds/mu4e-ai-scheduling-reply
  "cl" #'jds/mu4e-ai-extract-captures)

(provide 'ai-email)
;;; ai-email.el ends here
