;;; common.el --- Shared ai-email helpers -*- lexical-binding: t; -*-

;;; Calendar availability tool ---------------------------------------------

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-element)
(require 'subr-x)

(declare-function gptel-reinforce-set-active-database "gptel-reinforce" (database))
(declare-function gptel-reinforce-track-output-region "gptel-reinforce" (artifact start end &optional output-id))
(declare-function gptel-reinforce-register-database "gptel-reinforce" (&rest plist))
(declare-function gptel-reinforce-register-artifact "gptel-reinforce" (&rest plist))

(defconst jds/ai-email-reinforce-reply-artifact "ai-email-reply"
  "Artifact name for standard AI email replies.")

(defconst jds/ai-email-reinforce-reply-database
  jds/ai-email-reinforce-reply-artifact
  "Reinforcement database for standard AI email replies.")

(defconst jds/ai-email-reinforce-scheduling-reply-artifact "ai-email-scheduling-reply"
  "Artifact name for AI scheduling replies.")

(defconst jds/ai-email-reinforce-scheduling-reply-database
  jds/ai-email-reinforce-scheduling-reply-artifact
  "Reinforcement database for AI scheduling replies.")

(defconst jds/ai-email-reinforce-thread-summary-artifact "ai-email-thread-summary"
  "Artifact name for AI thread summaries.")

(defconst jds/ai-email-reinforce-thread-summary-database
  jds/ai-email-reinforce-thread-summary-artifact
  "Reinforcement database for AI thread summaries.")

(defconst jds/ai-email-reinforce-zoom-summary-artifact "ai-email-zoom-summary"
  "Artifact name for AI Zoom summaries.")

(defconst jds/ai-email-reinforce-zoom-summary-database
  jds/ai-email-reinforce-zoom-summary-artifact
  "Reinforcement database for AI Zoom summaries.")

(defconst jds/ai-email--reinforce-database-specs
  `((,jds/ai-email-reinforce-reply-database
     :candidate-fn jds/ai-email--reinforce-reply-candidate
     :context-fn jds/ai-email--reinforce-reply-context)
    (,jds/ai-email-reinforce-scheduling-reply-database
     :candidate-fn jds/ai-email--reinforce-scheduling-reply-candidate
     :context-fn jds/ai-email--reinforce-scheduling-reply-context)
    (,jds/ai-email-reinforce-thread-summary-database
     :candidate-fn jds/ai-email--reinforce-thread-summary-candidate
     :context-fn jds/ai-email--reinforce-thread-summary-context)
    (,jds/ai-email-reinforce-zoom-summary-database
     :candidate-fn jds/ai-email--reinforce-zoom-summary-candidate
     :context-fn jds/ai-email--reinforce-zoom-summary-context))
  "Database registrations for ai-email reinforcement workflows.")

(defconst jds/ai-email--reinforce-reply-summarizer-guidance
  (concat
   "For this reply-writing workflow, treat output-feedback events as the primary signal.\n"
   "Item-feedback is weak background context only and should usually be ignored unless it clearly repeats a stable, reply-relevant pattern.\n"
   "Focus on what users liked or disliked about the generated reply text itself: tone, structure, directness, completeness, and actionability.")
  "Summarizer guidance for AI email reply artifacts.")

(defconst jds/ai-email--reinforce-reply-updater-guidance
  (concat
   "Update this reply prompt mainly from output-feedback evidence.\n"
   "Treat item-feedback as low-priority background signal and ignore it when it would push the prompt away from better reply phrasing or structure.\n"
   "Prefer small edits that improve clarity, tone, and usefulness of generated replies.")
  "Updater guidance for AI email reply artifacts.")

(defconst jds/ai-email--reinforce-summary-summarizer-guidance
  (concat
   "For this summarization workflow, treat output-feedback events as the primary signal.\n"
   "Item-feedback is weak background context only and should usually be ignored unless it clearly identifies a recurring summarization failure.\n"
   "Focus on what users liked or disliked about the generated summary text itself: coverage, salience, brevity, structure, and correctness.")
  "Summarizer guidance for AI email summary artifacts.")

(defconst jds/ai-email--reinforce-summary-updater-guidance
  (concat
   "Update this summarization prompt mainly from output-feedback evidence.\n"
   "Treat item-feedback as low-priority background signal and ignore it when it would blur the prompt across unrelated summary cases.\n"
   "Prefer small edits that improve summary quality, not domain ranking behavior.")
  "Updater guidance for AI email summary artifacts.")

(defconst jds/ai-email--reinforce-artifact-specs
  `((,jds/ai-email-reinforce-reply-artifact
     :database ,jds/ai-email-reinforce-reply-database
     :type "prompt"
     :summarizer-user-prompt ,jds/ai-email--reinforce-reply-summarizer-guidance
     :updater-user-prompt ,jds/ai-email--reinforce-reply-updater-guidance)
    (,jds/ai-email-reinforce-scheduling-reply-artifact
     :database ,jds/ai-email-reinforce-scheduling-reply-database
     :type "prompt"
     :summarizer-user-prompt ,jds/ai-email--reinforce-reply-summarizer-guidance
     :updater-user-prompt ,jds/ai-email--reinforce-reply-updater-guidance)
    (,jds/ai-email-reinforce-thread-summary-artifact
     :database ,jds/ai-email-reinforce-thread-summary-database
     :type "prompt"
     :summarizer-user-prompt ,jds/ai-email--reinforce-summary-summarizer-guidance
     :updater-user-prompt ,jds/ai-email--reinforce-summary-updater-guidance)
    (,jds/ai-email-reinforce-zoom-summary-artifact
     :database ,jds/ai-email-reinforce-zoom-summary-database
     :type "prompt"
     :summarizer-user-prompt ,jds/ai-email--reinforce-summary-summarizer-guidance
     :updater-user-prompt ,jds/ai-email--reinforce-summary-updater-guidance))
  "Artifact registrations for ai-email reinforcement workflows.")

(defvar jds/ai-email--reinforce-registered nil
  "Whether `ai-email.el' has registered its gptel-reinforce integration.")

(defvar-local jds/ai-email-reinforce-database nil
  "Buffer-local ai-email reinforcement database name.")

(defvar-local jds/ai-email-reinforce-context nil
  "Buffer-local item context used by ai-email reinforcement candidates.")

(defun jds/ai-email--reinforce-candidate (database-name)
  "Return a reinforcement candidate for DATABASE-NAME in the current buffer."
  (when (and jds/ai-email-reinforce-context
             (equal jds/ai-email-reinforce-database database-name))
    (list :priority 100
          :label (format "ai-email %s" database-name)
          :context jds/ai-email-reinforce-context)))

(defun jds/ai-email--reinforce-reply-candidate ()
  "Return the ai-email reply reinforcement candidate for the current buffer."
  (jds/ai-email--reinforce-candidate jds/ai-email-reinforce-reply-database))

(defun jds/ai-email--reinforce-scheduling-reply-candidate ()
  "Return the scheduling-reply reinforcement candidate for the current buffer."
  (jds/ai-email--reinforce-candidate
   jds/ai-email-reinforce-scheduling-reply-database))

(defun jds/ai-email--reinforce-thread-summary-candidate ()
  "Return the thread-summary reinforcement candidate for the current buffer."
  (jds/ai-email--reinforce-candidate
   jds/ai-email-reinforce-thread-summary-database))

(defun jds/ai-email--reinforce-zoom-summary-candidate ()
  "Return the zoom-summary reinforcement candidate for the current buffer."
  (jds/ai-email--reinforce-candidate
   jds/ai-email-reinforce-zoom-summary-database))

(defun jds/ai-email--reinforce-context-only (database-name)
  "Return ai-email reinforcement context for DATABASE-NAME in the current buffer."
  (when (equal jds/ai-email-reinforce-database database-name)
    jds/ai-email-reinforce-context))

(defun jds/ai-email--reinforce-reply-context ()
  "Return the ai-email reply reinforcement context for the current buffer."
  (jds/ai-email--reinforce-context-only jds/ai-email-reinforce-reply-database))

(defun jds/ai-email--reinforce-scheduling-reply-context ()
  "Return the ai-email scheduling-reply reinforcement context for the buffer."
  (jds/ai-email--reinforce-context-only
   jds/ai-email-reinforce-scheduling-reply-database))

(defun jds/ai-email--reinforce-thread-summary-context ()
  "Return the ai-email thread-summary reinforcement context for the buffer."
  (jds/ai-email--reinforce-context-only
   jds/ai-email-reinforce-thread-summary-database))

(defun jds/ai-email--reinforce-zoom-summary-context ()
  "Return the ai-email zoom-summary reinforcement context for the buffer."
  (jds/ai-email--reinforce-context-only
   jds/ai-email-reinforce-zoom-summary-database))

(defun jds/ai-email--reinforce-context-for-message (msg workflow)
  "Return a minimal reinforcement context for MSG and WORKFLOW."
  (let* ((message-id (or (mu4e-message-field msg :message-id) ""))
         (subject (or (mu4e-message-field msg :subject) "(no subject)"))
         (item-key (if (string-empty-p message-id)
                       (format "ai-email:%s:%s"
                               workflow
                               (secure-hash
                                'sha1
                                (format "%s|%s|%s"
                                        workflow
                                        subject
                                        (mu4e-message-field msg :date))))
                     (format "ai-email:%s:%s" workflow message-id))))
    (list :item-key item-key
          :title subject
          :meta (list :workflow workflow
                      :message-id (unless (string-empty-p message-id) message-id)))))

(defun jds/ai-email--reinforce-setup-buffer (buffer database context)
  "Attach ai-email reinforcement DATABASE and CONTEXT to BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local jds/ai-email-reinforce-database database)
      (setq-local jds/ai-email-reinforce-context context)
      (when (fboundp 'gptel-reinforce-set-active-database)
        (gptel-reinforce-set-active-database database)))))

(defun jds/ai-email--reinforce-track-output-region (buffer artifact start end)
  "Track BUFFER region START..END as ARTIFACT output when available."
  (when (and (buffer-live-p buffer)
             (fboundp 'gptel-reinforce-track-output-region)
             (< start end))
    (with-current-buffer buffer
      (gptel-reinforce-track-output-region artifact start end))))

(defun jds/ai-email--register-reinforce-database (spec)
  "Register one ai-email reinforcement database from SPEC."
  (pcase-let ((`(,name . ,plist) spec))
    (apply #'gptel-reinforce-register-database
           :name name
           plist)))

(defun jds/ai-email--register-reinforce-artifact (spec)
  "Register one ai-email reinforcement artifact from SPEC."
  (pcase-let ((`(,name . ,plist) spec))
    (apply #'gptel-reinforce-register-artifact
           :name name
           plist)))

(defun jds/ai-email--register-reinforce-integration ()
  "Register ai-email databases and artifacts with gptel-reinforce."
  (when (and (featurep 'gptel-reinforce)
             (not jds/ai-email--reinforce-registered))
    (mapc #'jds/ai-email--register-reinforce-database
          jds/ai-email--reinforce-database-specs)
    (mapc #'jds/ai-email--register-reinforce-artifact
          jds/ai-email--reinforce-artifact-specs)
    (setq jds/ai-email--reinforce-registered t)))

(with-eval-after-load 'gptel-reinforce
  (jds/ai-email--register-reinforce-integration))

(when (featurep 'gptel-reinforce)
  (jds/ai-email--register-reinforce-integration))

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

(defcustom jds/scheduling-weekday-mode-alist
  '((0 . "either")
    (1 . "zoom")
    (2 . "either")
    (3 . "zoom")
    (4 . "either")
    (5 . "zoom")
    (6 . "either"))
  "Alist mapping `decoded-time-weekday' index to preferred meeting mode.
0=Sunday, 1=Monday, ..., 6=Saturday.
Each value is \"zoom\", \"in_person\", or \"either\"."
  :type '(alist :key-type integer :value-type string)
  :group 'jds/scheduling)

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

(defun jds/org-calendar--advance-by-repeater (time value unit)
  "Advance TIME by VALUE UNITs using exact calendar arithmetic."
  (pcase unit
    ('hour  (time-add time (seconds-to-time (* value 3600))))
    ('day   (time-add time (days-to-time value)))
    ('week  (time-add time (days-to-time (* 7 value))))
    ('month (pcase-let ((`(,sec ,min ,hour ,day ,month ,year . ,_)
                         (decode-time time)))
              (let* ((raw-month (+ month value))
                     (new-year  (+ year (/ (1- raw-month) 12)))
                     (new-month (1+ (mod (1- raw-month) 12))))
                (encode-time sec min hour day new-month new-year))))
    ('year  (pcase-let ((`(,sec ,min ,hour ,day ,month ,year . ,_)
                         (decode-time time)))
              (encode-time sec min hour day month (+ year value))))
    (_      nil)))

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
                         (let ((file-name (file-name-nondirectory file))
                               (title     (org-element-property :raw-value headline))
                               (todo      (org-element-property :todo-keyword headline))
                               (path      (jds/org-calendar--headline-path headline))
                               (ts-raw    (org-element-property :raw-value ts))
                               (rep-type  (org-element-property :repeater-type ts))
                               (rep-val   (org-element-property :repeater-value ts))
                               (rep-unit  (org-element-property :repeater-unit ts)))
                           (push (list :file-name file-name :title title :todo todo
                                       :path path :timestamp ts-raw :start start :end end)
                                 events)
                           (when (and rep-type rep-val rep-unit (> rep-val 0))
                             (let* ((duration   (time-subtract end start))
                                    (horizon    (time-add (current-time) (days-to-time 365)))
                                    (next-start (jds/org-calendar--advance-by-repeater
                                                 start rep-val rep-unit)))
                               (while (and next-start (time-less-p next-start horizon))
                                 (let ((next-end (time-add next-start duration)))
                                   (push (list :file-name file-name :title title :todo todo
                                               :path path :timestamp ts-raw
                                               :start next-start :end next-end)
                                         events))
                                 (setq next-start (jds/org-calendar--advance-by-repeater
                                                   next-start rep-val rep-unit))))))))))))))
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
  (let ((dow (decoded-time-weekday (decode-time day-time))))
    (or (alist-get dow jds/scheduling-weekday-mode-alist) "either")))

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
       ("no_availability" . ,(if (null candidates) t :json-false))
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
  "Insert TEXT into BUF at POS, tolerating stale markers.
Return (START . END) for the inserted text, or nil when nothing was inserted."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (goto-char (jds/ai-email--live-insertion-point buf pos))
      (let ((start (point)))
        (insert text "\n\n")
        (prog1 (cons start (- (point) 2))
          (when (and (markerp pos)
                     (marker-buffer pos))
            (set-marker pos nil)))))))

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

(defun jds/ai-email--insert-if-final-string (response info buf pos &optional artifact)
  "Insert RESPONSE into BUF at POS when RESPONSE is the final string result.
When ARTIFACT is non-nil, track the inserted output for reinforcement."
  (cond
   ((not response)
    (message "gptel error: %s" (plist-get info :status)))
   ((stringp response)
    (when-let ((region
                (jds/ai-email--insert-response-at-point
                 buf pos (jds/ai-email--sanitize-response response))))
      (when artifact
        (jds/ai-email--reinforce-track-output-region
         buf artifact (car region) (cdr region)))))
   (t nil)))

(defvar jds/ai-email-last-prompt nil
  "Full prompt sent in the most recent AI email request, for debugging.")

(defun jds/ai-email--request-inserting-response
    (prompt system buf callback &optional tools)
  "Run `gptel-request' for PROMPT and insert into BUF via CALLBACK.
TOOLS, when non-nil, are bound for the request."
  (setq jds/ai-email-last-prompt prompt)
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
              &optional tools artifact)
  "Handle RESPONSE for insert-at-point flows that may need one retry.
RETRY-SYSTEM-SUFFIX is appended to SYSTEM for the retry request when the model
returns planning text. INVALID-MESSAGE is shown if the model still fails after
retries. TOOLS, when non-nil, are provided on the retry request. When ARTIFACT
is non-nil, track the inserted output for reinforcement."
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
                retry-system-suffix invalid-message tools artifact))
             tools))
        (if (jds/ai-email--planning-response-p clean)
            (progn
              (jds/ai-email--delete-leading-planning-text buf pos)
              (message "%s" invalid-message))
          (when-let ((region (jds/ai-email--insert-response-at-point buf pos clean)))
            (when artifact
              (jds/ai-email--reinforce-track-output-region
               buf artifact (car region) (cdr region))))))))
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

(defun jds/ai-email--compose-mu4e-reply-with-ai
    (prompt-builder system callback &optional tools reinforce-database reinforce-context)
  "Compose a mu4e reply and invoke AI with PROMPT-BUILDER and CALLBACK.
PROMPT-BUILDER is called with the compose buffer contents and should return the
prompt text. CALLBACK receives RESPONSE, INFO, prompt, SYSTEM, buffer and
marker. TOOLS, when non-nil, are passed to the request. When REINFORCE-DATABASE
and REINFORCE-CONTEXT are non-nil, attach them to the compose buffer."
  (let (hook-fn)
    (setq hook-fn
          (lambda ()
            (remove-hook 'mu4e-compose-mode-hook hook-fn)
            (let* ((buf (current-buffer))
                   (content (buffer-substring-no-properties (point-min) (point-max)))
                   (prompt (funcall prompt-builder content)))
              (when (and reinforce-database reinforce-context)
                (jds/ai-email--reinforce-setup-buffer
                 buf reinforce-database reinforce-context))
              (message-goto-body)
              (let ((pos (copy-marker (point))))
                (jds/ai-email--request-inserting-response
                 prompt system buf
                 (lambda (response info)
                   (funcall callback response info prompt system buf pos))
                 tools)))))
    (add-hook 'mu4e-compose-mode-hook hook-fn)
    (jds/mu4e-compose-reply)))
