;;; common.el --- Shared ai-email helpers -*- lexical-binding: t; -*-

;;; Calendar availability tool ---------------------------------------------

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-element)
(require 'subr-x)

(defgroup jds/ai-email nil
  "AI email helpers and workflows."
  :group 'applications
  :prefix "jds/ai-email-")

(defvar mu4e-compose-parent-message nil
  "mu4e parent message for the current compose buffer.")

(defcustom jds/ai-email-debug-normalization nil
  "When non-nil, log ai-email normalization traces for inspection.
Only logs cases where the normalized output materially differs from the raw
generation output."
  :type 'boolean
  :group 'jds/ai-email)

(defcustom jds/ai-email-debug-buffer-name "*ai-email normalization debug*"
  "Buffer name used for optional ai-email normalization debug logs."
  :type 'string
  :group 'jds/ai-email)

(declare-function gptel-reinforce-set-active-database "gptel-reinforce" (database))
(declare-function gptel-reinforce-track-output-region "gptel-reinforce" (artifact start end &optional output-id))
(declare-function gptel-reinforce-register-database "gptel-reinforce" (&rest plist))
(declare-function gptel-reinforce-register-artifact "gptel-reinforce" (&rest plist))
(declare-function gptel-reinforce-get-artifact "gptel-reinforce-core" (name))
(declare-function gptel-reinforce-org-read-current "gptel-reinforce-org" (artifact))
(declare-function jds/ai-email-stage-scheduling-capture "capture"
                  (start end &optional title all_day modality location conference_url notes))

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

(defconst jds/ai-email-reinforce-capture-artifact "ai-email-capture"
  "Artifact name for AI email capture extraction.")

(defconst jds/ai-email-reinforce-capture-database
  jds/ai-email-reinforce-capture-artifact
  "Reinforcement database for AI email capture extraction.")

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
     :context-fn jds/ai-email--reinforce-zoom-summary-context)
    (,jds/ai-email-reinforce-capture-database
     :candidate-fn jds/ai-email--reinforce-capture-candidate
     :context-fn jds/ai-email--reinforce-capture-context))
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

(defconst jds/ai-email--reinforce-capture-summarizer-guidance
  (concat
   "For this email-capture extraction workflow, treat output-feedback as the primary signal.\n"
   "Focus on what users liked or disliked about the extracted candidates: precision, recall, accuracy of dates/times, and relevance of todos.")
  "Summarizer guidance for AI email capture artifacts.")

(defconst jds/ai-email--reinforce-capture-updater-guidance
  (concat
   "Update this extraction prompt mainly from output-feedback.\n"
   "Prefer edits that improve the accuracy and relevance of extracted events and todos.\n"
   "Do not alter the JSON schema or the dynamic date/scope instructions — those are injected at call time.")
  "Updater guidance for AI email capture artifacts.")

(defconst jds/ai-email--reinforce-capture-initial-text
  (concat
   "You extract Org capture candidates from email text.\n"
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
   "When unsure, return fewer candidates, not more.")
  "Initial artifact text for AI email capture extraction (static rules; date and scope injected at call time).")

(defconst jds/ai-email--reinforce-scheduling-initial-text
  (string-join
   '("You are an AI assistant helping compose email replies for scheduling discussions."
     "Your goal is to generate natural, contextually aware responses that move scheduling forward efficiently."
     ""
     "## Core principles"
     ""
     "1. **Match conversational tone**: Write as if continuing a natural dialogue, not a formal template."
     "   Extract only practical information (dates, times, constraints) from the user's notes. Never echo back the user's internal framing or reasoning."
     ""
     "2. **Treat explicit constraints as binding**: If the user names a time, day, or narrow window, or the other person says they can only meet at certain times, stay inside those constraints."
     "   Do not suggest options outside the stated availability."
     ""
     "3. **Filter, don't list**: When someone has already proposed specific times or narrowed a window, respond with 1–2 concrete options rather than exhaustively listing all availability."
     "   Prioritize the options that best fit the thread."
     ""
     "4. **Respect context**: Read the full conversation thread to understand what has already been discussed, rejected, or accepted."
     "   Avoid repeating information or options the other party has already addressed."
     ""
     "5. **Commit when the meeting is effectively settled**: If the other person gives a clear availability window and one concrete slot plainly fits, prefer committing to that slot rather than asking for another round of confirmation."
     "   Use commitment language like \"Let's do Thursday at 12:00 PM\" instead of proposal language like \"How about Thursday at 12:00 PM?\"."
     "   However, if other attendees still need to confirm, or if the latest message is only proposing a time to the group, stay in coordination mode and do not treat the slot as settled."
     "   A concrete slot is not settled until every attendee whose availability matters has explicitly agreed, unless you are writing the last required acceptance to an already-proposed time."
     ""
     "6. **Be concise and natural**: Keep language conversational and direct."
     "   Do not expose reasoning or mention sending a calendar invite unless the user explicitly asks for that."
     ""
     "## When composing replies"
     ""
     "- Extract the user's actual availability/constraints, not their internal reasoning."
     "- If multiple times are possible within constraints, lead with the strongest 1–2 options."
     "- If the slot is already effectively settled, reply with a brief confirmation instead of another question."
     "- Before treating a slot as settled, check whether the visible recipients/context imply other attendees whose availability still matters."
     "- Do not convert one participant's suggested time into a full confirmation when another participant has not yet explicitly accepted it."
     "- Use simple, direct language."
     "- If confirming a time, be brief and clear."
     "- If proposing alternatives, explain why briefly (e.g., \"Tuesday works better for me than Wednesday\").")
   "\n")
  "Initial artifact text for the AI scheduling reply prompt.")

(defun jds/ai-email--scheduling-artifact-text ()
  "Return the current text of the scheduling reply artifact, or nil when empty."
  (when (featurep 'gptel-reinforce)
    (when-let* ((artifact (gptel-reinforce-get-artifact
                           jds/ai-email-reinforce-scheduling-reply-artifact))
                (current (gptel-reinforce-org-read-current artifact))
                (text (string-trim (or (plist-get current :text) ""))))
      (unless (string-empty-p text)
        text))))

(defun jds/ai-email--capture-artifact-text ()
  "Return the current text of the capture artifact, or nil when empty."
  (when (featurep 'gptel-reinforce)
    (when-let* ((artifact (gptel-reinforce-get-artifact
                           jds/ai-email-reinforce-capture-artifact))
                (current (gptel-reinforce-org-read-current artifact))
                (text (string-trim (or (plist-get current :text) ""))))
      (unless (string-empty-p text)
        text))))

(defconst jds/ai-email--reinforce-artifact-specs
  `((,jds/ai-email-reinforce-reply-artifact
     :database ,jds/ai-email-reinforce-reply-database
     :type "prompt"
     :summarizer-user-prompt ,jds/ai-email--reinforce-reply-summarizer-guidance
     :updater-user-prompt ,jds/ai-email--reinforce-reply-updater-guidance)
    (,jds/ai-email-reinforce-scheduling-reply-artifact
     :database ,jds/ai-email-reinforce-scheduling-reply-database
     :type "prompt"
     :initial-text ,jds/ai-email--reinforce-scheduling-initial-text
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
     :updater-user-prompt ,jds/ai-email--reinforce-summary-updater-guidance)
    (,jds/ai-email-reinforce-capture-artifact
     :database ,jds/ai-email-reinforce-capture-database
     :type "prompt"
     :initial-text ,jds/ai-email--reinforce-capture-initial-text
     :summarizer-user-prompt ,jds/ai-email--reinforce-capture-summarizer-guidance
     :updater-user-prompt ,jds/ai-email--reinforce-capture-updater-guidance))
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

(defun jds/ai-email--reinforce-capture-candidate ()
  "Return the capture reinforcement candidate for the current buffer."
  (jds/ai-email--reinforce-candidate
   jds/ai-email-reinforce-capture-database))

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

(defun jds/ai-email--reinforce-capture-context ()
  "Return the ai-email capture reinforcement context for the buffer."
  (jds/ai-email--reinforce-context-only
   jds/ai-email-reinforce-capture-database))

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

(defvar jds/scheduling-workday-end-hour 16
  "Hour when the scheduling workday ends.")

(defvar jds/scheduling-workday-end-minute 30
  "Minute within `jds/scheduling-workday-end-hour' when the workday ends.")

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

(defun jds/org-calendar--at-hour (time hour &optional minute)
  "Return TIME's date at HOUR:MINUTE (MINUTE defaults to 0)."
  (pcase-let* ((`(,_sec ,_min ,_hour ,day ,month ,year . ,_)
                (decode-time time)))
    (encode-time 0 (or minute 0) hour day month year)))

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
      (equal day-mode "either")
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

(defun jds/org-calendar--parse-clock-string (clock-string)
  "Return (HOUR . MINUTE) for CLOCK-STRING in 24-hour HH:MM format."
  (when (and (stringp clock-string)
             (string-match
              "\\`\\([01]?[0-9]\\|2[0-3]\\):\\([0-5][0-9]\\)\\'" clock-string))
    (cons (string-to-number (match-string 1 clock-string))
          (string-to-number (match-string 2 clock-string)))))

(defun jds/org-calendar--minute-of-day (time)
  "Return TIME as minutes since midnight."
  (pcase-let ((`(,_sec ,min ,hour . ,_) (decode-time time)))
    (+ (* hour 60) min)))

(defun jds/org-calendar--time-constraints-match-p
    (time exact-start earliest-start latest-start)
  "Whether TIME satisfies the given optional start-time constraints.
EXACT-START, EARLIEST-START, and LATEST-START are clock strings in HH:MM
24-hour format."
  (let* ((minute-of-day (jds/org-calendar--minute-of-day time))
         (exact (when exact-start
                  (jds/org-calendar--parse-clock-string exact-start)))
         (earliest (when earliest-start
                     (jds/org-calendar--parse-clock-string earliest-start)))
         (latest (when latest-start
                   (jds/org-calendar--parse-clock-string latest-start))))
    (and (or (null exact)
             (= minute-of-day (+ (* 60 (car exact)) (cdr exact))))
         (or (null earliest)
             (>= minute-of-day (+ (* 60 (car earliest)) (cdr earliest))))
         (or (null latest)
             (<= minute-of-day (+ (* 60 (car latest)) (cdr latest)))))))

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
    (window-start window-end duration-seconds increment-seconds mode time-pref
                  &optional exact-start earliest-start latest-start)
  "Return candidate slots within WINDOW-START and WINDOW-END."
  (let ((slots nil)
        (slot-start window-start))
    (while (not (time-less-p window-end
                             (time-add slot-start duration-seconds)))
      (when (and (jds/org-calendar--time-of-day-matches-p slot-start time-pref)
                 (jds/org-calendar--time-constraints-match-p
                  slot-start exact-start earliest-start latest-start))
        (let ((slot-end (time-add slot-start duration-seconds)))
          (push `(("start" . ,(jds/org-calendar--iso-datetime slot-start))
                  ("end" . ,(jds/org-calendar--iso-datetime slot-end))
                  ("mode" . ,mode)
                  ("display" . ,(jds/org-calendar--slot-display slot-start mode)))
                slots)))
      (setq slot-start (time-add slot-start increment-seconds)))
    (nreverse slots)))

(defun jds/org-calendar--free-windows-for-day
    (day-start busy-intervals duration-seconds mode-preference time-preference
     &optional inperson-override exact-start earliest-start latest-start)
  "Return free windows and candidate slots for a single day.
When INPERSON-OVERRIDE is non-nil and the day's configured mode is \"zoom\",
treat the day as \"in_person\" instead, allowing in-person slots to be generated."
  (let* ((work-start (jds/org-calendar--at-hour day-start jds/scheduling-workday-start-hour))
         (work-end   (jds/org-calendar--at-hour day-start jds/scheduling-workday-end-hour
                                                jds/scheduling-workday-end-minute))
         (day-mode   (let ((raw (jds/org-calendar--day-mode day-start)))
                       (if (and inperson-override (equal raw "zoom"))
                           "in_person"
                         raw)))
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
                        time-preference
                        exact-start
                        earliest-start
                        latest-start))))))
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
    (start_date end_date duration_minutes
                &optional count mode_preference time_of_day_preference
                inperson_on_zoom_days exact_start_time earliest_start_time
                latest_start_time)
  "Return validated free meeting slots as structured JSON."
  (let* ((start-time (jds/org-calendar--day-start
                      (jds/org-calendar--parse-date start_date)))
         (end-time   (jds/org-calendar--at-hour
                      (jds/org-calendar--parse-date end_date) 23))
         (duration   (max 1 (truncate duration_minutes)))
         (slot-count (max 1 (truncate (or count jds/scheduling-default-slot-count))))
         (mode-pref  (or mode_preference "either"))
         (time-pref  (or time_of_day_preference "either"))
         (inperson-override (and inperson_on_zoom_days
                                 (not (eq inperson_on_zoom_days :json-false))
                                 (not (equal inperson_on_zoom_days "false"))))
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
               day-start busy-intervals duration-seconds mode-pref time-pref
               inperson-override exact_start_time earliest_start_time
               latest_start_time)))
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
       ("exact_start_time" . ,(or exact_start_time json-null))
       ("earliest_start_time" . ,(or earliest_start_time json-null))
       ("latest_start_time" . ,(or latest_start_time json-null))
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
          '(:name "time_of_day_preference" :type string :optional t :description "Time preference: morning, afternoon, or either")
          '(:name "inperson_on_zoom_days" :type boolean :optional t :description "When true, treat zoom-preferred days (normally Mon/Wed/Fri) as available for in-person meetings. Use only when the user has explicitly said in-person is acceptable on those days.")
          '(:name "exact_start_time" :type string :optional t :description "Exact requested slot start time in 24-hour HH:MM format, for requests like 'at 3pm'.")
          '(:name "earliest_start_time" :type string :optional t :description "Lower bound on slot start time in 24-hour HH:MM format, for requests like 'after 1pm'.")
          '(:name "latest_start_time" :type string :optional t :description "Upper bound on slot start time in 24-hour HH:MM format, for requests like 'before 4pm' or 'by 1pm'."))
   :function #'jds/find-free-times)
  "Gptel tool for scheduling against validated org calendar availability.")

(defvar jds~gptel-stage-calendar-capture-tool
  (gptel-make-tool
   :name "stage_calendar_capture"
   :description
   "Stage a confirmed meeting in the AI email capture review buffer. Use this only when the thread is settled enough that the final reply is confirming a concrete meeting, not when merely proposing options. This does not write directly to Org; it only opens the user's normal review buffer."
   :args (list
          '(:name "start" :type string :description "Confirmed event start in YYYY-MM-DDTHH:MM or YYYY-MM-DD format")
          '(:name "end" :type string :optional t :description "Confirmed event end in YYYY-MM-DDTHH:MM or YYYY-MM-DD format")
          '(:name "title" :type string :optional t :description "Short calendar title")
          '(:name "all_day" :type boolean :optional t :description "Whether this is an all-day event")
          '(:name "modality" :type string :optional t :description "Meeting modality: zoom, in_person, or unknown")
          '(:name "location" :type string :optional t :description "Physical location if known")
          '(:name "conference_url" :type string :optional t :description "Meeting URL if known")
          '(:name "notes" :type string :optional t :description "Optional note to include in the staged event body"))
   :function #'jds/ai-email-stage-scheduling-capture)
  "Gptel tool for staging confirmed scheduling captures.")


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

(defun jds/ai-email--reply-wrapper-prefix-p (text)
  "Whether TEXT is a wrapper prefix like \"Here's your reply:\"."
  (let ((clean (downcase (string-trim (or text "")))))
    (or (string-match-p
         (rx string-start
             (or "here's your reply"
                 "here is your reply"
                 "here's the reply"
                 "here is the reply"
                 "draft reply"
                 "reply")
             (? ":")
             string-end)
         clean)
        (string-match-p
         (rx string-start
             (or "here's your email"
                 "here is your email"
                 "here's the email"
                 "here is the email"
                 "email draft")
             (? ":")
             string-end)
         clean))))

(defun jds/ai-email--extract-final-email-shaped-draft (text)
  "Return the last email-shaped draft in TEXT when one is clearly embedded.
This handles model outputs that contain an earlier draft followed by a more
complete final draft starting with a greeting line."
  (let ((case-fold-search t)
        (start nil))
    (with-temp-buffer
      (insert (or text ""))
      (goto-char (point-min))
      (while (re-search-forward
              (rx line-start
                  (* blank)
                  (or "hi" "hello" "dear")
                  (+ (not (any "\n")))
                  line-end)
              nil t)
        (setq start (match-beginning 0)))
      (when (and start
                 (> start (point-min))
                 (save-excursion
                   (goto-char start)
                   (re-search-forward
                    (rx line-start
                        (* blank)
                        (or "best,"
                            "best regards,"
                            "regards,"
                            "sincerely,"
                            "thanks,"
                            "thank you,"
                            "cheers,")
                        (* blank)
                        line-end)
                    nil t)))
        (string-trim
         (buffer-substring-no-properties start (point-max)))))))

(defun jds/ai-email--count-line-matches (text regexp)
  "Return the number of lines in TEXT matching REGEXP."
  (let ((count 0))
    (with-temp-buffer
      (insert (or text ""))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq count (1+ count))))
    count))

(defun jds/ai-email--reply-has-multiple-drafts-p (text)
  "Return non-nil when TEXT appears to contain more than one reply draft."
  (let* ((clean (jds/ai-email--strip-code-fences text))
         (tagged (jds/ai-email--extract-tagged-reply clean))
         (body (string-trim (or tagged clean)))
         (case-fold-search t)
         (greeting-re (rx line-start
                          (* blank)
                          (or "hi" "hello" "dear")
                          (+ (not (any "\n")))
                          line-end))
         (signoff-re (rx line-start
                         (* blank)
                         (or "best,"
                             "best regards,"
                             "regards,"
                             "sincerely,"
                             "thanks,"
                             "thank you,"
                             "cheers,")
                         (* blank)
                         line-end))
         (greeting-count (jds/ai-email--count-line-matches body greeting-re))
         (signoff-count (jds/ai-email--count-line-matches body signoff-re))
         (first-greeting-pos
          (with-temp-buffer
            (insert body)
            (goto-char (point-min))
            (when (re-search-forward greeting-re nil t)
              (match-beginning 0))))
         (trailing-after-signoff
          (with-temp-buffer
            (insert body)
            (goto-char (point-min))
            (when (re-search-forward signoff-re nil t)
              (forward-line 1)
              ;; Skip the sender-name line if present, then inspect anything after it.
              (when (looking-at (rx (* blank) (+ (not (any "\n"))) line-end))
                (forward-line 1))
              (string-trim
               (buffer-substring-no-properties (point) (point-max)))))))
    (or (> greeting-count 1)
        (> signoff-count 1)
        (and trailing-after-signoff
             (not (string-empty-p trailing-after-signoff)))
        (and first-greeting-pos
             (> first-greeting-pos (point-min))
             (not (string-empty-p
                   (string-trim
                    (substring body (point-min) first-greeting-pos))))
             (> signoff-count 0)))))

(defun jds/ai-email--sanitize-response (response)
  "Return lightly cleaned RESPONSE text.
This is a deterministic fallback used before and after AI normalization."
  (let* ((clean  (jds/ai-email--strip-code-fences response))
         (tagged (jds/ai-email--extract-tagged-reply clean)))
    (setq clean (or tagged clean))
    (setq clean (or (jds/ai-email--extract-final-email-shaped-draft clean)
                    clean))
    (string-trim clean)))

(defvar jds/ai-email-last-raw-response nil
  "Raw text returned by the most recent ai-email generation pass.")

(defvar jds/ai-email--pending-compose-request nil
  "Pending ai-email compose request consumed by the next mu4e compose buffer.
This stores a plist with :prompt-builder, :system, :callback, :tools,
:reinforce-database, and :reinforce-context.")

(defun jds/ai-email--valid-reply-text-p (text)
  "Return non-nil when TEXT looks like a usable reply body.
When RAW-RESPONSE is provided, reject outputs that still contain multiple
plausible reply drafts."
  (let ((clean (string-trim (or text ""))))
    (and (not (string-empty-p clean))
         (not (jds/ai-email--reply-wrapper-prefix-p clean)))))

(defun jds/ai-email--valid-final-reply-text-p (text &optional raw-response)
  "Return non-nil when TEXT is a usable single final reply body."
  (let ((clean (string-trim (or text ""))))
    (and (not (string-empty-p clean))
         (not (jds/ai-email--reply-wrapper-prefix-p clean))
         (not (jds/ai-email--reply-has-multiple-drafts-p
               (or raw-response clean))))))

(defun jds/ai-email--org-msg-editable-body-point (buf)
  "Return the first editable body position in an org-msg compose buffer.
Falls back to the regular message body when no org-msg scaffold is present."
  (with-current-buffer buf
    (save-excursion
      (message-goto-body)
      (let ((body-start (point))
            (scan-limit (min (point-max) (+ (point) 2000))))
        (if (and (re-search-forward "^#\\+OPTIONS:" scan-limit t)
                 (re-search-forward "^:PROPERTIES:[ \t]*$" scan-limit t)
                 (re-search-forward "^:END:[ \t]*$" scan-limit t))
            (progn
              (forward-line 1)
              (skip-chars-forward "\n\t ")
              (point))
          body-start)))))

(defun jds/ai-email--editable-body-point (buf)
  "Return the earliest safe insertion point for AI text in BUF."
  (with-current-buffer buf
    (save-excursion
      (message-goto-body)
      (let ((body-start (point)))
        (max body-start
             (jds/ai-email--org-msg-editable-body-point buf))))))

(defun jds/ai-email--live-insertion-point (buf pos)
  "Return a safe insertion point in BUF based on marker POS."
  (let ((floor (jds/ai-email--editable-body-point buf)))
    (with-current-buffer buf
      (cond
       ((and (markerp pos)
             (marker-buffer pos)
             (eq (marker-buffer pos) buf))
        (max floor (marker-position pos)))
       (t floor)))))

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

(defvar jds/ai-email-last-prompt nil
  "Full prompt sent in the most recent AI email request, for debugging.")

(defvar jds/ai-email-last-tool-results nil
  "Most recent gptel tool-result payload seen during an ai-email request.")

(defvar jds/ai-email--request-buffer-name " *ai-email-gptel*"
  "Hidden buffer name used as the stable source buffer for ai-email gptel requests.")

(defun jds/ai-email--request-buffer ()
  "Return the stable hidden buffer used for ai-email gptel requests."
  (get-buffer-create jds/ai-email--request-buffer-name))

(defun jds/ai-email--request-response
    (prompt system buf callback &optional tools)
  "Run `gptel-request' for PROMPT in BUF via CALLBACK.
TOOLS, when non-nil, are bound for the request."
  (setq jds/ai-email-last-prompt prompt)
  (let ((gptel-include-reasoning nil)
        (gptel-use-tools (and tools t))
        (gptel-tools tools))
    (gptel-request prompt
                   :system system
                   :stream nil
                   :buffer (jds/ai-email--request-buffer)
                   :callback callback)))

(defun jds/ai-email--request-cleaned-response
    (prompt system buf callback &optional tools)
  "Request a raw model response and call CALLBACK with cleaned text.
PROMPT and SYSTEM drive the generation pass in BUF. TOOLS are available only
to the generation pass."
  (jds/ai-email--request-response
   prompt system buf
   (lambda (response info)
     (cond
      ((not response)
       (message "gptel error: %s" (plist-get info :status)))
      ((and (consp response)
            (eq (car response) 'tool-result))
       (setq jds/ai-email-last-tool-results (cdr response))
       nil)
      ((and (consp response)
            (memq (car response) '(tool-call reasoning)))
       nil)
      ((stringp response)
       ;; Skip intermediate responses that accompany a tool call — gptel calls
       ;; the callback once with any text produced alongside the tool call and
       ;; again with the final reply after the tool runs.  Only the final
       ;; response (no pending :tool-use) should be cleaned and inserted.
       (unless (plist-get info :tool-use)
         (setq jds/ai-email-last-raw-response response)
         (let ((clean (jds/ai-email--sanitize-response response)))
           (if (string-empty-p clean)
               (message "ai-email response was empty")
             (funcall callback clean)))))
      (t
       (message "ai-email: unhandled callback payload=%S status=%S"
                response (plist-get info :status)))))
   tools))

(defun jds/ai-email--request-inserting-response
    (prompt system buf pos &optional tools artifact after-insert)
  "Request cleaned text and insert it into BUF at POS."
  (jds/ai-email--request-cleaned-response
   prompt system buf
   (lambda (text)
     (let ((region (jds/ai-email--insert-response-at-point buf pos text)))
       (when-let ((inserted region))
         (when artifact
           (jds/ai-email--reinforce-track-output-region
            buf artifact (car inserted) (cdr inserted))))
        (when after-insert
          (funcall after-insert text region))))
   tools))

(defun jds/ai-email--compose-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is an editable email compose buffer."
  (with-current-buffer (or buffer (current-buffer))
    (derived-mode-p 'mu4e-compose-mode 'org-msg-edit-mode)))

(defun jds/ai-email--current-message (&optional msg noerror)
  "Return MSG or the relevant mu4e message for the current context.
In compose buffers, prefer `mu4e-compose-parent-message'."
  (or msg
      (and (jds/ai-email--compose-buffer-p)
           mu4e-compose-parent-message)
      (mu4e-message-at-point noerror)))

(defun jds/ai-email--compose-buffer-content (&optional buf)
  "Return the editable compose body contents for BUF."
  (with-current-buffer (or buf (current-buffer))
    (buffer-substring-no-properties
     (jds/ai-email--editable-body-point (current-buffer))
     (point-max))))

(defun jds/ai-email--mu4e-message-metadata (&optional msg)
  "Return plist with sender and subject metadata for MSG at point."
  (let* ((message (jds/ai-email--current-message msg))
         (from-c (car (mu4e-message-field message :from))))
    (list :message message
          :from (or (plist-get from-c :name)
                    (plist-get from-c :email)
                    "Unknown")
          :subject (or (mu4e-message-field message :subject) "(no subject)"))))

(defun jds/ai-email--compose-mu4e-reply-with-ai
    (prompt-builder system callback &optional tools reinforce-database reinforce-context)
  "Compose a mu4e reply or reuse the current draft with PROMPT-BUILDER and CALLBACK.
PROMPT-BUILDER is called with the compose buffer contents and should return the
prompt text. CALLBACK receives PROMPT, SYSTEM, buffer, marker, and TOOLS.
When REINFORCE-DATABASE and REINFORCE-CONTEXT are non-nil, attach them to the
compose buffer before invoking CALLBACK."
  (if (jds/ai-email--compose-buffer-p)
      (let* ((buf (current-buffer))
             (content (jds/ai-email--compose-buffer-content buf))
             (prompt (funcall prompt-builder content))
             (pos (copy-marker (point))))
        (when (and reinforce-database reinforce-context)
          (jds/ai-email--reinforce-setup-buffer
           buf reinforce-database reinforce-context))
        (funcall callback prompt system buf pos tools))
    (setq jds/ai-email--pending-compose-request
          (list :prompt-builder prompt-builder
                :system system
                :callback callback
                :tools tools
                :reinforce-database reinforce-database
                :reinforce-context reinforce-context))
    (jds/mu4e-compose-reply)))

(defun jds/ai-email--consume-pending-compose-request ()
  "Run the pending ai-email compose request in the current mu4e compose buffer."
  (when-let* ((request jds/ai-email--pending-compose-request))
    (setq jds/ai-email--pending-compose-request nil)
    (let* ((prompt-builder (plist-get request :prompt-builder))
           (system (plist-get request :system))
           (callback (plist-get request :callback))
           (tools (plist-get request :tools))
           (reinforce-database (plist-get request :reinforce-database))
           (reinforce-context (plist-get request :reinforce-context))
           (buf (current-buffer))
           (content (buffer-substring-no-properties (point-min) (point-max)))
           (prompt (funcall prompt-builder content)))
      (when (and reinforce-database reinforce-context)
        (jds/ai-email--reinforce-setup-buffer
         buf reinforce-database reinforce-context))
      (message-goto-body)
      (let ((pos (copy-marker (point))))
        (funcall callback prompt system buf pos tools)))))

(add-hook 'mu4e-compose-mode-hook #'jds/ai-email--consume-pending-compose-request)
