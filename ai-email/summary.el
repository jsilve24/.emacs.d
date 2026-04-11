;;; summary.el --- AI email summary workflows -*- lexical-binding: t; -*-

;;; AI Thread Summarization ------------------------------------------------

(defun jds/ai-email--thread-query-ids (msg)
  "Return candidate message IDs for collecting MSG's thread via mu.
Starts with MSG's own Message-ID, then falls back to reply ancestry
headers for messages that are visible in mu4e before indexing catches
up with the newest message."
  (delete-dups
   (delq nil
         (append
          (list (let ((msgid (mu4e-message-field msg :message-id)))
                  (unless (string-empty-p msgid) msgid))
                (let ((parent (mu4e-message-field msg :in-reply-to)))
                  (unless (string-empty-p parent) parent)))
          (let ((refs (mu4e-message-field-raw msg :references)))
            (when (listp refs)
              (seq-filter #'identity refs)))))))

(defun jds/ai-email--mu-binary ()
  "Return the path to the mu executable, or nil if not found."
  (or (and (boundp 'mu4e-mu-binary)
           (stringp mu4e-mu-binary)
           (file-executable-p mu4e-mu-binary)
           mu4e-mu-binary)
      (executable-find "mu")))

(defun jds/ai-email--mu-message-plist-p (value)
  "Return non-nil when VALUE looks like a mu message plist."
  (and (listp value)
       (keywordp (car value))
       (plist-member value :message-id)))

(defun jds/ai-email--dedupe-messages-by-id (messages)
  "Return MESSAGES with duplicate Message-IDs removed.
Messages without a Message-ID are kept."
  (let ((seen (make-hash-table :test #'equal))
        (result nil))
    (dolist (message messages)
      (let ((msgid (mu4e-message-field message :message-id)))
        (if (or (not msgid) (string-empty-p msgid))
            (push message result)
          (unless (gethash msgid seen)
            (puthash msgid t seen)
            (push message result)))))
    (nreverse result)))

(defun jds/ai-email--query-thread-plists (mu-bin query-id)
  "Return thread messages from MU-BIN for QUERY-ID, or nil on failure."
  (let ((messages nil))
    (with-temp-buffer
      (let ((exit-code
             (call-process mu-bin nil t nil
                           "find"
                           "--include-related"
                           "--format=sexp"
                           (format "msgid:%s" query-id))))
        (when (= exit-code 0)
          (goto-char (point-min))
          (while (< (point) (point-max))
            (condition-case nil
                (let ((entry (read (current-buffer))))
                  (when (jds/ai-email--mu-message-plist-p entry)
                    (push entry messages)))
              (error (forward-line 1)))))))
    messages))

(defun jds/ai-email--collect-thread-plists (msg)
  "Return a date-sorted list of message plists for MSG's thread.
Uses the mu CLI to query the index.  Returns nil if mu is unavailable
or the thread cannot be found."
  (let* ((mu-bin    (jds/ai-email--mu-binary))
         (query-ids (jds/ai-email--thread-query-ids msg)))
    (unless mu-bin
      (user-error "mu binary not found; cannot collect thread messages"))
    (unless query-ids
      (user-error "Message has no usable Message-ID or References headers"))
    (let* ((messages (seq-some
                      (lambda (query-id)
                        (let ((thread (jds/ai-email--query-thread-plists mu-bin query-id)))
                          (when thread thread)))
                      query-ids))
           (current-id (mu4e-message-field msg :message-id)))
      (when (and msg
                 (or (null messages)
                     (string-empty-p current-id)
                     (not (seq-some
                           (lambda (entry)
                             (string= (mu4e-message-field entry :message-id)
                                      current-id))
                           messages))))
        (push msg messages))
      (setq messages (jds/ai-email--dedupe-messages-by-id messages))
      (sort messages
            (lambda (a b)
              (let ((da (plist-get a :date))
                    (db (plist-get b :date)))
                (< (float-time da) (float-time db))))))))

(defconst jds/ai-email--thread-msg-max-chars 3000
  "Maximum characters to include from each individual message body.")

(defconst jds/ai-email--thread-total-max-chars 24000
  "Maximum total characters for the combined thread text sent to AI.")

(defun jds/ai-email--select-thread-messages-for-prompt (messages)
  "Return the newest MESSAGES that fit the prompt size budget."
  (let ((selected nil)
        (total 0))
    (dolist (msg (reverse messages))
      (let* ((formatted (jds/ai-email--format-one-thread-message msg))
             (separator-cost (if selected 7 0))
             (message-cost (+ separator-cost (length formatted))))
        (when (or (null selected)
                  (<= (+ total message-cost)
                      jds/ai-email--thread-total-max-chars))
          (push msg selected)
          (setq total (+ total message-cost)))))
    selected))

(defun jds/ai-email--get-message-body (msg)
  "Return the plain-text body of MSG.
Tries `mu4e-view-message-text' first; falls back to reading the
raw file past the first blank line."
  (condition-case nil
      (mu4e-view-message-text msg)
    (error
     (let ((path (mu4e-message-field msg :path)))
       (when (and path (file-exists-p path))
         (with-temp-buffer
           (insert-file-contents path)
           (goto-char (point-min))
           (if (search-forward "\n\n" nil t)
               (buffer-substring-no-properties (point) (point-max))
             "")))))))

(defun jds/ai-email--format-one-thread-message (msg)
  "Return a formatted header+body string for one thread MSG."
  (let* ((from-c  (car (mu4e-message-field msg :from)))
         (from    (or (plist-get from-c :name)
                      (plist-get from-c :email)
                      "Unknown"))
         (date    (format-time-string "%Y-%m-%d %H:%M"
                                      (mu4e-message-field msg :date)))
         (subject (or (mu4e-message-field msg :subject) "(no subject)"))
         (msgid   (or (mu4e-message-field msg :message-id) ""))
         (body    (string-trim (or (jds/ai-email--get-message-body msg) "")))
         (body    (if (> (length body) jds/ai-email--thread-msg-max-chars)
                      (concat (substring body 0 jds/ai-email--thread-msg-max-chars)
                              "\n[... truncated ...]")
                    body)))
    (format "From: %s\nDate: %s\nSubject: %s\nMessage-ID: %s\n\n%s"
            from date subject msgid body)))

(defun jds/ai-email--build-thread-prompt (messages)
  "Build the AI prompt from a list of mu4e message plists MESSAGES."
  (let* ((selected (jds/ai-email--select-thread-messages-for-prompt messages))
         (omitted  (- (length messages) (length selected)))
         (formatted (mapcar #'jds/ai-email--format-one-thread-message selected))
         (joined   (string-join formatted "\n\n---\n\n"))
         (prefix   (if (> omitted 0)
                       (format "Older messages omitted due to length: %d\n\n" omitted)
                     "")))
    (format "Summarize this email thread (%d message(s)):\n\n%s%s"
            (length messages) prefix joined)))

(defun jds/ai-email--thread-summary-system-prompt ()
  "Return the system prompt for thread summarization."
  (concat
   "You analyze email threads and extract structured summaries.\n"
   "Return JSON only. No commentary, no markdown code fences.\n"
   "Treat the most recent messages as highest priority.\n"
   "The JSON schema is:\n"
   "{\n"
   "  \"summary\": string,\n"
   "  \"open_questions\": [{\"text\": string, \"msgid\": optional string}],\n"
   "  \"commitments\": [{\"who\": string, \"what\": string, \"msgid\": optional string}],\n"
   "  \"participants\": [string]\n"
   "}\n"
   "summary: 2-4 sentences on the main topic, current state, and resolved decisions.\n"
   "open_questions: only unresolved questions requiring action; include message-id of the message that raised the question when available.\n"
   "commitments: explicit commitments by any party; include message-id where the commitment appears when available.\n"
   "participants: list as \"Name <email>\" for each unique sender.\n"
   "When unsure about a question or commitment, omit it rather than guessing."))

(define-derived-mode jds/ai-email-thread-summary-mode org-mode "AI-Thread"
  "Read-only org buffer for AI-generated email thread summaries."
  (setq-local header-line-format "AI Thread Summary  |  q: close")
  (read-only-mode 1))

(define-derived-mode jds/ai-email-zoom-summary-mode org-mode "AI-Zoom"
  "Read-only org buffer for AI-generated Zoom meeting summaries."
  (setq-local header-line-format "AI Zoom Summary  |  q: close")
  (read-only-mode 1))

(defun jds/ai-email--thread-summary-buffer-name (msg)
  "Return a buffer name for the thread summary of MSG."
  (format "*AI Thread Summary: %s*"
          (truncate-string-to-width
           (or (mu4e-message-field msg :subject) "(no subject)") 50 nil nil t)))

(defun jds/ai-email--zoom-summary-buffer-name (msg)
  "Return a buffer name for the Zoom summary of MSG."
  (format "*AI Zoom Summary: %s*"
          (truncate-string-to-width
           (or (mu4e-message-field msg :subject) "(no subject)") 50 nil nil t)))

(defun jds/ai-email--zoom-summary-system-prompt ()
  "Return the system prompt for compact Zoom meeting-asset summaries."
  (concat
   "You condense Zoom meeting asset emails into very short meeting-note summaries.\n"
   "Return plain Org text only. No markdown fences, no preamble.\n"
   "Ignore email boilerplate, social links, feedback prompts, and repeated section headings.\n"
   "Prioritize substantive project discussion, decisions, unresolved questions, and concrete next steps.\n"
   "Omit incidental personal chatter unless it materially affects follow-up.\n"
   "Write for a 60-second skim before the next meeting.\n"
   "Keep the total output under 160 words.\n"
   "Use this exact structure:\n"
   "* Main points\n"
   "- bullet\n"
   "- bullet\n"
   "* Next steps\n"
   "- bullet\n"
   "- bullet\n"
   "If there are no clear next steps, write '- None noted.' under that heading.\n"
   "Use crisp, specific bullets rather than generic abstractions."))

(defun jds/ai-email--build-zoom-summary-prompt (msg)
  "Build the AI prompt for compacting a Zoom meeting asset MSG."
  (let* ((subject (or (mu4e-message-field msg :subject) "(no subject)"))
         (body (string-trim (or (jds/ai-email--get-message-body msg) ""))))
    (format
     (concat
      "Condense this Zoom meeting asset email into a short summary suitable for meeting notes.\n\n"
      "Subject: %s\n\n"
      "%s")
     subject
     body)))

(defun jds/ai-email--create-zoom-summary-buffer (msg response)
  "Pop an org buffer with compact Zoom summary RESPONSE for MSG."
  (let ((buf (generate-new-buffer (jds/ai-email--zoom-summary-buffer-name msg))))
    (with-current-buffer buf
      (jds/ai-email-zoom-summary-mode)
      (read-only-mode -1)
      (jds/ai-email--reinforce-setup-buffer
       buf
       jds/ai-email-reinforce-summary-database
       (jds/ai-email--reinforce-context-for-message msg "zoom-summary"))
      (let ((start (point)))
        (insert (format "#+TITLE: Zoom Summary: %s\n\n"
                        (or (mu4e-message-field msg :subject) "(no subject)")))
        (insert (string-trim (or response "")))
        (insert "\n")
        (jds/ai-email--reinforce-track-output-region
         buf jds/ai-email-reinforce-zoom-summary-artifact start (point)))
      (goto-char (point-min))
      (read-only-mode 1)
      (evil-local-set-key 'normal (kbd "q") #'bury-buffer))
    (pop-to-buffer buf)))

(defun jds/ai-email--create-thread-summary-buffer (msg parsed message-count)
  "Pop an org buffer with the thread summary for MSG from PARSED JSON."
  (let* ((summary      (alist-get 'summary      parsed))
         (questions    (alist-get 'open_questions parsed))
         (commitments  (alist-get 'commitments   parsed))
         (participants (alist-get 'participants  parsed))
         (subject      (or (mu4e-message-field msg :subject) "(no subject)"))
         (buf          (generate-new-buffer
                        (jds/ai-email--thread-summary-buffer-name msg))))
    (with-current-buffer buf
      (jds/ai-email-thread-summary-mode)
      (read-only-mode -1)
      (jds/ai-email--reinforce-setup-buffer
       buf
       jds/ai-email-reinforce-summary-database
       (jds/ai-email--reinforce-context-for-message msg "thread-summary"))
      (let ((start (point)))
        (insert (format "#+TITLE: Thread Summary: %s\n" subject))
        (insert "#+STARTUP: showall\n")
        (insert (format "\n/%d messages/\n\n" message-count))

        (insert "* Summary\n")
        (insert (or (jds/ai-email--string-or-nil summary) "/No summary returned./") "\n\n")

        (insert "* Open Questions\n")
        (if (null questions)
            (insert "/None identified./\n\n")
          (dolist (q (if (listp questions) questions '()))
            (let ((text  (jds/ai-email--string-or-nil (alist-get 'text  q)))
                  (msgid (jds/ai-email--string-or-nil (alist-get 'msgid q))))
              (when text
                (insert (format "- %s" text))
                (when msgid
                  (insert (format "  [[mu4e:msgid:%s][↗]]" msgid)))
                (insert "\n"))))
          (insert "\n"))

        (insert "* Commitments / Action Items\n")
        (if (null commitments)
            (insert "/None identified./\n\n")
          (dolist (c (if (listp commitments) commitments '()))
            (let ((who   (jds/ai-email--string-or-nil (alist-get 'who   c)))
                  (what  (jds/ai-email--string-or-nil (alist-get 'what  c)))
                  (msgid (jds/ai-email--string-or-nil (alist-get 'msgid c))))
              (when what
                (insert (format "- %s%s"
                                (if who (format "*%s*: " who) "")
                                what))
                (when msgid
                  (insert (format "  [[mu4e:msgid:%s][↗]]" msgid)))
                (insert "\n"))))
          (insert "\n"))

        (insert "* Participants\n")
        (if (null participants)
            (insert "/None identified./\n")
          (dolist (p (if (listp participants) participants '()))
            (when (jds/ai-email--string-or-nil p)
              (insert (format "- %s\n" p)))))
        (jds/ai-email--reinforce-track-output-region
         buf jds/ai-email-reinforce-thread-summary-artifact start (point)))

      (goto-char (point-min))
      (read-only-mode 1)
      (evil-local-set-key 'normal (kbd "q") #'bury-buffer))
    (pop-to-buffer buf)))

(defun jds/mu4e-ai-summarize-thread ()
  "Summarize the email thread at point using AI.
Collects all messages in the thread via the mu index, sends them to
the AI, and displays a structured org-mode summary buffer."
  (interactive)
  (let* ((msg (mu4e-message-at-point t)))
    (unless msg
      (user-error "No message at point"))
    (message "Collecting thread messages...")
    (let* ((messages (jds/ai-email--collect-thread-plists msg))
           (count    (length messages)))
      (if (zerop count)
          (message "No messages found for this thread (is mu indexed?)")
        (message "Summarizing thread (%d message(s)) with AI..." count)
        (let* ((prompt (jds/ai-email--build-thread-prompt messages))
               (system (jds/ai-email--thread-summary-system-prompt))
               (origin (current-buffer)))
          (let ((gptel-include-reasoning nil))
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
                     (jds/ai-email--create-thread-summary-buffer
                      msg
                      (jds/ai-email--parse-json-response response)
                      count)
                   (error
                    (message "Thread summary parse error: %s"
                             (error-message-string err)))))))))))))

(defun jds/mu4e-ai-compact-zoom-meeting-assets ()
  "Compact the Zoom meeting asset email at point into short meeting notes."
  (interactive)
  (let* ((msg (mu4e-message-at-point t)))
    (unless msg
      (user-error "No message at point"))
    (message "Summarizing Zoom meeting assets...")
    (let* ((prompt (jds/ai-email--build-zoom-summary-prompt msg))
           (system (jds/ai-email--zoom-summary-system-prompt))
           (origin (current-buffer)))
      (let ((gptel-include-reasoning nil))
        (gptel-request
         prompt
         :system system
         :stream nil
         :buffer origin
         :callback
         (lambda (response info)
           (if (not response)
               (message "gptel error: %s" (plist-get info :status))
             (jds/ai-email--create-zoom-summary-buffer
              msg
              (jds/ai-email--sanitize-response response)))))))))


;;; Keybindings ------------------------------------------------------------

;; ca = compose with AI draft
;; cA = compose with AI scheduling assistant
;; cS = summarize thread
(evil-collection-define-key 'normal 'mu4e-headers-mode-map
  "ca" #'jds/mu4e-ai-draft-reply
  "cA" #'jds/mu4e-ai-scheduling-reply
  "cl" #'jds/mu4e-ai-extract-captures
  "cS" #'jds/mu4e-ai-summarize-thread)

(evil-collection-define-key 'normal 'mu4e-view-mode-map
  "ca" #'jds/mu4e-ai-draft-reply
  "cA" #'jds/mu4e-ai-scheduling-reply
  "cl" #'jds/mu4e-ai-extract-captures
  "cS" #'jds/mu4e-ai-summarize-thread)
