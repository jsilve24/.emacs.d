;;; firefiles.el --- firefiles api  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Justin Silverman

;; Author: Justin Silverman <jsilve24@gmail.com>
;; Keywords: AI


(require 'auth-source)
(require 'json)
(require 'url)
(require 'subr-x)
(require 'cl-lib)

(declare-function gptel-request "gptel" (prompt &rest args))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-outline-level "org" ())

(defgroup fireflies nil
  "Fireflies.ai helpers."
  :group 'applications)

(defcustom fireflies-api-url "https://api.fireflies.ai/graphql"
  "Fireflies GraphQL endpoint."
  :type 'string
  :group 'fireflies)

(defcustom fireflies-auth-host "api.fireflies.ai"
  "Host used to look up Fireflies credentials in auth-source."
  :type 'string
  :group 'fireflies)

(defcustom fireflies-auth-user "apikey"
  "User/login used to look up Fireflies credentials in auth-source."
  :type 'string
  :group 'fireflies)

(defcustom fireflies-default-list-limit 25
  "How many recent meetings to offer in completion."
  :type 'integer
  :group 'fireflies)

(defcustom fireflies-ai-source-preference 'both
  "Default Fireflies source used for AI summarization."
  :type '(choice (const :tag "Summary only" summary)
                 (const :tag "Transcript only" transcript)
                 (const :tag "Both when possible" both))
  :group 'fireflies)

(defcustom fireflies-ai-system-prompt
  "You convert research meeting material into precise Org-mode notes."
  "System prompt used for Fireflies meeting summarization requests."
  :type 'string
  :group 'fireflies)

(defcustom fireflies-ai-summary-prompt
  (string-join
   '("You are preparing concise Org-mode meeting notes from a research meeting involving Dr. Silverman."
     ""
     "Use the source material below to produce Org-mode output only."
     "Do not include any preamble, explanation, code fences, or Markdown."
     ""
     "Requirements:"
     "- Be concise."
     "- Do not include a Participants section."
     "- Do not repeat the same information across sections."
     "- Focus on what will matter for the next meeting."
     "- If a detail is uncertain, state it briefly without guessing."
     "- Omit any section that has no meaningful content, except Overview, Developments, and Action Items, which should always be present."
     ""
     "Output format:"
     "* Overview"
     "Write 2-4 sentences summarizing the meeting at a high level."
     ""
     "* Developments"
     "Bullet points only."
     "Include decisions, clarified plans, constraints, important context, and key details worth remembering for the next meeting."
     "Do not include action items here."
     ""
     "* Action Items"
     "Group tasks by person when possible."
     "Use Org subheadings only if there are assigned tasks."
     "If ownership is unclear, use an \"Unassigned\" subsection."
     ""
     "* Project Ideas"
     "Include this section only if there were notable future ideas worth saving but not acting on now."
     ""
     "Style rules:"
     "- Keep bullets short and information-dense."
     "- Do not create a Main Topics section."
     "- Do not create separate Decisions or Open Questions sections unless that information naturally belongs under Developments."
     "- Avoid repeating the overview in bullet form."
     "- Return only Org-mode text with `*` headings.")
   "\n")
  "User prompt template used for Fireflies meeting summarization."
  :type 'string
  :group 'fireflies)

(defvar-local fireflies-last-source-text nil
  "Last raw Fireflies source material used for AI summarization in this buffer.")

(defvar-local fireflies-last-source-kind nil
  "Source kind used for the last Fireflies AI summarization in this buffer.")

(defvar-local fireflies-last-meeting-id nil
  "Fireflies meeting ID used for the last AI summarization in this buffer.")

(defvar-local fireflies-last-meeting-title nil
  "Fireflies meeting title used for the last AI summarization in this buffer.")

(defvar-local fireflies-last-meeting-date nil
  "Fireflies meeting date used for the last AI summarization in this buffer.")

(defvar-local fireflies-last-ai-prompt nil
  "Prompt used for the last Fireflies AI summarization in this buffer.")

(defun fireflies--api-key ()
  "Return Fireflies API key from auth-source."
  (let* ((match (car (auth-source-search
                      :host fireflies-auth-host
                      :user fireflies-auth-user
                      :max 1
                      :require '(:secret))))
         (secret (plist-get match :secret)))
    (unless secret
      (user-error "No Fireflies credential found for host=%s user=%s"
                  fireflies-auth-host fireflies-auth-user))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun fireflies--graphql (query &optional variables)
  "Send GraphQL QUERY with VARIABLES to Fireflies and return the data alist."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " (fireflies--api-key)))))
         (url-request-data
          (encode-coding-string
           (json-encode
            `(("query" . ,query)
              ("variables" . ,(or variables (make-hash-table)))))
           'utf-8))
         (buf (url-retrieve-synchronously fireflies-api-url t t 30)))
    (unless buf
      (error "Fireflies request failed: no response"))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (unless (search-forward "\n\n" nil t)
            (error "Malformed HTTP response from Fireflies"))
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'symbol)
                 (payload (json-parse-buffer
                           :object-type 'alist
                           :array-type 'list
                           :null-object nil
                           :false-object nil))
                 (errors (alist-get 'errors payload))
                 (data (alist-get 'data payload)))
            (when errors
              (error "Fireflies GraphQL error: %s"
                     (mapconcat
                      (lambda (e)
                        (or (alist-get 'message e)
                            (format "%S" e)))
                      errors
                      "; ")))
            data))
      (kill-buffer buf))))

(defun fireflies--graphql-maybe (query &optional variables)
  "Return Fireflies GraphQL response for QUERY or nil on error."
  (condition-case nil
      (fireflies--graphql query variables)
    (error nil)))

(defun fireflies--format-date (x)
  "Format Fireflies epoch-ish date X into YYYY-MM-DD HH:MM.
Handles seconds or milliseconds."
  (cond
   ((numberp x)
    (let* ((secs (if (> x 1000000000000) (/ x 1000.0) x)))
      (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time secs))))
   ((stringp x) x)
   (t "unknown-date")))

(defun fireflies--short-id (id)
  "Return a short printable version of transcript ID."
  (if (and (stringp id) (> (length id) 8))
      (substring id (- (length id) 8))
    (or id "")))

(defun fireflies--one-line (s)
  "Collapse S to a single trimmed line."
  (when (stringp s)
    (string-trim
     (replace-regexp-in-string "[[:space:]\n\r]+" " " s))))

(defun fireflies--textify (x &optional bulletize)
  "Convert X into a printable string.
If BULLETIZE is non-nil and X is a list, format as bullet points."
  (cond
   ((null x) "")
   ((stringp x) (string-trim x))
   ((numberp x) (number-to-string x))
   ((vectorp x)
    (fireflies--textify (append x nil) bulletize))
   ((listp x)
    (let* ((items
            (delq nil
                  (mapcar
                   (lambda (elt)
                     (let ((s (fireflies--textify elt nil)))
                       (unless (string-empty-p s) s)))
                   x))))
      (if bulletize
          (mapconcat (lambda (s) (concat "- " s)) items "\n")
        (mapconcat #'identity items ", "))))
   (t (format "%s" x))))

(defun fireflies--best-preview (summary)
  "Return a short preview string from SUMMARY."
  (let ((text (or (alist-get 'short_summary summary)
                  (alist-get 'short_overview summary)
                  (alist-get 'overview summary)
                  (alist-get 'gist summary)
                  (alist-get 'bullet_gist summary)
                  "")))
    (truncate-string-to-width
     (or (fireflies--one-line (fireflies--textify text))
         "")
     90 nil nil t)))

(defun fireflies--meeting-choice-label (meeting)
  "Return completion label for MEETING."
  (let* ((id (alist-get 'id meeting))
         (title (or (alist-get 'title meeting) "Untitled"))
         (date (fireflies--format-date (alist-get 'date meeting)))
         (preview (fireflies--best-preview (alist-get 'summary meeting))))
    (format "%s | %s | %s | %s"
            date title (fireflies--short-id id) preview)))

(defun fireflies--choose-meeting (&optional limit)
  "Prompt for a meeting from the recent Fireflies list."
  (let ((meetings (fireflies--list-transcripts
                   (or limit fireflies-default-list-limit))))
    (unless meetings
      (user-error "No Fireflies meetings found"))
    (let* ((choices
            (mapcar (lambda (m)
                      (cons (fireflies--meeting-choice-label m) m))
                    meetings))
           (choice (completing-read "Meeting: " (mapcar #'car choices) nil t)))
      (cdr (assoc choice choices)))))

(defun fireflies--list-transcripts (&optional limit)
  "Fetch recent transcripts."
  (let* ((query
          "query ListTranscripts($limit: Int!, $skip: Int!) {
             transcripts(limit: $limit, skip: $skip) {
               id
               title
               date
               summary {
                 short_summary
                 short_overview
                 overview
                 gist
                 bullet_gist
               }
             }
           }")
         (data (fireflies--graphql
                query
                `(("limit" . ,(or limit fireflies-default-list-limit))
                  ("skip" . 0))))
         (items (alist-get 'transcripts data)))
    (sort (copy-sequence items)
          (lambda (a b)
            (> (or (alist-get 'date a) 0)
               (or (alist-get 'date b) 0))))))

(defun fireflies--get-transcript-summary (id)
  "Fetch detailed summary fields for transcript ID."
  (let* ((query
          "query GetTranscript($id: String!) {
             transcript(id: $id) {
               id
               title
               date
               summary {
                 short_summary
                 short_overview
                 overview
                 gist
                 bullet_gist
                 notes
                 action_items
                 keywords
                 meeting_type
               }
             }
           }")
         (data (fireflies--graphql query `(("id" . ,id)))))
    (alist-get 'transcript data)))

(defun fireflies--summary-source-text (transcript)
  "Render TRANSCRIPT summary fields as plain source text."
  (let* ((summary (alist-get 'summary transcript))
         (parts
          (delq nil
                (list
                 (let ((s (fireflies--textify (alist-get 'short_summary summary))))
                   (unless (string-empty-p s)
                     (format "Short summary:\n%s" s)))
                 (let ((s (fireflies--textify (alist-get 'overview summary))))
                   (unless (string-empty-p s)
                     (format "Overview:\n%s" s)))
                 (let ((s (fireflies--textify (alist-get 'notes summary))))
                   (unless (string-empty-p s)
                     (format "Notes:\n%s" s)))
                 (let ((s (fireflies--textify (alist-get 'bullet_gist summary) t)))
                   (unless (string-empty-p s)
                     (format "Bullet gist:\n%s" s)))
                 (let ((s (fireflies--textify (alist-get 'action_items summary) t)))
                   (unless (string-empty-p s)
                     (format "Action items:\n%s" s)))
                 (let ((s (fireflies--textify (alist-get 'keywords summary) t)))
                   (unless (string-empty-p s)
                     (format "Keywords:\n%s" s))))))
         (text (string-join parts "\n\n")))
    (string-trim text)))

(defun fireflies--render-sentences (sentences)
  "Render Fireflies SENTENCES to text."
  (string-trim
   (mapconcat
    (lambda (sentence)
      (let* ((speaker (or (alist-get 'speaker_name sentence)
                          (alist-get 'speaker sentence)
                          "Unknown"))
             (text (or (alist-get 'text sentence)
                       (alist-get 'sentence sentence)
                       "")))
        (string-trim (format "%s: %s" speaker text))))
    sentences
    "\n")))

(defun fireflies--get-transcript-text (id)
  "Fetch transcript text for Fireflies transcript ID.
Returns nil if transcript text fields are unavailable."
  (let* ((variables `(("id" . ,id)))
         (queries
          (list
           "query GetTranscriptSentences($id: String!) {
              transcript(id: $id) {
                sentences {
                  speaker_name
                  text
                }
              }
            }"
           "query GetTranscriptSentencesAlt($id: String!) {
              transcript(id: $id) {
                sentences {
                  speaker_name
                  sentence
                }
              }
            }"
           "query GetTranscriptText($id: String!) {
              transcript(id: $id) {
                transcript_text
              }
            }"
           "query GetTranscriptRawText($id: String!) {
              transcript(id: $id) {
                raw_text
              }
            }")))
    (cl-loop for query in queries
             for data = (fireflies--graphql-maybe query variables)
             when data
             for transcript = (alist-get 'transcript data)
             for sentences = (alist-get 'sentences transcript)
             for text = (or (and (listp sentences)
                                 sentences
                                 (fireflies--render-sentences sentences))
                            (alist-get 'transcript_text transcript)
                            (alist-get 'raw_text transcript))
             when (and (stringp text)
                       (not (string-empty-p (string-trim text))))
             return (string-trim text))))

(defun fireflies--render-summary (transcript)
  "Render TRANSCRIPT summary as Markdown-ish text."
  (let* ((title (or (alist-get 'title transcript) "Untitled meeting"))
         (date  (fireflies--format-date (alist-get 'date transcript)))
         (summary (alist-get 'summary transcript))
         (main (or (alist-get 'short_summary summary)
                   (alist-get 'overview summary)
                   (alist-get 'short_overview summary)
                   (alist-get 'gist summary)
                   (alist-get 'bullet_gist summary)
                   (alist-get 'notes summary)
                   ""))
         (actions (alist-get 'action_items summary))
         (keywords (alist-get 'keywords summary))
         (meeting-type (alist-get 'meeting_type summary))
         (parts
          (delq nil
                (list
                 (format "# %s" title)
                 (format "_%s_" date)
                 (let ((s (fireflies--textify main)))
                   (unless (string-empty-p s) s))
                 (let ((s (fireflies--textify meeting-type)))
                   (unless (string-empty-p s)
                     (format "## Meeting type\n\n%s" s)))
                 (let ((s (fireflies--textify actions t)))
                   (unless (string-empty-p s)
                     (format "## Action items\n\n%s" s)))
                 (let ((s (fireflies--textify keywords t)))
                   (unless (string-empty-p s)
                     (format "## Keywords\n\n%s" s)))))))
    (string-join parts "\n\n")))

(defun fireflies--read-source-kind ()
  "Prompt for a Fireflies AI source kind."
  (intern
   (completing-read
    "AI source: "
    '("both" "summary" "transcript")
    nil t nil nil
    (symbol-name fireflies-ai-source-preference))))

(defun fireflies--build-ai-source (transcript source-kind)
  "Return source text and actual source kind for TRANSCRIPT and SOURCE-KIND."
  (let* ((id (alist-get 'id transcript))
         (summary-text (fireflies--summary-source-text transcript))
         (transcript-text (fireflies--get-transcript-text id)))
    (pcase source-kind
      ('summary
       (unless (string-empty-p (or summary-text ""))
         (cons summary-text 'summary)))
      ('transcript
       (unless (string-empty-p (or transcript-text ""))
         (cons transcript-text 'transcript)))
      (_
       (cond
        ((and transcript-text (not (string-empty-p transcript-text))
              (not (string-empty-p (or summary-text ""))))
         (cons (format "Fireflies summary:\n%s\n\nTranscript:\n%s"
                       summary-text transcript-text)
               'both))
        ((and transcript-text (not (string-empty-p transcript-text)))
         (cons transcript-text 'transcript))
        ((not (string-empty-p (or summary-text "")))
         (cons summary-text 'summary)))))))

(defun fireflies--build-ai-prompt (transcript source-text source-kind &optional prompt)
  "Build the full user prompt for TRANSCRIPT using SOURCE-TEXT and SOURCE-KIND."
  (format
   "%s\n\nMeeting metadata:\nTitle: %s\nDate: %s\nSource kind: %s\n\nSource material:\n---\n%s\n---"
   (or prompt fireflies-ai-summary-prompt)
   (or (alist-get 'title transcript) "Untitled meeting")
   (fireflies--format-date (alist-get 'date transcript))
   source-kind
   source-text))

(defun fireflies--sanitize-ai-response (response)
  "Normalize model RESPONSE before insertion."
  (let ((text (string-trim (or response ""))))
    (setq text
          (replace-regexp-in-string
           "\\`\\(?:```[[:alpha:]-]*[[:space:]\n]*\\)"
           ""
           text))
    (setq text
          (replace-regexp-in-string
           "[[:space:]\n]*```\\'"
           ""
           text))
    (string-trim text)))

(defun fireflies--org-insertion-base-level ()
  "Return top-level Org heading level for insertion at point."
  (if (derived-mode-p 'org-mode)
      (save-excursion
        (condition-case nil
            (progn
              (org-back-to-heading t)
              (1+ (org-outline-level)))
          (error 1)))
    1))

(defun fireflies--relevel-org-text (text base-level)
  "Adjust Org heading levels in TEXT so top-level is BASE-LEVEL."
  (let ((offset (max 0 (1- base-level))))
    (replace-regexp-in-string
     "^\\(\\*+\\) "
     (lambda (match)
       (concat (make-string (+ offset (length (match-string 1 match))) ?*)
               " "))
     text
     t t)))

(defun fireflies--store-last-ai-context (transcript source-text source-kind prompt)
  "Store last Fireflies AI context for redo in the current buffer."
  (setq-local fireflies-last-source-text source-text
              fireflies-last-source-kind source-kind
              fireflies-last-meeting-id (alist-get 'id transcript)
              fireflies-last-meeting-title (alist-get 'title transcript)
              fireflies-last-meeting-date (alist-get 'date transcript)
              fireflies-last-ai-prompt prompt))

(defun fireflies--insert-ai-summary-response
    (buffer marker transcript source-text source-kind prompt response)
  "Insert AI RESPONSE in BUFFER at MARKER and persist local Fireflies state."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char marker)
        (let* ((clean (fireflies--sanitize-ai-response response))
               (text (if (derived-mode-p 'org-mode)
                         (fireflies--relevel-org-text
                          clean
                          (fireflies--org-insertion-base-level))
                       clean)))
          (unless (bolp)
            (insert "\n"))
          (let ((start (point)))
            (insert text)
            (unless (bolp)
              (insert "\n"))
            (fireflies--store-last-ai-context
             transcript source-text source-kind prompt)
            (message "Inserted Fireflies AI summary for: %s"
                     (or (alist-get 'title transcript)
                         (alist-get 'id transcript)))
            start))))))

(defun fireflies--request-ai-summary
    (buffer marker transcript source-text source-kind &optional prompt)
  "Request AI summary for TRANSCRIPT using SOURCE-TEXT and insert into BUFFER."
  (unless (require 'gptel nil t)
    (user-error "gptel is not available"))
  (let ((full-prompt
         (fireflies--build-ai-prompt transcript source-text source-kind prompt)))
    (gptel-request
     full-prompt
     :system fireflies-ai-system-prompt
     :stream nil
     :callback
     (lambda (response info)
       (cond
        ((not response)
         (message "Fireflies AI summary failed: %s" (plist-get info :status)))
        ((stringp response)
         (fireflies--insert-ai-summary-response
          buffer marker transcript source-text source-kind
          (or prompt fireflies-ai-summary-prompt)
          response))
        (t
         (message "Unhandled Fireflies AI response: %S" response)))))))

;;;###autoload
(defun fireflies-insert-meeting-summary (&optional limit)
  "Choose a Fireflies meeting via `completing-read` and insert its summary at point.

With prefix arg, prompt for the number of meetings to fetch."
  (interactive
   (list (when current-prefix-arg
           (read-number "How many recent meetings? "
                        fireflies-default-list-limit))))
  (let* ((target-buffer (current-buffer))
         (target-point (point-marker))
         (meeting (fireflies--choose-meeting
                   (or limit fireflies-default-list-limit)))
         (id (alist-get 'id meeting))
         (transcript (fireflies--get-transcript-summary id))
         (text (concat (fireflies--render-summary transcript) "\n")))
    (with-current-buffer target-buffer
      (save-excursion
        (goto-char target-point)
        (insert text)))
    (message "Inserted Fireflies summary for: %s"
             (or (alist-get 'title transcript) id))))

;;;###autoload
(defun fireflies-insert-meeting-ai-summary (&optional limit edit-prompt)
  "Fetch a Fireflies meeting, summarize it with gptel, and insert Org text.

With prefix arg, prompt for LIMIT and allow editing the default AI prompt."
  (interactive
   (if current-prefix-arg
       (list
        (read-number "How many recent meetings? " fireflies-default-list-limit)
        (read-from-minibuffer "AI prompt: " fireflies-ai-summary-prompt))
     (list nil nil)))
  (let* ((target-buffer (current-buffer))
         (target-point (point-marker))
         (meeting (fireflies--choose-meeting
                   (or limit fireflies-default-list-limit)))
         (transcript (fireflies--get-transcript-summary (alist-get 'id meeting)))
         (requested-kind (fireflies--read-source-kind))
         (source (fireflies--build-ai-source transcript requested-kind)))
    (unless source
      (user-error "Could not retrieve usable Fireflies source text for %s"
                  (or (alist-get 'title transcript)
                      (alist-get 'id transcript))))
    (message "Requesting Fireflies AI summary for %s..."
             (or (alist-get 'title transcript)
                 (alist-get 'id transcript)))
    (fireflies--request-ai-summary
     target-buffer target-point transcript
     (car source) (cdr source) edit-prompt)))

;;;###autoload
(defun fireflies-insert-meeting-source (&optional limit)
  "Insert raw Fireflies source material at point."
  (interactive
   (list (when current-prefix-arg
           (read-number "How many recent meetings? "
                        fireflies-default-list-limit))))
  (let* ((meeting (fireflies--choose-meeting
                   (or limit fireflies-default-list-limit)))
         (transcript (fireflies--get-transcript-summary (alist-get 'id meeting)))
         (requested-kind (fireflies--read-source-kind))
         (source (fireflies--build-ai-source transcript requested-kind)))
    (unless source
      (user-error "Could not retrieve usable Fireflies source text"))
    (insert (car source))
    (unless (bolp)
      (insert "\n"))
    (fireflies--store-last-ai-context
     transcript (car source) (cdr source) fireflies-ai-summary-prompt)
    (message "Inserted Fireflies source for: %s"
             (or (alist-get 'title transcript)
                 (alist-get 'id transcript)))))

;;;###autoload
(defun fireflies-redo-last-ai-summary (&optional edit-prompt)
  "Reuse the last stored Fireflies source in this buffer and rerun AI summary.

With prefix arg, prompt to edit the AI prompt before rerunning."
  (interactive
   (list (when current-prefix-arg
           (read-from-minibuffer
            "AI prompt: "
            (or fireflies-last-ai-prompt fireflies-ai-summary-prompt)))))
  (unless fireflies-last-source-text
    (user-error "No stored Fireflies source found in this buffer"))
  (let ((transcript
         `((id . ,fireflies-last-meeting-id)
           (title . ,fireflies-last-meeting-title)
           (date . ,fireflies-last-meeting-date))))
    (message "Rerunning Fireflies AI summary for %s..."
             (or fireflies-last-meeting-title fireflies-last-meeting-id))
    (fireflies--request-ai-summary
     (current-buffer)
     (point-marker)
     transcript
     fireflies-last-source-text
     fireflies-last-source-kind
     edit-prompt)))

(provide 'fireflies)




;;; Insert into org functionality ----------------------------------------------
