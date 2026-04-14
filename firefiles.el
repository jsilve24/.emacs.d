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
(declare-function jds/localleader-def "core" (&rest args))

(defgroup firefiles nil
  "Firefiles helpers for Fireflies.ai."
  :group 'applications)

(defcustom firefiles-api-url "https://api.fireflies.ai/graphql"
  "Fireflies GraphQL endpoint."
  :type 'string
  :group 'firefiles)

(defcustom firefiles-auth-host "api.fireflies.ai"
  "Host used to look up Fireflies credentials in auth-source."
  :type 'string
  :group 'firefiles)

(defcustom firefiles-auth-user "apikey"
  "User/login used to look up Fireflies credentials in auth-source."
  :type 'string
  :group 'firefiles)

(defcustom firefiles-default-list-limit 25
  "How many recent meetings to offer in completion."
  :type 'integer
  :group 'firefiles)

(defcustom firefiles-ai-source-preference 'summary
  "Default Fireflies source used for AI summarization."
  :type '(choice (const :tag "Summary only" summary)
                 (const :tag "Transcript only" transcript)
                 (const :tag "Both when possible" both))
  :group 'firefiles)

(defcustom firefiles-localleader-key-prefix "m"
  "Org localleader prefix used for Firefiles commands."
  :type 'string
  :group 'firefiles)

(defcustom firefiles-ai-system-prompt
  "You convert research meeting material into precise Org-mode notes."
  "System prompt used for Fireflies meeting summarization requests."
  :type 'string
  :group 'firefiles)

(defcustom firefiles-ai-summary-prompt
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
  :group 'firefiles)

(defvar-local firefiles-last-source-text nil
  "Last raw Fireflies source material used for AI summarization in this buffer.")

(defvar-local firefiles-last-source-kind nil
  "Source kind used for the last Firefiles AI summarization in this buffer.")

(defvar-local firefiles-last-meeting-id nil
  "Fireflies meeting ID used for the last AI summarization in this buffer.")

(defvar-local firefiles-last-meeting-title nil
  "Fireflies meeting title used for the last AI summarization in this buffer.")

(defvar-local firefiles-last-meeting-date nil
  "Fireflies meeting date used for the last AI summarization in this buffer.")

(defvar-local firefiles-last-ai-prompt nil
  "Prompt used for the last Fireflies AI summarization in this buffer.")

(defun firefiles--api-key ()
  "Return Fireflies API key from auth-source."
  (let* ((match (car (auth-source-search
                      :host firefiles-auth-host
                      :user firefiles-auth-user
                      :max 1
                      :require '(:secret))))
         (secret (plist-get match :secret)))
    (unless secret
      (user-error "No Fireflies credential found for host=%s user=%s"
                  firefiles-auth-host firefiles-auth-user))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun firefiles--graphql (query &optional variables)
  "Send GraphQL QUERY with VARIABLES to Fireflies and return the data alist."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " (firefiles--api-key)))))
         (url-request-data
          (encode-coding-string
           (json-encode
            `(("query" . ,query)
              ("variables" . ,(or variables (make-hash-table)))))
           'utf-8))
         (buf (url-retrieve-synchronously firefiles-api-url t t 30)))
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

(defun firefiles--graphql-maybe (query &optional variables)
  "Return Fireflies GraphQL response for QUERY or nil on error."
  (condition-case nil
      (firefiles--graphql query variables)
    (error nil)))

(defun firefiles--format-date (x)
  "Format Fireflies epoch-ish date X into YYYY-MM-DD HH:MM.
Handles seconds or milliseconds."
  (cond
   ((numberp x)
    (let* ((secs (if (> x 1000000000000) (/ x 1000.0) x)))
      (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time secs))))
   ((stringp x) x)
   (t "unknown-date")))

(defun firefiles--short-id (id)
  "Return a short printable version of transcript ID."
  (if (and (stringp id) (> (length id) 8))
      (substring id (- (length id) 8))
    (or id "")))

(defun firefiles--one-line (s)
  "Collapse S to a single trimmed line."
  (when (stringp s)
    (string-trim
     (replace-regexp-in-string "[[:space:]\n\r]+" " " s))))

(defun firefiles--textify (x &optional bulletize)
  "Convert X into a printable string.
If BULLETIZE is non-nil and X is a list, format as bullet points."
  (cond
   ((null x) "")
   ((stringp x) (string-trim x))
   ((numberp x) (number-to-string x))
   ((vectorp x)
    (firefiles--textify (append x nil) bulletize))
   ((listp x)
    (let* ((items
            (delq nil
                  (mapcar
                   (lambda (elt)
                     (let ((s (firefiles--textify elt nil)))
                       (unless (string-empty-p s) s)))
                   x))))
      (if bulletize
          (mapconcat (lambda (s) (concat "- " s)) items "\n")
        (mapconcat #'identity items ", "))))
   (t (format "%s" x))))

(defun firefiles--best-preview (summary)
  "Return a short preview string from SUMMARY."
  (let ((text (or (alist-get 'short_summary summary)
                  (alist-get 'short_overview summary)
                  (alist-get 'overview summary)
                  (alist-get 'gist summary)
                  (alist-get 'bullet_gist summary)
                  "")))
    (truncate-string-to-width
     (or (firefiles--one-line (firefiles--textify text))
         "")
     90 nil nil t)))

(defun firefiles--meeting-choice-label (meeting)
  "Return completion label for MEETING."
  (let* ((id (alist-get 'id meeting))
         (title (or (alist-get 'title meeting) "Untitled"))
         (date (firefiles--format-date (alist-get 'date meeting)))
         (preview (firefiles--best-preview (alist-get 'summary meeting))))
    (format "%s | %s | %s | %s"
            date title (firefiles--short-id id) preview)))

(defun firefiles--choose-meeting (&optional limit)
  "Prompt for a meeting from the recent Fireflies list."
  (let ((meetings (firefiles--list-transcripts
                   (or limit firefiles-default-list-limit))))
    (unless meetings
      (user-error "No Fireflies meetings found"))
    (let* ((choices
            (mapcar (lambda (m)
                      (cons (firefiles--meeting-choice-label m) m))
                    meetings))
           (choice (completing-read "Meeting: " (mapcar #'car choices) nil t)))
      (cdr (assoc choice choices)))))

(defun firefiles--list-transcripts (&optional limit)
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
         (data (firefiles--graphql
                query
                `(("limit" . ,(or limit firefiles-default-list-limit))
                  ("skip" . 0))))
         (items (alist-get 'transcripts data)))
    (sort (copy-sequence items)
          (lambda (a b)
            (> (or (alist-get 'date a) 0)
               (or (alist-get 'date b) 0))))))

(defun firefiles--get-transcript-summary (id)
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
         (data (firefiles--graphql query `(("id" . ,id)))))
    (alist-get 'transcript data)))

(defun firefiles--summary-source-text (transcript)
  "Render TRANSCRIPT summary fields as plain source text."
  (let* ((summary (alist-get 'summary transcript))
         (parts
          (delq nil
                (list
                 (let ((s (firefiles--textify (alist-get 'short_summary summary))))
                   (unless (string-empty-p s)
                     (format "Short summary:\n%s" s)))
                 (let ((s (firefiles--textify (alist-get 'overview summary))))
                   (unless (string-empty-p s)
                     (format "Overview:\n%s" s)))
                 (let ((s (firefiles--textify (alist-get 'notes summary))))
                   (unless (string-empty-p s)
                     (format "Notes:\n%s" s)))
                 (let ((s (firefiles--textify (alist-get 'bullet_gist summary) t)))
                   (unless (string-empty-p s)
                     (format "Bullet gist:\n%s" s)))
                 (let ((s (firefiles--textify (alist-get 'action_items summary) t)))
                   (unless (string-empty-p s)
                     (format "Action items:\n%s" s)))
                 (let ((s (firefiles--textify (alist-get 'keywords summary) t)))
                   (unless (string-empty-p s)
                     (format "Keywords:\n%s" s))))))
         (text (string-join parts "\n\n")))
    (string-trim text)))

(defun firefiles--render-sentences (sentences)
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

(defun firefiles--get-transcript-text (id)
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
             for data = (firefiles--graphql-maybe query variables)
             when data
             for transcript = (alist-get 'transcript data)
             for sentences = (alist-get 'sentences transcript)
             for text = (or (and (listp sentences)
                                 sentences
                                 (firefiles--render-sentences sentences))
                            (alist-get 'transcript_text transcript)
                            (alist-get 'raw_text transcript))
             when (and (stringp text)
                       (not (string-empty-p (string-trim text))))
             return (string-trim text))))

(defun firefiles--render-summary (transcript)
  "Render TRANSCRIPT summary as Markdown-ish text."
  (let* ((title (or (alist-get 'title transcript) "Untitled meeting"))
         (date  (firefiles--format-date (alist-get 'date transcript)))
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
                 (let ((s (firefiles--textify main)))
                   (unless (string-empty-p s) s))
                 (let ((s (firefiles--textify meeting-type)))
                   (unless (string-empty-p s)
                     (format "## Meeting type\n\n%s" s)))
                 (let ((s (firefiles--textify actions t)))
                   (unless (string-empty-p s)
                     (format "## Action items\n\n%s" s)))
                 (let ((s (firefiles--textify keywords t)))
                   (unless (string-empty-p s)
                     (format "## Keywords\n\n%s" s)))))))
    (string-join parts "\n\n")))

(defun firefiles--read-source-kind ()
  "Prompt for a Fireflies AI source kind."
  (intern
   (completing-read
    "AI source: "
    '("both" "summary" "transcript")
    nil t nil nil
    (symbol-name firefiles-ai-source-preference))))

(defun firefiles--read-custom-ai-prompt-addition ()
  "Prompt for an optional extra instruction to append to the AI prompt."
  (let ((addition (read-from-minibuffer "Add to AI prompt: ")))
    (unless (string-empty-p (string-trim addition))
      (string-trim addition))))

(defun firefiles--compose-ai-prompt (&optional prompt-addition)
  "Return the Fireflies AI prompt with optional PROMPT-ADDITION appended."
  (if (and prompt-addition
           (not (string-empty-p (string-trim prompt-addition))))
      (concat firefiles-ai-summary-prompt
              "\n\nAdditional instruction:\n"
              (string-trim prompt-addition))
    firefiles-ai-summary-prompt))

(defun firefiles--compose-prompt-from-base (base-prompt &optional prompt-addition)
  "Return BASE-PROMPT with optional PROMPT-ADDITION appended."
  (if (and prompt-addition
           (not (string-empty-p (string-trim prompt-addition))))
      (concat base-prompt
              "\n\nAdditional instruction:\n"
              (string-trim prompt-addition))
    base-prompt))

(defun firefiles--build-ai-source (transcript source-kind)
  "Return source text and actual source kind for TRANSCRIPT and SOURCE-KIND."
  (let* ((id (alist-get 'id transcript))
         (summary-text (firefiles--summary-source-text transcript))
         (transcript-text (firefiles--get-transcript-text id)))
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

(defun firefiles--build-ai-prompt (transcript source-text source-kind &optional prompt)
  "Build the full user prompt for TRANSCRIPT using SOURCE-TEXT and SOURCE-KIND."
  (format
   "%s\n\nMeeting metadata:\nTitle: %s\nDate: %s\nSource kind: %s\n\nSource material:\n---\n%s\n---"
   (or prompt firefiles-ai-summary-prompt)
   (or (alist-get 'title transcript) "Untitled meeting")
   (firefiles--format-date (alist-get 'date transcript))
   source-kind
   source-text))

(defun firefiles--sanitize-ai-response (response)
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

(defun firefiles--org-insertion-base-level ()
  "Return top-level Org heading level for insertion at point."
  (if (derived-mode-p 'org-mode)
      (save-excursion
        (condition-case nil
            (progn
              (org-back-to-heading t)
              (1+ (org-outline-level)))
          (error 1)))
    1))

(defun firefiles--relevel-org-text (text base-level)
  "Adjust Org heading levels in TEXT so top-level is BASE-LEVEL."
  (let ((offset (max 0 (1- base-level))))
    (replace-regexp-in-string
     "^\\(\\*+\\) "
     (lambda (match)
       (concat (make-string (+ offset (length (match-string 1 match))) ?*)
               " "))
     text
     t t)))

(defun firefiles--store-last-ai-context (transcript source-text source-kind prompt)
  "Store last Firefiles AI context for redo in the current buffer."
  (setq-local firefiles-last-source-text source-text
              firefiles-last-source-kind source-kind
              firefiles-last-meeting-id (alist-get 'id transcript)
              firefiles-last-meeting-title (alist-get 'title transcript)
              firefiles-last-meeting-date (alist-get 'date transcript)
              firefiles-last-ai-prompt prompt))

(defun firefiles--insert-ai-summary-response
    (buffer marker transcript source-text source-kind prompt response)
  "Insert AI RESPONSE in BUFFER at MARKER and persist local Firefiles state."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char marker)
        (let* ((clean (firefiles--sanitize-ai-response response))
               (text (if (derived-mode-p 'org-mode)
                         (firefiles--relevel-org-text
                          clean
                          (firefiles--org-insertion-base-level))
                       clean)))
          (unless (bolp)
            (insert "\n"))
          (let ((start (point)))
            (insert text)
            (unless (bolp)
              (insert "\n"))
            (firefiles--store-last-ai-context
             transcript source-text source-kind prompt)
            (message "Inserted Firefiles AI summary for: %s"
                     (or (alist-get 'title transcript)
                         (alist-get 'id transcript)))
            start))))))

(defun firefiles--request-ai-summary
    (buffer marker transcript source-text source-kind &optional prompt)
  "Request AI summary for TRANSCRIPT using SOURCE-TEXT and insert into BUFFER."
  (unless (require 'gptel nil t)
    (user-error "gptel is not available"))
  (let ((full-prompt
         (firefiles--build-ai-prompt transcript source-text source-kind prompt)))
    (gptel-request
     full-prompt
     :system firefiles-ai-system-prompt
     :stream nil
     :callback
     (lambda (response info)
       (cond
        ((not response)
         (message "Firefiles AI summary failed: %s" (plist-get info :status)))
        ((stringp response)
         (firefiles--insert-ai-summary-response
          buffer marker transcript source-text source-kind
          (or prompt firefiles-ai-summary-prompt)
          response))
        (t
         (message "Unhandled Firefiles AI response: %S" response)))))))

;;;###autoload
(defun firefiles-insert-meeting-summary (&optional limit)
  "Choose a Fireflies meeting and insert its summary at point.
With prefix arg, prompt for the number of meetings to fetch."
  (interactive
   (list (when current-prefix-arg
           (read-number "How many recent meetings? "
                        firefiles-default-list-limit))))
  (let* ((target-buffer (current-buffer))
         (target-point (point-marker))
         (meeting (firefiles--choose-meeting
                   (or limit firefiles-default-list-limit)))
         (id (alist-get 'id meeting))
         (transcript (firefiles--get-transcript-summary id))
         (text (concat (firefiles--render-summary transcript) "\n")))
    (with-current-buffer target-buffer
      (save-excursion
        (goto-char target-point)
        (insert text)))
    (message "Inserted Firefiles summary for: %s"
             (or (alist-get 'title transcript) id))))

;;;###autoload
(defun firefiles-insert-meeting-ai-summary
    (&optional limit requested-kind prompt-addition)
  "Fetch a Fireflies meeting, summarize it with gptel, and insert Org text.
With prefix arg, prompt for LIMIT, source kind, and extra instructions."
  (interactive
   (if current-prefix-arg
       (list
        (read-number "How many recent meetings? " firefiles-default-list-limit)
        (firefiles--read-source-kind)
        (firefiles--read-custom-ai-prompt-addition))
     (list nil firefiles-ai-source-preference nil)))
  (let* ((target-buffer (current-buffer))
         (target-point (point-marker))
         (meeting (firefiles--choose-meeting
                   (or limit firefiles-default-list-limit)))
         (transcript (firefiles--get-transcript-summary (alist-get 'id meeting)))
         (source (firefiles--build-ai-source transcript requested-kind))
         (prompt (firefiles--compose-ai-prompt prompt-addition)))
    (unless source
      (user-error "Could not retrieve usable Fireflies source text for %s"
                  (or (alist-get 'title transcript)
                      (alist-get 'id transcript))))
    (message "Requesting Firefiles AI summary for %s..."
             (or (alist-get 'title transcript)
                 (alist-get 'id transcript)))
    (firefiles--request-ai-summary
     target-buffer target-point transcript
     (car source) (cdr source) prompt)))

;;;###autoload
(defun firefiles-insert-meeting-source (&optional limit requested-kind)
  "Insert raw Fireflies source material at point."
  (interactive
   (if current-prefix-arg
       (list (read-number "How many recent meetings? "
                          firefiles-default-list-limit)
             (firefiles--read-source-kind))
     (list nil firefiles-ai-source-preference)))
  (let* ((meeting (firefiles--choose-meeting
                   (or limit firefiles-default-list-limit)))
         (transcript (firefiles--get-transcript-summary (alist-get 'id meeting)))
         (source (firefiles--build-ai-source transcript requested-kind)))
    (unless source
      (user-error "Could not retrieve usable Fireflies source text"))
    (insert (car source))
    (unless (bolp)
      (insert "\n"))
    (firefiles--store-last-ai-context
     transcript (car source) (cdr source) firefiles-ai-summary-prompt)
    (message "Inserted Firefiles source for: %s"
             (or (alist-get 'title transcript)
                 (alist-get 'id transcript)))))

;;;###autoload
(defun firefiles-redo-last-ai-summary (&optional edit-prompt)
  "Reuse the last stored Fireflies source in this buffer and rerun AI summary.
With prefix arg, append an extra instruction before rerunning."
  (interactive
   (list (when current-prefix-arg
           (firefiles--read-custom-ai-prompt-addition))))
  (unless firefiles-last-source-text
    (user-error "No stored Firefiles source found in this buffer"))
  (let ((transcript
         `((id . ,firefiles-last-meeting-id)
           (title . ,firefiles-last-meeting-title)
           (date . ,firefiles-last-meeting-date)))
        (prompt
         (firefiles--compose-prompt-from-base
          (or firefiles-last-ai-prompt firefiles-ai-summary-prompt)
          edit-prompt)))
    (message "Rerunning Firefiles AI summary for %s..."
             (or firefiles-last-meeting-title firefiles-last-meeting-id))
    (firefiles--request-ai-summary
     (current-buffer)
     (point-marker)
     transcript
     firefiles-last-source-text
     firefiles-last-source-kind
     prompt)))

(with-eval-after-load 'org
  (when (fboundp 'jds/localleader-def)
    (jds/localleader-def
      :keymaps '(org-mode-map org-capture-mode-map)
      firefiles-localleader-key-prefix '(:ignore t :wk "firefiles")
      (concat firefiles-localleader-key-prefix "s") #'firefiles-insert-meeting-summary
      (concat firefiles-localleader-key-prefix "a") #'firefiles-insert-meeting-ai-summary
      (concat firefiles-localleader-key-prefix "S") #'firefiles-insert-meeting-source
      (concat firefiles-localleader-key-prefix "r") #'firefiles-redo-last-ai-summary)))

(provide 'firefiles)




;;; Insert into org functionality ----------------------------------------------
