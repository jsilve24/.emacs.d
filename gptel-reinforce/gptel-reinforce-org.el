;;; gptel-reinforce-org.el --- Org storage for gptel-reinforce -*- lexical-binding: t; -*-

;;; Commentary:

;; Human-editable Org files for artifacts and summaries.

;;; Code:

(require 'subr-x)
(require 'gptel-reinforce-core)
(require 'gptel-reinforce-db)

(defun gptel-reinforce-org--property-drawer (properties)
  "Render a top-level property drawer from PROPERTIES."
  (concat
   ":PROPERTIES:\n"
   (mapconcat
    (lambda (entry)
      (format ":%s: %s" (car entry) (or (cdr entry) "")))
    properties
    "\n")
   "\n:END:\n\n"))

(defun gptel-reinforce-org--read-file (file)
  "Read FILE and return its contents, or nil if it does not exist."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun gptel-reinforce-org--parse-properties (content)
  "Parse a top-level property drawer from CONTENT."
  (let (properties)
    (when (string-match
           "\\`[[:space:]\n]*:PROPERTIES:\n\\([[:ascii:][:nonascii:]\n]*?\\)\n:END:\n?"
           content)
      (dolist (line (split-string (match-string 1 content) "\n" t))
        (when (string-match "^:\\([^:]+\\):[[:space:]]*\\(.*\\)$" line)
          (push (cons (match-string 1 line) (match-string 2 line)) properties))))
    (nreverse properties)))

(defun gptel-reinforce-org--body-after-properties (content)
  "Return CONTENT without the top-level property drawer."
  (if (string-match
       "\\`[[:space:]\n]*:PROPERTIES:\n\\([[:ascii:][:nonascii:]\n]*?\\)\n:END:\n?"
       content)
      (string-trim-left (substring content (match-end 0)))
    content))

(defun gptel-reinforce-org--indent-body (body)
  "Indent BODY by one leading space per line."
  (mapconcat (lambda (line) (if (string-empty-p line) "" (concat " " line)))
             (split-string (or body "") "\n" nil)
             "\n"))

(defun gptel-reinforce-org--undent-body (body)
  "Remove one leading space from each line in BODY."
  (mapconcat (lambda (line)
               (if (string-prefix-p " " line)
                   (substring line 1)
                 line))
             (split-string (or body "") "\n" nil)
             "\n"))

(defun gptel-reinforce-org--extract-section (content heading)
  "Extract section BODY under HEADING from CONTENT."
  (let* ((pattern (format "^\\* %s\n" (regexp-quote heading)))
         (start (and (string-match pattern content) (match-end 0))))
    (when start
      (let ((end (or (string-match "^\\* " content start)
                     (length content))))
        (gptel-reinforce-org--undent-body
         (string-trim-right (substring content start end)))))))

(defun gptel-reinforce-org--write-file (file content)
  "Write CONTENT to FILE."
  (gptel-reinforce--ensure-directory (file-name-directory file))
  (with-temp-file file
    (insert content)))

(defun gptel-reinforce-org-default-summary-body ()
  "Return the default summary body."
  (string-join
   '("* Summary"
     ""
     "No feedback has been summarized yet."
     ""
     "* Uncertainty"
     ""
     "- No evidence yet."
     ""
     "* Notes"
     "")
   "\n"))

(defun gptel-reinforce-org-normalize-summary-body (body)
  "Ensure BODY looks like a summary.org body."
  (if (string-match-p "^\\* Summary\\(?:\n\\|\\'\\)" (string-trim-left (or body "")))
      (string-trim-right body)
    (string-join
     (list "* Summary" "" (string-trim (or body "")) "" "* Uncertainty" "" "* Notes")
     "\n")))

(defun gptel-reinforce-org-read-current (artifact)
  "Read ARTIFACT's current.org and return a plist."
  (let* ((artifact (gptel-reinforce-resolve-artifact artifact))
         (content (or (gptel-reinforce-org--read-file
                       (gptel-reinforce-artifact-current-file artifact))
                      ""))
         (properties (gptel-reinforce-org--parse-properties content))
         (body (gptel-reinforce-org--body-after-properties content)))
    (list :database (cdr (assoc "DATABASE" properties))
          :artifact (cdr (assoc "ARTIFACT" properties))
          :type (cdr (assoc "TYPE" properties))
          :version-ref (or (cdr (assoc "VERSION_REF" properties))
                           (cdr (assoc "VERSION_ID" properties)))
          :updated-at (cdr (assoc "UPDATED_AT" properties))
          :auto-update (equal (cdr (assoc "AUTO_UPDATE" properties)) "t")
          :text (or (gptel-reinforce-org--extract-section body "Current Text") "")
          :applied-summary
          (or (gptel-reinforce-org--extract-section body "Applied Summary") "")
          :summarizer-user-prompt
          (or (gptel-reinforce-org--extract-section body "Summarizer User Prompt") "")
          :updater-user-prompt
          (or (gptel-reinforce-org--extract-section body "Updater User Prompt") ""))))

(defun gptel-reinforce-org-read-summary (artifact)
  "Read ARTIFACT's summary.org and return a plist."
  (let* ((artifact (gptel-reinforce-resolve-artifact artifact))
         (content (or (gptel-reinforce-org--read-file
                       (gptel-reinforce-artifact-summary-file artifact))
                      ""))
         (properties (gptel-reinforce-org--parse-properties content))
         (body (gptel-reinforce-org--body-after-properties content)))
    (list :database (cdr (assoc "DATABASE" properties))
          :artifact (cdr (assoc "ARTIFACT" properties))
          :last-event-id (when-let* ((value (cdr (assoc "LAST_EVENT_ID" properties))))
                            (string-to-number value))
          :updated-at (cdr (assoc "UPDATED_AT" properties))
          :body (string-trim body))))

(defun gptel-reinforce-org-write-current (artifact &rest plist)
  "Write ARTIFACT's current.org from PLIST."
  (let* ((artifact (gptel-reinforce-resolve-artifact artifact))
         (file (gptel-reinforce-artifact-current-file artifact))
         (text (or (plist-get plist :text) ""))
         (applied-summary (or (plist-get plist :applied-summary) ""))
         (summarizer-user-prompt (or (plist-get plist :summarizer-user-prompt) ""))
         (updater-user-prompt (or (plist-get plist :updater-user-prompt) ""))
         (type (or (plist-get plist :type)
                   (gptel-reinforce-artifact-type artifact)
                   ""))
         (version-ref (plist-get plist :version-ref))
         (updated-at (or (plist-get plist :updated-at)
                         (gptel-reinforce--timestamp)))
         (auto-update (if (plist-member plist :auto-update)
                          (plist-get plist :auto-update)
                        (gptel-reinforce-artifact-auto-update artifact)))
         (content
          (concat
           (gptel-reinforce-org--property-drawer
           `(("DATABASE" . ,(gptel-reinforce-artifact-database artifact))
              ("ARTIFACT" . ,(gptel-reinforce-artifact-name artifact))
              ("TYPE" . ,type)
              ("VERSION_REF" . ,(or version-ref ""))
              ("UPDATED_AT" . ,updated-at)
              ("AUTO_UPDATE" . ,(if auto-update "t" "nil"))))
           "* Current Text\n"
           (gptel-reinforce-org--indent-body text)
           "\n\n* Applied Summary\n"
           (gptel-reinforce-org--indent-body applied-summary)
           "\n\n* Summarizer User Prompt\n"
           (gptel-reinforce-org--indent-body summarizer-user-prompt)
           "\n\n* Updater User Prompt\n"
           (gptel-reinforce-org--indent-body updater-user-prompt)
           "\n")))
    (gptel-reinforce-org--write-file file content)
    file))

(defun gptel-reinforce-org-write-summary (artifact body &optional last-event-id updated-at)
  "Write ARTIFACT's summary.org with BODY."
  (let* ((artifact (gptel-reinforce-resolve-artifact artifact))
         (file (gptel-reinforce-artifact-summary-file artifact))
         (content
          (concat
           (gptel-reinforce-org--property-drawer
            `(("DATABASE" . ,(gptel-reinforce-artifact-database artifact))
              ("ARTIFACT" . ,(gptel-reinforce-artifact-name artifact))
              ("LAST_EVENT_ID" . ,(format "%s" (or last-event-id 0)))
              ("UPDATED_AT" . ,(or updated-at (gptel-reinforce--timestamp)))))
           (string-trim-right
            (gptel-reinforce-org-normalize-summary-body
             (or body (gptel-reinforce-org-default-summary-body))))
           "\n")))
    (gptel-reinforce-org--write-file file content)
    file))

(defun gptel-reinforce-org-write-history-entry (artifact text &rest plist)
  "Write a history snapshot for ARTIFACT containing TEXT.
Return the history file basename."
  (let* ((artifact (gptel-reinforce-resolve-artifact artifact))
         (history-dir (gptel-reinforce-artifact-history-dir artifact))
         (updated-at (or (plist-get plist :updated-at)
                         (gptel-reinforce--timestamp)))
         (summary-event-ref (plist-get plist :summary-event-ref))
         (update-mode (or (plist-get plist :update-mode) "manual-approved"))
         (base-name (format "%s-%s.org"
                            (format-time-string "%Y%m%dT%H%M%S")
                            (gptel-reinforce-artifact-name artifact)))
         (history-file (expand-file-name base-name history-dir))
         (content
          (concat
           (gptel-reinforce-org--property-drawer
            `(("DATABASE" . ,(gptel-reinforce-artifact-database artifact))
              ("ARTIFACT" . ,(gptel-reinforce-artifact-name artifact))
              ("TYPE" . ,(or (plist-get plist :type)
                             (gptel-reinforce-artifact-type artifact)
                             ""))
              ("UPDATED_AT" . ,updated-at)
              ("UPDATE_MODE" . ,update-mode)
              ("SOURCE_SUMMARY_LAST_EVENT_ID" . ,(format "%s" (or summary-event-ref 0)))))
           "* Current Text\n"
           (gptel-reinforce-org--indent-body text)
           "\n")))
    (gptel-reinforce--ensure-directory history-dir)
    (gptel-reinforce-org--write-file history-file content)
    base-name))

(defun gptel-reinforce-org-initialize-artifact (artifact)
  "Ensure ARTIFACT has current.org, summary.org, and archive state."
  (let* ((artifact (gptel-reinforce-resolve-artifact artifact))
         (artifact-dir (gptel-reinforce-artifact-dir artifact))
         (history-dir (gptel-reinforce-artifact-history-dir artifact))
         (current-file (gptel-reinforce-artifact-current-file artifact))
         (summary-file (gptel-reinforce-artifact-summary-file artifact)))
    (gptel-reinforce--ensure-directory artifact-dir)
    (gptel-reinforce--ensure-directory history-dir)
    (unless (file-exists-p current-file)
      (let ((version-ref
             (gptel-reinforce-org-write-history-entry
              artifact
              ""
              :type (gptel-reinforce-artifact-type artifact)
              :update-mode "initial"
              :summary-event-ref 0)))
        (gptel-reinforce-org-write-current
         artifact
         :version-ref version-ref
         :text ""
         :summarizer-user-prompt
         (gptel-reinforce-artifact-summarizer-user-prompt artifact)
         :updater-user-prompt
         (gptel-reinforce-artifact-updater-user-prompt artifact)
         :type (gptel-reinforce-artifact-type artifact)
         :auto-update (gptel-reinforce-artifact-auto-update artifact))))
    (let* ((current (gptel-reinforce-org-read-current artifact))
           (version-ref (plist-get current :version-ref)))
      (unless (and version-ref
                   (file-exists-p
                    (expand-file-name version-ref history-dir)))
        (setq version-ref
              (gptel-reinforce-org-write-history-entry
               artifact
               (plist-get current :text)
               :type (or (plist-get current :type)
                         (gptel-reinforce-artifact-type artifact))
               :updated-at (or (plist-get current :updated-at)
                               (gptel-reinforce--timestamp))
               :update-mode "initial"
               :summary-event-ref 0))
        (gptel-reinforce-org-write-current
         artifact
         :version-ref version-ref
         :text (plist-get current :text)
         :summarizer-user-prompt (plist-get current :summarizer-user-prompt)
         :updater-user-prompt (plist-get current :updater-user-prompt)
         :type (or (plist-get current :type)
                   (gptel-reinforce-artifact-type artifact))
         :auto-update (plist-get current :auto-update))))
    (unless (file-exists-p summary-file)
      (gptel-reinforce-org-write-summary
       artifact
       (gptel-reinforce-org-default-summary-body)
       0))
    artifact))

(provide 'gptel-reinforce-org)

;;; gptel-reinforce-org.el ends here
