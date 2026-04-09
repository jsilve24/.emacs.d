;;; elfeed-ai.el --- Elfeed reinforcement capture -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'elfeed)
(require 'elfeed-show)

(defgroup jds/elfeed-ai nil
  "Reinforcement capture for Elfeed."
  :group 'elfeed)

(defconst jds/elfeed-reinforcement-default-file
  (expand-file-name "var/elfeed/elfeed-reinforcement" user-emacs-directory)
  "Default location for the Elfeed reinforcement database.")

(defconst jds/elfeed-reinforcement-legacy-file
  (expand-file-name "elfeed-reinforcement" user-emacs-directory)
  "Previous location of the Elfeed reinforcement database.")

(defcustom jds/elfeed-reinforcement-file
  jds/elfeed-reinforcement-default-file
  "File storing newline-delimited Elfeed reinforcement events."
  :type 'file
  :group 'jds/elfeed-ai)

(defcustom jds/elfeed-reinforcement-max-excerpt-length 500
  "Maximum number of characters to retain from entry content."
  :type 'integer
  :group 'jds/elfeed-ai)

;; This path moved out of the config root; force the runtime value to the new
;; location even when this file is reloaded in an existing Emacs session.
(setq jds/elfeed-reinforcement-file jds/elfeed-reinforcement-default-file)

(defun jds/elfeed-migrate-reinforcement-file ()
  "Move legacy reinforcement data into `jds/elfeed-reinforcement-file'."
  (when (file-exists-p jds/elfeed-reinforcement-legacy-file)
    (make-directory (file-name-directory jds/elfeed-reinforcement-file) t)
    (when (> (file-attribute-size
              (file-attributes jds/elfeed-reinforcement-legacy-file))
             0)
      (with-temp-buffer
        (insert-file-contents jds/elfeed-reinforcement-legacy-file)
        (append-to-file (point-min) (point-max)
                        jds/elfeed-reinforcement-file)))
    (delete-file jds/elfeed-reinforcement-legacy-file)))

(jds/elfeed-migrate-reinforcement-file)

(defun jds/elfeed-current-entry ()
  "Return the current Elfeed entry in search or show buffers."
  (cond
   ((derived-mode-p 'elfeed-show-mode)
    elfeed-show-entry)
   ((derived-mode-p 'elfeed-search-mode)
    (elfeed-search-selected :ignore-region))
   (t
    (user-error "Not in an Elfeed buffer"))))

(defun jds/elfeed-entry-authors (entry)
  "Return ENTRY authors as a list of names or strings."
  (mapcar (lambda (author)
            (or (plist-get author :name)
                (plist-get author :email)
                (plist-get author :uri)
                (format "%s" author)))
          (elfeed-meta entry :authors)))

(defun jds/elfeed-entry-excerpt (entry)
  "Return a compact plain-text excerpt for ENTRY."
  (let* ((content (elfeed-deref (elfeed-entry-content entry)))
         (text (cond
                ((stringp content) content)
                (t "")))
         (collapsed (replace-regexp-in-string
                     "[[:space:]\n\r\t]+" " "
                     (substring-no-properties text))))
    (string-trim
     (truncate-string-to-width
      collapsed
      jds/elfeed-reinforcement-max-excerpt-length
      nil nil t))))

(defun jds/elfeed-entry-key (entry)
  "Return a stable string key for ENTRY in the reinforcement db."
  (secure-hash
   'sha1
   (json-encode
    `((feed-url . ,(or (when-let ((feed (elfeed-entry-feed entry)))
                         (elfeed-feed-url feed))
                       ""))
      (link . ,(or (elfeed-entry-link entry) ""))
      (date . ,(or (elfeed-entry-date entry) 0))
      (title . ,(or (elfeed-entry-title entry) ""))))))

(defun jds/elfeed-entry-context (entry)
  "Return a compact plist snapshot describing ENTRY."
  (let* ((feed (elfeed-entry-feed entry))
         (score (when (featurep 'elfeed-score)
                  (elfeed-score-scoring-get-score-from-entry entry))))
    (list
     :captured-at (format-time-string "%FT%T%z")
     :feedback-buffer (buffer-name)
     :search-filter (when (boundp 'elfeed-search-filter)
                      elfeed-search-filter)
     :entry-key (jds/elfeed-entry-key entry)
     :entry-id (prin1-to-string (elfeed-entry-id entry))
     :entry-date (elfeed-entry-date entry)
     :title (or (elfeed-meta entry :title)
                (elfeed-entry-title entry)
                "")
     :feed-title (when feed
                   (or (elfeed-meta feed :title)
                       (elfeed-feed-title feed)))
     :feed-url (when feed (elfeed-feed-url feed))
     :link (elfeed-entry-link entry)
     :tags (mapcar #'symbol-name (elfeed-entry-tags entry))
     :authors (jds/elfeed-entry-authors entry)
     :score score
     :excerpt (jds/elfeed-entry-excerpt entry))))

(defun jds/elfeed-append-reinforcement-event (event)
  "Append EVENT to `jds/elfeed-reinforcement-file' as a single line."
  (make-directory (file-name-directory jds/elfeed-reinforcement-file) t)
  (with-temp-buffer
    (let ((print-length nil)
          (print-level nil))
      (prin1 event (current-buffer))
      (insert "\n"))
    (append-to-file (point-min) (point-max) jds/elfeed-reinforcement-file)))

(defun jds/elfeed-read-reinforcement-events ()
  "Return all saved reinforcement events."
  (when (file-exists-p jds/elfeed-reinforcement-file)
    (with-temp-buffer
      (insert-file-contents jds/elfeed-reinforcement-file)
      (cl-loop for line in (split-string (buffer-string) "\n" t)
               unless (string-empty-p (string-trim line))
               collect (condition-case nil
                           (car (read-from-string line))
                         (error nil))
               into events
               finally return (delq nil events)))))

(defun jds/elfeed-write-reinforcement-events (events)
  "Rewrite `jds/elfeed-reinforcement-file' with EVENTS."
  (make-directory (file-name-directory jds/elfeed-reinforcement-file) t)
  (with-temp-file jds/elfeed-reinforcement-file
    (let ((print-length nil)
          (print-level nil))
      (dolist (event events)
        (prin1 event (current-buffer))
        (insert "\n")))))

(defun jds/elfeed-upsert-feedback (entry feedback)
  "Replace any existing reinforcement record for ENTRY with FEEDBACK.
When FEEDBACK is nil, remove ENTRY from the database."
  (let* ((entry-key (jds/elfeed-entry-key entry))
         (events (jds/elfeed-read-reinforcement-events))
         (filtered
          (seq-remove
           (lambda (event)
             (equal (plist-get (plist-get event :context) :entry-key)
                    entry-key))
           events)))
    (when feedback
      (setq filtered
            (append filtered
                    (list (list :feedback feedback
                                :context (jds/elfeed-entry-context entry))))))
    (jds/elfeed-write-reinforcement-events filtered)))

(defun jds/elfeed-apply-feedback-tags (entry feedback)
  "Synchronize ENTRY tags with FEEDBACK."
  (elfeed-untag entry 'like 'dislike)
  (pcase feedback
    ('like (elfeed-tag entry 'like))
    ('dislike (elfeed-tag entry 'dislike))))

(defun jds/elfeed-refresh-entry (entry)
  "Refresh ENTRY in the Elfeed search buffer when available."
  (when (get-buffer elfeed-search-buffer)
    (with-current-buffer elfeed-search-buffer
      (elfeed-search-update-entry entry))))

(defun jds/elfeed-record-feedback (feedback)
  "Persist FEEDBACK for the current Elfeed entry."
  (let ((entry (jds/elfeed-current-entry)))
    (unless (elfeed-entry-p entry)
      (user-error "No Elfeed entry at point"))
    (jds/elfeed-apply-feedback-tags entry feedback)
    (jds/elfeed-upsert-feedback entry feedback)
    (jds/elfeed-refresh-entry entry)
    (if feedback
        (message "Recorded Elfeed %s: %s"
                 feedback
                 (or (elfeed-entry-title entry) "(untitled)"))
      (message "Removed Elfeed feedback: %s"
               (or (elfeed-entry-title entry) "(untitled)")))))

(defun jds/elfeed-like ()
  "Record positive feedback for the current Elfeed entry."
  (interactive)
  (jds/elfeed-record-feedback 'like))

(defun jds/elfeed-dislike ()
  "Record negative feedback for the current Elfeed entry."
  (interactive)
  (jds/elfeed-record-feedback 'dislike))

(defun jds/elfeed-neutral ()
  "Remove any recorded feedback for the current Elfeed entry."
  (interactive)
  (jds/elfeed-record-feedback nil))

(defun jds/elfeed-capture-reading-list ()
  "Store the current Elfeed entry as a reading-list item and mark it liked."
  (interactive)
  (jds/elfeed-like)
  (org-store-link nil t)
  (org-capture nil "r"))

(with-eval-after-load 'elfeed
  (evil-collection-define-key 'normal 'elfeed-search-mode-map
    "+" #'jds/elfeed-like
    "-" #'jds/elfeed-dislike
    "0" #'jds/elfeed-neutral)
  (evil-collection-define-key 'normal 'elfeed-show-mode-map
    "+" #'jds/elfeed-like
    "-" #'jds/elfeed-dislike
    "0" #'jds/elfeed-neutral))

(provide 'elfeed-ai)
;;; elfeed-ai.el ends here
