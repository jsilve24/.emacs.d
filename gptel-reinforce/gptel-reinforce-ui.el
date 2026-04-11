;;; gptel-reinforce-ui.el --- Interactive commands for gptel-reinforce -*- lexical-binding: t; -*-

;;; Commentary:

;; User-facing commands for feedback capture, summarization, and updates.

;;; Code:

(require 'diff)
(require 'smerge-mode)
(require 'subr-x)
(require 'gptel-reinforce-core)
(require 'gptel-reinforce-db)
(require 'gptel-reinforce-org)
(require 'gptel-reinforce-backend)

(defvar-local gptel-reinforce--review-finished nil
  "Non-nil when the current review buffer was accepted.")

(defvar-local gptel-reinforce--review-canceled nil
  "Non-nil when the current review buffer was canceled.")

(defun gptel-reinforce-review-accept ()
  "Finish the current review buffer and accept its contents."
  (interactive)
  (setq gptel-reinforce--review-finished t
        gptel-reinforce--review-canceled nil)
  (exit-recursive-edit))

(defun gptel-reinforce-review-cancel ()
  "Abort the current review buffer."
  (interactive)
  (setq gptel-reinforce--review-finished nil
        gptel-reinforce--review-canceled t)
  (exit-recursive-edit))

(defvar gptel-reinforce-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'gptel-reinforce-review-accept)
    (define-key map (kbd "C-c C-k") #'gptel-reinforce-review-cancel)
    map)
  "Keymap used while editing gptel-reinforce review buffers.")

(defun gptel-reinforce--read-note (prefix)
  "Return a note when PREFIX is non-nil."
  (when prefix
    (let ((note (read-string "Note: ")))
      (unless (string-empty-p note)
        note))))

(defun gptel-reinforce--record-item-feedback (score prefix &optional database)
  "Record item feedback SCORE using DATABASE.
When PREFIX is non-nil, prompt for a note."
  (pcase-let ((`(,db . ,context)
               (gptel-reinforce-resolve-database-and-context database t)))
    (unless db
      (user-error "Could not resolve a database"))
    (unless context
      (user-error "No item context is available for %s"
                  (gptel-reinforce-database-name db)))
    (let* ((note (gptel-reinforce--read-note prefix))
           (event-id
            (gptel-reinforce-db-record-feedback
             db
             (list :event-type "item-feedback"
                   :item-key (plist-get context :item-key)
                   :score score
                   :title (plist-get context :title)
                   :primary-text (plist-get context :primary-text)
                   :meta (plist-get context :meta)
                   :note note))))
      (message "Recorded item feedback in %s as event %s"
               (gptel-reinforce-database-name db)
               event-id)
      event-id)))

(defun gptel-reinforce--record-output-feedback (score prefix &optional provenance database)
  "Record output feedback SCORE.
When PREFIX is non-nil, prompt for a note.  Use PROVENANCE if supplied."
  (let* ((provenance (or provenance (gptel-reinforce-output-at-point)))
         (database-name (or database (plist-get provenance :database)))
         (db (and database-name
                  (gptel-reinforce-resolve-database database-name)))
         (context (and db
                       (gptel-reinforce-context-for-database db))))
    (unless provenance
      (user-error "No output provenance is available at point"))
    (unless db
      (user-error "Could not resolve a database"))
    (unless context
      (user-error "No item context is available for %s"
                  (gptel-reinforce-database-name db)))
    (let* ((note (gptel-reinforce--read-note prefix))
           (event-id
            (gptel-reinforce-db-record-feedback
             db
             (list :event-type "output-feedback"
                   :item-key (plist-get context :item-key)
                   :score score
                   :title (plist-get context :title)
                   :primary-text (plist-get context :primary-text)
                   :meta (plist-get context :meta)
                   :note note
                   :artifact-name (plist-get provenance :artifact-name)
                   :artifact-version-ref (plist-get provenance :artifact-version-ref)
                   :output-id (plist-get provenance :output-id)))))
      (message "Recorded output feedback for %s version %s as event %s"
               (plist-get provenance :artifact-name)
               (plist-get provenance :artifact-version-ref)
               event-id)
      event-id)))

;;;###autoload
(defun gptel-reinforce-score-output (prefix &optional score database provenance)
  "Record output feedback.
With PREFIX, prompt for a note."
  (interactive "P")
  (let ((score (or score (read-number "Output score: "))))
    (gptel-reinforce--record-output-feedback score prefix provenance database)))

;;;###autoload
(defun gptel-reinforce-score-item (prefix &optional score database)
  "Record item feedback.
With PREFIX, prompt for a note."
  (interactive "P")
  (let ((score (or score (read-number "Item score: "))))
    (gptel-reinforce--record-item-feedback score prefix database)))

;;;###autoload
(defun gptel-reinforce-like-item (prefix &optional database)
  "Record +1 item feedback.
With PREFIX, prompt for a note."
  (interactive "P")
  (gptel-reinforce--record-item-feedback 1 prefix database))

;;;###autoload
(defun gptel-reinforce-dislike-item (prefix &optional database)
  "Record -1 item feedback.
With PREFIX, prompt for a note."
  (interactive "P")
  (gptel-reinforce--record-item-feedback -1 prefix database))

;;;###autoload
(defun gptel-reinforce-neutral-item (prefix &optional database)
  "Record 0 item feedback.
With PREFIX, prompt for a note."
  (interactive "P")
  (gptel-reinforce--record-item-feedback 0 prefix database))

;;;###autoload
(defun gptel-reinforce-like (prefix &optional database)
  "Record +1 feedback for the current output when available, else the current item.
With PREFIX, prompt for a note."
  (interactive "P")
  (if (gptel-reinforce-output-at-point)
      (gptel-reinforce--record-output-feedback 1 prefix nil database)
    (gptel-reinforce--record-item-feedback 1 prefix database)))

;;;###autoload
(defun gptel-reinforce-dislike (prefix &optional database)
  "Record -1 feedback for the current output when available, else the current item.
With PREFIX, prompt for a note."
  (interactive "P")
  (if (gptel-reinforce-output-at-point)
      (gptel-reinforce--record-output-feedback -1 prefix nil database)
    (gptel-reinforce--record-item-feedback -1 prefix database)))

;;;###autoload
(defun gptel-reinforce-neutral (prefix &optional database)
  "Record 0 feedback for the current output when available, else the current item.
With PREFIX, prompt for a note."
  (interactive "P")
  (if (gptel-reinforce-output-at-point)
      (gptel-reinforce--record-output-feedback 0 prefix nil database)
    (gptel-reinforce--record-item-feedback 0 prefix database)))

;;;###autoload
(defun gptel-reinforce-score (prefix &optional score database)
  "Record numeric feedback for output when available, else for the current item.
With PREFIX, prompt for a note."
  (interactive "P")
  (let ((score (or score (read-number "Score: "))))
    (if (gptel-reinforce-output-at-point)
        (gptel-reinforce--record-output-feedback score prefix nil database)
      (gptel-reinforce--record-item-feedback score prefix database))))

(defun gptel-reinforce--run-pre-update-hooks (artifact current-record candidate-text)
  "Run pre-update hooks for ARTIFACT.
Abort when any hook returns nil."
  (dolist (hook (gptel-reinforce-artifact-pre-update-hooks artifact))
    (unless (funcall hook artifact current-record candidate-text)
      (user-error "Pre-update hook %S rejected the candidate" hook))))

(defun gptel-reinforce--run-post-update-hooks (artifact version-ref current-record candidate-text)
  "Run post-update hooks for ARTIFACT.
Errors are surfaced after all hooks run."
  (let (first-error)
    (dolist (hook (gptel-reinforce-artifact-post-update-hooks artifact))
      (condition-case err
          (funcall hook artifact version-ref current-record candidate-text)
        (error
         (unless first-error
           (setq first-error err))
         (display-warning
          'gptel-reinforce
          (format "Post-update hook %S failed: %s"
                  hook
                  (error-message-string err))
          :error))))
    (when first-error
      (signal (car first-error) (cdr first-error)))))

(defun gptel-reinforce--show-diff-review (title old-text new-text)
  "Show a read-only diff for TITLE and return non-nil when accepted."
  (let* ((old-file (make-temp-file "gptel-reinforce-old-"))
         (new-file (make-temp-file "gptel-reinforce-new-"))
         (diff-buffer nil))
    (unwind-protect
        (progn
          (with-temp-file old-file (insert old-text))
          (with-temp-file new-file (insert new-text))
          (setq diff-buffer (diff-no-select old-file new-file "-u" t))
          (with-current-buffer diff-buffer
            (setq buffer-read-only t)
            (rename-buffer
             (format "*gptel-reinforce diff: %s*" title)
             t))
          (display-buffer diff-buffer)
          (when (y-or-n-p (format "Accept update for %s? " title))
            new-text))
      (ignore-errors (delete-file old-file))
      (ignore-errors (delete-file new-file)))))

(defun gptel-reinforce--conflict-markers-present-p ()
  "Return non-nil when the current buffer still contains merge markers."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^<<<<<<< " nil t)))

(defun gptel-reinforce--show-smerge-review (title old-text new-text)
  "Open an editable smerge buffer for TITLE and return the accepted text."
  (let ((buffer (generate-new-buffer (format "*gptel-reinforce review: %s*" title))))
    (unwind-protect
        (with-current-buffer buffer
          (erase-buffer)
          (insert "<<<<<<< current\n")
          (insert old-text)
          (unless (or (string-empty-p old-text)
                      (string-suffix-p "\n" old-text))
            (insert "\n"))
          (insert "=======\n")
          (insert new-text)
          (unless (or (string-empty-p new-text)
                      (string-suffix-p "\n" new-text))
            (insert "\n"))
          (insert ">>>>>>> candidate\n")
          (text-mode)
          (use-local-map gptel-reinforce-review-mode-map)
          (setq-local header-line-format
                      "Resolve/edit, then C-c C-c to apply or C-c C-k to cancel")
          (setq-local gptel-reinforce--review-finished nil)
          (setq-local gptel-reinforce--review-canceled nil)
          (goto-char (point-min))
          (smerge-mode 1)
          (display-buffer buffer)
          (recursive-edit)
          (when gptel-reinforce--review-finished
            (when (and (gptel-reinforce--conflict-markers-present-p)
                       (not (y-or-n-p
                             (format "Merge markers remain in %s. Apply anyway? "
                                     title))))
              (setq gptel-reinforce--review-finished nil))
            (when gptel-reinforce--review-finished
              (buffer-substring-no-properties (point-min) (point-max)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun gptel-reinforce--review-text (title old-text new-text review-mode)
  "Review NEW-TEXT against OLD-TEXT for TITLE using REVIEW-MODE.
Return the accepted text, or nil when rejected."
  (pcase review-mode
    ('nil new-text)
    ('diff (gptel-reinforce--show-diff-review title old-text new-text))
    ('smerge (gptel-reinforce--show-smerge-review title old-text new-text))
    (_ (user-error "Unsupported review mode: %S" review-mode))))

;;;###autoload
(defun gptel-reinforce-summarize (artifact)
  "Summarize new feedback for ARTIFACT."
  (interactive
   (list (completing-read "Artifact: " (gptel-reinforce-list-artifacts) nil t)))
  (let* ((artifact (gptel-reinforce-resolve-artifact artifact))
         (summary-record (gptel-reinforce-org-read-summary artifact))
         (last-event-id (or (plist-get summary-record :last-event-id) 0))
         (database (gptel-reinforce-artifact-database artifact))
         (events (gptel-reinforce-db-feedback-since
                  database
                  last-event-id
                  (gptel-reinforce-artifact-name artifact))))
    (if (null events)
        (message "No new feedback to summarize for %s"
                 (gptel-reinforce-artifact-name artifact))
      (let* ((current-record (gptel-reinforce-org-read-current artifact))
             (request
              (gptel-reinforce-summary-request-create
               :database-name database
               :artifact-name (gptel-reinforce-artifact-name artifact)
               :artifact-type (gptel-reinforce-artifact-type artifact)
               :existing-summary (plist-get summary-record :body)
               :events events
               :system-prompt
               (gptel-reinforce-artifact-effective-summarizer-prompt
                artifact current-record)))
             (target-event-id (plist-get (car (last events)) :id)))
        (message "Summarizing %s (%s new events)"
                 (gptel-reinforce-artifact-name artifact)
                 (length events))
        (gptel-reinforce-backend-send
         request
         (lambda (response _info)
           (when-let* ((summary-body
                        (gptel-reinforce--review-text
                         (format "%s summary" (gptel-reinforce-artifact-name artifact))
                         (plist-get summary-record :body)
                         response
                         gptel-reinforce-summary-review-mode)))
             (gptel-reinforce-org-write-summary artifact summary-body target-event-id)
             (message "Updated summary for %s through event %s"
                      (gptel-reinforce-artifact-name artifact)
                      target-event-id))))))))

;;;###autoload
(defun gptel-reinforce-update (artifact)
  "Update ARTIFACT from its current summary."
  (interactive
   (list (completing-read "Artifact: " (gptel-reinforce-list-artifacts) nil t)))
  (let* ((artifact (gptel-reinforce-resolve-artifact artifact))
         (current-record (gptel-reinforce-org-read-current artifact))
         (summary-record (gptel-reinforce-org-read-summary artifact))
         (request
          (gptel-reinforce-update-request-create
           :database-name (gptel-reinforce-artifact-database artifact)
           :artifact-name (gptel-reinforce-artifact-name artifact)
           :artifact-type (or (plist-get current-record :type)
                              (gptel-reinforce-artifact-type artifact))
           :current-text (plist-get current-record :text)
           :applied-summary-body (plist-get current-record :applied-summary)
           :summary-body (plist-get summary-record :body)
           :system-prompt
           (gptel-reinforce-artifact-effective-updater-prompt
            artifact current-record))))
    (message "Requesting updated text for %s" (gptel-reinforce-artifact-name artifact))
    (gptel-reinforce-backend-send
     request
     (lambda (response _info)
       (let* ((candidate-text response)
              (auto-update (plist-get current-record :auto-update))
              (approved-text
               (if auto-update
                   candidate-text
                  (gptel-reinforce--review-text
                   (gptel-reinforce-artifact-name artifact)
                   (plist-get current-record :text)
                   candidate-text
                   gptel-reinforce-update-review-mode))))
         (when approved-text
           (gptel-reinforce--run-pre-update-hooks artifact current-record approved-text)
           (let ((version-ref
                  (gptel-reinforce-org-write-history-entry
                   artifact
                   approved-text
                   :type (or (plist-get current-record :type)
                             (gptel-reinforce-artifact-type artifact))
                   :summary-event-ref (or (plist-get summary-record :last-event-id) 0)
                   :update-mode (if auto-update "auto-updated" "manual-approved"))))
             (gptel-reinforce-org-write-current
              artifact
              :version-ref version-ref
              :text approved-text
              :applied-summary (plist-get summary-record :body)
              :summarizer-user-prompt (plist-get current-record :summarizer-user-prompt)
              :updater-user-prompt (plist-get current-record :updater-user-prompt)
              :type (or (plist-get current-record :type)
                        (gptel-reinforce-artifact-type artifact))
              :auto-update auto-update)
             (message "Updated %s to version %s"
                      (gptel-reinforce-artifact-name artifact)
                      version-ref)
             (gptel-reinforce--run-post-update-hooks
              artifact
              version-ref
              current-record
              approved-text))))))))

(provide 'gptel-reinforce-ui)

;;; gptel-reinforce-ui.el ends here
