;;; gptel-reinforce-ui.el --- Interactive commands for gptel-reinforce -*- lexical-binding: t; -*-

;;; Commentary:

;; User-facing commands for feedback capture, summarization, and updates.

;;; Code:

(require 'diff)
(require 'ediff)
(require 'subr-x)
(require 'gptel-reinforce-core)
(require 'gptel-reinforce-db)
(require 'gptel-reinforce-org)
(require 'gptel-reinforce-backend)

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

(defun gptel-reinforce--apply-artifact-text (artifact current-record candidate-text &rest plist)
  "Apply CANDIDATE-TEXT to ARTIFACT and return the new version ref.
CURRENT-RECORD is the parsed current state.  Optional PLIST keys:
:summary-event-ref, :applied-summary, :update-mode, and
:rollback-source-version-ref."
  (gptel-reinforce--run-pre-update-hooks artifact current-record candidate-text)
  (let ((version-ref
         (gptel-reinforce-org-write-history-entry
          artifact
          candidate-text
          :type (or (plist-get current-record :type)
                    (gptel-reinforce-artifact-type artifact))
          :summary-event-ref (or (plist-get plist :summary-event-ref) 0)
          :update-mode (or (plist-get plist :update-mode) "manual-approved")
          :rollback-source-version-ref
          (plist-get plist :rollback-source-version-ref))))
    (gptel-reinforce-org-write-current
     artifact
     :version-ref version-ref
     :text candidate-text
     :applied-summary (or (plist-get plist :applied-summary) "")
     :summarizer-user-prompt (plist-get current-record :summarizer-user-prompt)
     :updater-user-prompt (plist-get current-record :updater-user-prompt)
     :type (or (plist-get current-record :type)
               (gptel-reinforce-artifact-type artifact))
     :auto-update (plist-get current-record :auto-update)
     :config-hash (plist-get current-record :config-hash))
    (gptel-reinforce--run-post-update-hooks
     artifact
     version-ref
     current-record
     candidate-text)
    version-ref))

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

(defun gptel-reinforce--show-edit-review (title old-text new-text)
  "Open an ediff session to review and optionally edit NEW-TEXT against OLD-TEXT for TITLE.
\\`C-c C-c' accepts, \\`C-c C-k' rejects.  \\`q' quits ediff then prompts.
Return the accepted text, or nil when rejected."
  (let* ((old-buf (generate-new-buffer
                   (format " *gptel-reinforce-old: %s*" title)))
         (new-buf (generate-new-buffer
                   (format "*gptel-reinforce new: %s*" title)))
         (result nil)
         (done nil)
         (exit-fn (lambda ()
                    (condition-case nil (exit-recursive-edit) (error nil)))))
    (with-current-buffer old-buf
      (insert old-text)
      (setq buffer-read-only t))
    (with-current-buffer new-buf
      (insert new-text))
    (unwind-protect
        (progn
          (ediff-buffers
           old-buf new-buf
           (list
            (lambda ()
              ;; C-c C-c: accept immediately, no extra prompts
              (local-set-key
               (kbd "C-c C-c")
               (lambda () (interactive)
                 (unless done
                   (setq done t
                         result (with-current-buffer new-buf
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))
                   (ediff-really-quit t))))
              ;; C-c C-k: reject immediately, no extra prompts
              (local-set-key
               (kbd "C-c C-k")
               (lambda () (interactive)
                 (unless done
                   (setq done t)
                   (ediff-really-quit t))))
              ;; Normal q-quit: prompt to confirm
              (add-hook 'ediff-after-quit-hook-internal
                        (lambda ()
                          (unless done
                            (setq done t)
                            (when (and (buffer-live-p new-buf)
                                       (y-or-n-p (format "Apply update for %s? " title)))
                              (setq result
                                    (with-current-buffer new-buf
                                      (buffer-substring-no-properties
                                       (point-min) (point-max))))))
                          (funcall exit-fn))
                        nil t)
              ;; Guard against the control buffer being killed abnormally
              (add-hook 'kill-buffer-hook
                        (lambda ()
                          (unless done (setq done t))
                          (funcall exit-fn))
                        nil t))))
          (recursive-edit)
          result)
      (when (buffer-live-p old-buf) (kill-buffer old-buf))
      (when (buffer-live-p new-buf) (kill-buffer new-buf)))))

(defun gptel-reinforce--review-text (title old-text new-text review-mode)
  "Review NEW-TEXT against OLD-TEXT for TITLE using REVIEW-MODE.
Return the accepted text, or nil when rejected."
  (pcase review-mode
    ('nil new-text)
    ('diff (gptel-reinforce--show-diff-review title old-text new-text))
    ('edit (gptel-reinforce--show-edit-review title old-text new-text))
    (_ (user-error "Unsupported review mode: %S" review-mode))))

(defun gptel-reinforce--history-entry-excerpt (entry)
  "Return a short one-line excerpt for history ENTRY."
  (let* ((text (string-trim (or (plist-get entry :text) "")))
         (line (car (split-string text "\n"))))
    (truncate-string-to-width (or line "") 60 nil nil t)))

(defun gptel-reinforce--read-history-version (artifact)
  "Prompt for a history version for ARTIFACT and return its version ref."
  (let* ((entries (gptel-reinforce-org-list-history-entries artifact))
         (candidates (mapcar (lambda (entry)
                               (cons (plist-get entry :version-ref) entry))
                             entries)))
    (unless candidates
      (user-error "No history entries found for %s"
                  (gptel-reinforce-artifact-name artifact)))
    (let ((completion-extra-properties
           `(:affixation-function
             ,(lambda (versions)
                (mapcar
                 (lambda (version)
                   (let* ((entry (alist-get version candidates nil nil #'equal))
                          (updated-at (or (plist-get entry :updated-at) ""))
                          (update-mode (or (plist-get entry :update-mode) ""))
                          (excerpt (gptel-reinforce--history-entry-excerpt entry)))
                     (list version
                           ""
                           (format "  %s  [%s]  %s"
                                   updated-at
                                   update-mode
                                   excerpt))))
                 versions)))))
      (completing-read
       (format "Rollback %s to version: "
               (gptel-reinforce-artifact-name artifact))
       candidates
       nil
       t))))

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
           (let ((version-ref
                  (gptel-reinforce--apply-artifact-text
                   artifact
                   current-record
                   approved-text
                   :summary-event-ref (or (plist-get summary-record :last-event-id) 0)
                   :applied-summary (plist-get summary-record :body)
                   :update-mode (if auto-update "auto-updated" "manual-approved"))))
             (message "Updated %s to version %s"
                      (gptel-reinforce-artifact-name artifact)
                      version-ref))))))))

;;;###autoload
(defun gptel-reinforce-rollback (artifact version-ref)
  "Roll ARTIFACT back to VERSION-REF from artifact history."
  (interactive
   (let* ((artifact-name (completing-read
                          "Artifact: "
                          (gptel-reinforce-list-artifacts)
                          nil t))
          (artifact (gptel-reinforce-resolve-artifact artifact-name)))
     (list artifact
           (gptel-reinforce--read-history-version artifact))))
  (let* ((artifact (gptel-reinforce-resolve-artifact artifact))
         (current-record (gptel-reinforce-org-read-current artifact))
         (history-entry (gptel-reinforce-org-read-history-entry artifact version-ref))
         (target-text (plist-get history-entry :text))
         (approved-text
          (gptel-reinforce--review-text
           (format "%s rollback to %s"
                   (gptel-reinforce-artifact-name artifact)
                   (plist-get history-entry :version-ref))
           (plist-get current-record :text)
           target-text
           gptel-reinforce-update-review-mode)))
    (when approved-text
      (let ((new-version-ref
             (gptel-reinforce--apply-artifact-text
              artifact
              current-record
              approved-text
              :summary-event-ref (plist-get history-entry :summary-event-ref)
              :applied-summary ""
              :update-mode "rollback"
              :rollback-source-version-ref (plist-get history-entry :version-ref))))
        (message "Rolled %s back to %s as new version %s"
                 (gptel-reinforce-artifact-name artifact)
                 (plist-get history-entry :version-ref)
                 new-version-ref)))))

(provide 'gptel-reinforce-ui)

;;; gptel-reinforce-ui.el ends here
