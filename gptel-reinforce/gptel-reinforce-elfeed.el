;;; gptel-reinforce-elfeed.el --- Elfeed integration for gptel-reinforce -*- lexical-binding: t; -*-

;;; Commentary:

;; Predefined Elfeed integration for `gptel-reinforce'.

;;; Code:

(require 'subr-x)
(require 'gptel-reinforce-core)
(require 'gptel-reinforce-db)
(require 'gptel-reinforce-org)

(defgroup gptel-reinforce-elfeed nil
  "Elfeed integration for gptel-reinforce."
  :group 'gptel-reinforce)

(defcustom gptel-reinforce-elfeed-score-file
  (expand-file-name "elfeed.score" user-emacs-directory)
  "Score file synchronized with the predefined Elfeed artifact."
  :type 'file
  :group 'gptel-reinforce-elfeed)

(defcustom gptel-reinforce-elfeed-max-excerpt-length 500
  "Maximum number of characters to retain from entry content."
  :type 'integer
  :group 'gptel-reinforce-elfeed)

(defconst gptel-reinforce-elfeed-database-name "elfeed-ranking")
(defconst gptel-reinforce-elfeed-artifact-name "elfeed-score-rules")

(defvar elfeed-show-entry)
(defvar elfeed-search-buffer)

(declare-function elfeed-search-selected "elfeed-search" (&optional ignore-region))
(declare-function elfeed-meta "elfeed-db" (object field &optional default))
(declare-function elfeed-deref "elfeed-lib" (data))
(declare-function elfeed-entry-content "elfeed-db" (entry))
(declare-function elfeed-entry-feed "elfeed-db" (entry))
(declare-function elfeed-feed-url "elfeed-db" (feed))
(declare-function elfeed-entry-link "elfeed-db" (entry))
(declare-function elfeed-entry-date "elfeed-db" (entry))
(declare-function elfeed-entry-title "elfeed-db" (entry))
(declare-function elfeed-score-scoring-get-score-from-entry "elfeed-score-scoring" (entry))
(declare-function elfeed-entry-id "elfeed-db" (entry))
(declare-function elfeed-feed-title "elfeed-db" (feed))
(declare-function elfeed-entry-tags "elfeed-db" (entry))
(declare-function elfeed-score-load-score-file "elfeed-score" (score-file))
(declare-function elfeed-search-update--force "elfeed-search" ())

(defun gptel-reinforce-elfeed-current-entry ()
  "Return the current Elfeed entry in search or show buffers."
  (cond
   ((derived-mode-p 'elfeed-show-mode)
    elfeed-show-entry)
   ((derived-mode-p 'elfeed-search-mode)
    (elfeed-search-selected :ignore-region))
   (t
    (user-error "Not in an Elfeed buffer"))))

(defun gptel-reinforce-elfeed-entry-authors (entry)
  "Return ENTRY authors as a list of names or strings."
  (mapcar (lambda (author)
            (or (plist-get author :name)
                (plist-get author :email)
                (plist-get author :uri)
                (format "%s" author)))
          (elfeed-meta entry :authors)))

(defun gptel-reinforce-elfeed-entry-excerpt (entry)
  "Return a compact plain-text excerpt for ENTRY."
  (let* ((content (elfeed-deref (elfeed-entry-content entry)))
         (text (if (stringp content) content ""))
         (collapsed (replace-regexp-in-string
                     "[[:space:]\n\r\t]+" " "
                     (substring-no-properties text))))
    (string-trim
     (truncate-string-to-width
      collapsed
      gptel-reinforce-elfeed-max-excerpt-length
      nil nil t))))

(defun gptel-reinforce-elfeed-entry-key (entry)
  "Return a stable string key for ENTRY."
  (secure-hash
   'sha1
   (json-encode
    `((feed-url . ,(or (when-let* ((feed (elfeed-entry-feed entry)))
                         (elfeed-feed-url feed))
                       ""))
      (link . ,(or (elfeed-entry-link entry) ""))
      (date . ,(or (elfeed-entry-date entry) 0))
      (title . ,(or (elfeed-entry-title entry) ""))))))

(defun gptel-reinforce-elfeed-context ()
  "Return the current Elfeed entry as a reinforcement context plist."
  (when (and (featurep 'elfeed)
             (derived-mode-p 'elfeed-search-mode 'elfeed-show-mode))
    (let* ((entry (gptel-reinforce-elfeed-current-entry))
           (feed (elfeed-entry-feed entry))
           (score (when (featurep 'elfeed-score)
                    (elfeed-score-scoring-get-score-from-entry entry))))
      (list
       :item-key (gptel-reinforce-elfeed-entry-key entry)
       :title (or (elfeed-meta entry :title)
                  (elfeed-entry-title entry)
                  "")
       :primary-text (gptel-reinforce-elfeed-entry-excerpt entry)
       :meta (list :captured-at (format-time-string "%FT%T%z")
                   :feedback-buffer (buffer-name)
                   :search-filter (when (boundp 'elfeed-search-filter)
                                    elfeed-search-filter)
                   :entry-id (prin1-to-string (elfeed-entry-id entry))
                   :entry-date (elfeed-entry-date entry)
                   :feed-title (and feed
                                    (or (elfeed-meta feed :title)
                                        (elfeed-feed-title feed)))
                   :feed-url (and feed (elfeed-feed-url feed))
                   :link (elfeed-entry-link entry)
                   :tags (mapcar #'symbol-name (elfeed-entry-tags entry))
                   :authors (gptel-reinforce-elfeed-entry-authors entry)
                   :score-at-capture score)))))

(defun gptel-reinforce-elfeed-apply-score-file (_artifact _version-ref _current-record candidate-text)
  "Write CANDIDATE-TEXT to `gptel-reinforce-elfeed-score-file' and reload it."
  (with-temp-file gptel-reinforce-elfeed-score-file
    (insert candidate-text))
  (when (featurep 'elfeed-score)
    (elfeed-score-load-score-file gptel-reinforce-elfeed-score-file)
    (when (get-buffer "*elfeed-search*")
      (with-current-buffer "*elfeed-search*"
        (when (derived-mode-p 'elfeed-search-mode)
          (elfeed-search-update--force))))))

(defun gptel-reinforce-elfeed-validate-score-file (_artifact _current-record candidate-text)
  "Return non-nil when CANDIDATE-TEXT parses as valid Lisp data."
  (condition-case nil
      (progn (read-from-string candidate-text) t)
    (error nil)))

(defun gptel-reinforce-elfeed-seed-score-file (&optional force)
  "Seed the predefined Elfeed artifact from `gptel-reinforce-elfeed-score-file'.
When FORCE is non-nil, overwrite even if the artifact already has text."
  (interactive "P")
  (let* ((artifact (gptel-reinforce-resolve-artifact
                    gptel-reinforce-elfeed-artifact-name))
         (current (gptel-reinforce-org-read-current artifact))
         (score-text (when (file-exists-p gptel-reinforce-elfeed-score-file)
                       (with-temp-buffer
                         (insert-file-contents gptel-reinforce-elfeed-score-file)
                         (buffer-string))))
         (current-text (plist-get current :text)))
    (when (and score-text
               (or force
                   (string-empty-p (string-trim current-text))))
      (when (and force (not (string-empty-p (string-trim current-text))))
        (gptel-reinforce-org-write-history-entry
         artifact
         current-text
         :type (plist-get current :type)
         :updated-at (or (plist-get current :updated-at)
                         (gptel-reinforce--timestamp))
         :update-mode "manual-approved"
         :summary-event-ref 0))
      (let ((version-ref
             (gptel-reinforce-org-write-history-entry
              artifact
              score-text
              :type (plist-get current :type)
              :update-mode "manual-approved"
              :summary-event-ref 0)))
        (gptel-reinforce-org-write-current
         artifact
         :version-ref version-ref
         :text score-text
         :summarizer-user-prompt (plist-get current :summarizer-user-prompt)
         :updater-user-prompt (plist-get current :updater-user-prompt)
         :type (plist-get current :type)
         :auto-update (plist-get current :auto-update))
        version-ref))))

(defun gptel-reinforce-register-elfeed-module ()
  "Register the predefined Elfeed database and score-file artifact."
  (gptel-reinforce-register-database
   :name gptel-reinforce-elfeed-database-name
   :context-fn #'gptel-reinforce-elfeed-context
   :db-path (expand-file-name "var/gptel-reinforce/elfeed-ranking.sqlite" user-emacs-directory)
   :root-dir (expand-file-name "var/gptel-reinforce/elfeed-ranking/" user-emacs-directory)
   :legacy-root-dir "elfeed-ranking")
  (gptel-reinforce-register-artifact
   :name gptel-reinforce-elfeed-artifact-name
   :database gptel-reinforce-elfeed-database-name
   :type "code"
   :auto-update nil
   :summarizer-system-prompt gptel-reinforce-default-summarizer-prompt
   :summarizer-user-prompt
   "Prefer stable scoring rules for research papers, methods, and high-value feeds."
   :updater-system-prompt gptel-reinforce-default-updater-prompt
   :updater-user-prompt
   "Keep the score file readable and prefer conservative edits."
   :pre-update-hook #'gptel-reinforce-elfeed-validate-score-file
   :post-update-hook #'gptel-reinforce-elfeed-apply-score-file)
  (gptel-reinforce-elfeed-seed-score-file)
  t)

(provide 'gptel-reinforce-elfeed)

;;; gptel-reinforce-elfeed.el ends here
