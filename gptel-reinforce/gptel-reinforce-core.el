;;; gptel-reinforce-core.el --- Core state for gptel-reinforce -*- lexical-binding: t; -*-

;; Author: Justin Silverman <justinsilverman@psu.edu>
;; Maintainer: Justin Silverman <justinsilverman@psu.edu>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (gptel "0"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Shared state, registration, prompts, and utility helpers for
;; `gptel-reinforce'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)

(declare-function gptel-reinforce-db-ensure-schema "gptel-reinforce-db" (database))
(declare-function gptel-reinforce-org-initialize-artifact "gptel-reinforce-org" (artifact))
(declare-function gptel-reinforce-org-read-current "gptel-reinforce-org" (artifact))

(defgroup gptel-reinforce nil
  "Local reinforcement learning for evolving text artifacts."
  :group 'applications
  :prefix "gptel-reinforce-")

(defcustom gptel-reinforce-state-root
  (expand-file-name "var/gptel-reinforce/" user-emacs-directory)
  "Directory holding SQLite database files."
  :type 'directory)

(defcustom gptel-reinforce-config-root
  (expand-file-name "gptel-reinforce/" user-emacs-directory)
  "Directory holding human-editable Org files."
  :type 'directory)

(defconst gptel-reinforce-default-summarizer-prompt
  (string-join
   '("You maintain a running summary of user feedback about a general text artifact."
     "Update the summary from the new evidence only."
     "Focus on stable patterns instead of reacting to one-offs."
     "Handle conflicting evidence conservatively."
     "Mark mixed evidence as uncertain instead of forcing a conclusion."
     "Prefer discarding noisy patterns over inventing false certainty."
     "Treat optional user notes as high-value evidence."
     "The artifact may be a prompt, code block, rule set, template, or config snippet."
     "Return Org content with exactly these top-level headings:"
     "* Summary"
     "* Uncertainty"
     "* Notes")
   "\n")
  "Default system prompt for summary updates.")

(defconst gptel-reinforce-default-updater-prompt
  (string-join
   '("You revise a general text artifact from a feedback summary."
     "Make the smallest useful change."
     "Preserve structure and tone unless the evidence strongly justifies changing them."
     "Ignore weak, noisy, or conflicting evidence."
     "Avoid vague filler."
     "The artifact may be a prompt, code block, rule set, template, or config snippet."
     "Return only the revised artifact text."
     "Do not wrap the result in Markdown fences.")
   "\n")
  "Default system prompt for artifact updates.")

(cl-defstruct (gptel-reinforce-database
               (:constructor gptel-reinforce-database-create))
  name
  context-fn
  db-path
  root-dir)

(cl-defstruct (gptel-reinforce-artifact
               (:constructor gptel-reinforce-artifact-create))
  name
  database
  type
  auto-update
  summarizer-system-prompt
  summarizer-user-prompt
  updater-system-prompt
  updater-user-prompt
  pre-update-hooks
  post-update-hooks)

(defvar gptel-reinforce--databases (make-hash-table :test #'equal)
  "Registered reinforcement databases keyed by name.")

(defvar gptel-reinforce--artifacts (make-hash-table :test #'equal)
  "Registered artifacts keyed by name.")

(defvar-local gptel-reinforce-active-database nil
  "Buffer-local default database name for feedback commands.")

(defvar-local gptel-reinforce-last-output-provenance nil
  "Buffer-local fallback output provenance plist.")

(defun gptel-reinforce--timestamp ()
  "Return a local ISO-8601-like timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun gptel-reinforce--ensure-directory (dir)
  "Create DIR and parents if needed."
  (unless (file-directory-p dir)
    (make-directory dir t))
  dir)

(defun gptel-reinforce--directory-empty-p (dir)
  "Return non-nil when DIR has no entries besides dotfiles."
  (null (directory-files dir nil directory-files-no-dot-files-regexp t)))

(defun gptel-reinforce--maybe-migrate-root-dir (legacy-root-dir root-dir)
  "Move LEGACY-ROOT-DIR to ROOT-DIR when migrating artifact storage.
Migration only runs when LEGACY-ROOT-DIR exists and ROOT-DIR is absent or empty."
  (when (and legacy-root-dir
             (file-directory-p legacy-root-dir)
             (not (file-equal-p legacy-root-dir root-dir))
             (or (not (file-exists-p root-dir))
                 (and (file-directory-p root-dir)
                      (gptel-reinforce--directory-empty-p root-dir))))
    (gptel-reinforce--ensure-directory (file-name-directory root-dir))
    (when (file-directory-p root-dir)
      (delete-directory root-dir))
    (rename-file legacy-root-dir root-dir)))

(defun gptel-reinforce--artifact-name (artifact)
  "Return the name of ARTIFACT."
  (if (gptel-reinforce-artifact-p artifact)
      (gptel-reinforce-artifact-name artifact)
    artifact))

(defun gptel-reinforce--database-name (database)
  "Return the name of DATABASE."
  (if (gptel-reinforce-database-p database)
      (gptel-reinforce-database-name database)
    database))

(defun gptel-reinforce--normalize-hook-list (hooks)
  "Normalize HOOKS to a list of functions."
  (cond
   ((null hooks) nil)
   ((functionp hooks) (list hooks))
   ((and (listp hooks) (cl-every #'functionp hooks)) hooks)
   (t (user-error "Hooks must be nil, a function, or a list of functions"))))

(defun gptel-reinforce--keyword->string (key)
  "Convert KEY to a JSON-friendly string."
  (cond
   ((keywordp key) (substring (symbol-name key) 1))
   ((symbolp key) (symbol-name key))
   ((stringp key) key)
   (t (format "%s" key))))

(defun gptel-reinforce--json-normalize (value)
  "Convert VALUE into a structure accepted by `json-encode'."
  (cond
   ((hash-table-p value)
    (let (result)
      (maphash
       (lambda (key val)
         (push (cons (gptel-reinforce--keyword->string key)
                     (gptel-reinforce--json-normalize val))
               result))
       value)
      (nreverse result)))
   ((and (listp value) (keywordp (car-safe value)))
    (let (result)
      (while value
        (let ((key (pop value))
              (val (pop value)))
          (push (cons (gptel-reinforce--keyword->string key)
                      (gptel-reinforce--json-normalize val))
                result)))
      (nreverse result)))
   ((and (listp value) (consp (car-safe value)))
    (mapcar (lambda (pair)
              (cons (gptel-reinforce--keyword->string (car pair))
                    (gptel-reinforce--json-normalize (cdr pair))))
            value))
   ((listp value)
    (mapcar #'gptel-reinforce--json-normalize value))
   ((symbolp value)
    (symbol-name value))
   (t value)))

(defun gptel-reinforce-json-encode (value)
  "Encode VALUE as JSON."
  (json-encode (gptel-reinforce--json-normalize value)))

(defun gptel-reinforce-json-decode (string)
  "Decode STRING as JSON and return an alist."
  (when (and string (not (string-empty-p string)))
    (json-parse-string string :object-type 'alist :array-type 'list)))

(defun gptel-reinforce--alist-to-plist (alist)
  "Convert ALIST to a plist with keyword keys."
  (cl-loop for (key . value) in alist
           append (list (intern (concat ":" (gptel-reinforce--keyword->string key)))
                        value)))

(defun gptel-reinforce--context-plist (context)
  "Normalize CONTEXT to a plist."
  (cond
   ((null context) nil)
   ((and (listp context) (keywordp (car-safe context))) context)
   ((and (listp context) (consp (car-safe context)))
    (gptel-reinforce--alist-to-plist context))
   (t (user-error "Context function must return a plist or alist"))))

(defun gptel-reinforce-get-database (name)
  "Return the registered database NAME."
  (gethash name gptel-reinforce--databases))

(defun gptel-reinforce-get-artifact (name)
  "Return the registered artifact NAME."
  (gethash name gptel-reinforce--artifacts))

(defun gptel-reinforce-list-databases ()
  "Return registered database names."
  (sort (hash-table-keys gptel-reinforce--databases) #'string<))

(defun gptel-reinforce-list-artifacts ()
  "Return registered artifact names."
  (sort (hash-table-keys gptel-reinforce--artifacts) #'string<))

(defun gptel-reinforce-resolve-database (database)
  "Resolve DATABASE name or struct into a database struct."
  (cond
   ((gptel-reinforce-database-p database) database)
   ((stringp database)
    (or (gptel-reinforce-get-database database)
        (user-error "Unknown database: %s" database)))
   (t (user-error "Expected a database name or database object"))))

(defun gptel-reinforce-resolve-artifact (artifact)
  "Resolve ARTIFACT name or struct into an artifact struct."
  (cond
   ((gptel-reinforce-artifact-p artifact) artifact)
   ((stringp artifact)
    (or (gptel-reinforce-get-artifact artifact)
        (user-error "Unknown artifact: %s" artifact)))
   (t (user-error "Expected an artifact name or artifact object"))))

(defun gptel-reinforce-context-for-database (database)
  "Run DATABASE's context function and return the normalized context plist.
Returns nil when the context function returns nil (not in a valid context).
Signals `user-error' when the result is missing the required :item-key."
  (let* ((db (gptel-reinforce-resolve-database database))
         (context-fn (gptel-reinforce-database-context-fn db))
         (context (and context-fn (funcall context-fn))))
    (when context
      (setq context (gptel-reinforce--context-plist context))
      (unless (plist-get context :item-key)
        (user-error "Context for %s did not include :item-key"
                    (gptel-reinforce-database-name db))))
    context))

(defun gptel-reinforce-detect-databases ()
  "Return a list of (DATABASE . CONTEXT) pairs available at point."
  (let (matches)
    (maphash
     (lambda (_name db)
       (condition-case nil
           (when-let* ((context (gptel-reinforce-context-for-database db)))
             (push (cons db context) matches))
         (error nil)))
     gptel-reinforce--databases)
    (nreverse matches)))

(defun gptel-reinforce-resolve-database-and-context (&optional database prompt)
  "Return a (DATABASE . CONTEXT) pair.
If DATABASE is nil, infer it from the current context.  When PROMPT is
non-nil and resolution is ambiguous, ask the user."
  (cond
   (database
    (let ((db (gptel-reinforce-resolve-database database)))
      (cons db (gptel-reinforce-context-for-database db))))
   ((and gptel-reinforce-active-database
         (gptel-reinforce-get-database gptel-reinforce-active-database))
    (let ((db (gptel-reinforce-get-database gptel-reinforce-active-database)))
      (cons db (gptel-reinforce-context-for-database db))))
   (t
    (let ((matches (gptel-reinforce-detect-databases)))
      (pcase matches
        (`nil
         (if prompt
             (let* ((name (completing-read "Database: "
                                           (gptel-reinforce-list-databases)
                                           nil t))
                    (db (gptel-reinforce-resolve-database name)))
               (cons db (gptel-reinforce-context-for-database db)))
           (cons nil nil)))
        (`((,db . ,context)) (cons db context))
        (_
         (if prompt
             (let* ((choices (mapcar (lambda (entry)
                                       (gptel-reinforce-database-name (car entry)))
                                     matches))
                    (name (completing-read "Database: " choices nil t))
                    (db (gptel-reinforce-resolve-database name)))
               (cons db (or (cdr (assoc db matches))
                            (gptel-reinforce-context-for-database db))))
           (car matches))))))))

(defun gptel-reinforce-set-active-database (database)
  "Set the buffer-local default DATABASE."
  (interactive
   (list (completing-read "Active database: "
                          (gptel-reinforce-list-databases) nil t)))
  (setq-local gptel-reinforce-active-database
              (gptel-reinforce-database-name
               (gptel-reinforce-resolve-database database)))
  gptel-reinforce-active-database)

(defun gptel-reinforce-artifact-dir (artifact)
  "Return the root directory for ARTIFACT."
  (let* ((artifact (gptel-reinforce-resolve-artifact artifact))
         (database (gptel-reinforce-resolve-database
                    (gptel-reinforce-artifact-database artifact))))
    (expand-file-name
     (gptel-reinforce-artifact-name artifact)
     (file-name-as-directory (gptel-reinforce-database-root-dir database)))))

(defun gptel-reinforce-artifact-current-file (artifact)
  "Return ARTIFACT's current.org path."
  (expand-file-name "current.org" (gptel-reinforce-artifact-dir artifact)))

(defun gptel-reinforce-artifact-summary-file (artifact)
  "Return ARTIFACT's summary.org path."
  (expand-file-name "summary.org" (gptel-reinforce-artifact-dir artifact)))

(defun gptel-reinforce-database-state-dir (database)
  "Return the var-state directory for DATABASE."
  (let* ((database (gptel-reinforce-resolve-database database))
         (db-path (gptel-reinforce-database-db-path database)))
    (file-name-as-directory (file-name-sans-extension db-path))))

(defun gptel-reinforce-database-history-dir (database)
  "Return the history directory for DATABASE."
  (expand-file-name "history"
                    (gptel-reinforce-database-state-dir database)))

(defun gptel-reinforce-artifact-history-dir (artifact)
  "Return the history directory for ARTIFACT."
  (let* ((artifact (gptel-reinforce-resolve-artifact artifact))
         (database (gptel-reinforce-artifact-database artifact)))
    (expand-file-name
     (gptel-reinforce-artifact-name artifact)
     (gptel-reinforce-database-history-dir database))))

(defun gptel-reinforce-artifact-effective-summarizer-prompt (artifact &optional current-record)
  "Return ARTIFACT's effective summarizer system prompt.
Appends task-specific user guidance when present.
CURRENT-RECORD is the parsed current.org plist; when provided, its
`* Summarizer User Prompt' section takes precedence over the artifact's
registered :summarizer-user-prompt."
  (let* ((artifact (gptel-reinforce-resolve-artifact artifact))
         (system (gptel-reinforce-artifact-summarizer-system-prompt artifact))
         (user (or (plist-get current-record :summarizer-user-prompt)
                   (gptel-reinforce-artifact-summarizer-user-prompt artifact))))
    (if (string-empty-p (or user ""))
        system
      (format "%s\n\nAdditional task-specific guidance:\n%s" system user))))

(defun gptel-reinforce-artifact-effective-updater-prompt (artifact &optional current-record)
  "Return ARTIFACT's effective updater system prompt.
Appends task-specific user guidance when present.
CURRENT-RECORD is the parsed current.org plist; when provided, its
`* Updater User Prompt' section takes precedence over the artifact's
registered :updater-user-prompt."
  (let* ((artifact (gptel-reinforce-resolve-artifact artifact))
         (system (gptel-reinforce-artifact-updater-system-prompt artifact))
         (user (or (plist-get current-record :updater-user-prompt)
                   (gptel-reinforce-artifact-updater-user-prompt artifact))))
    (if (string-empty-p (or user ""))
        system
      (format "%s\n\nAdditional task-specific guidance:\n%s" system user))))

(defun gptel-reinforce-register-database (&rest plist)
  "Register a reinforcement database from PLIST.

Required keys:
  :name       Unique string identifier for this database.
  :context-fn Function called in the user's buffer to describe the current
              item.  Must return a plist with at least :item-key (a stable
              unique string) or nil when not applicable.  Optional plist
              keys: :title, :primary-text, :meta.

Optional keys:
  :db-path        Path to the SQLite file.  Defaults to
                  <state-root>/<name>.sqlite.
  :root-dir       Root directory for artifact org files.  Defaults to
                  <config-root>/<name>/.
  :legacy-root-dir  Old root-dir path to migrate from on startup.

Returns the created `gptel-reinforce-database' struct.
Calling again with the same :name replaces the existing registration."
  (let* ((name (or (plist-get plist :name)
                   (user-error "Database registration requires :name")))
         (context-fn (or (plist-get plist :context-fn)
                         (user-error "Database registration requires :context-fn")))
         (db-path (expand-file-name
                   (or (plist-get plist :db-path)
                       (format "%s.sqlite" name))
                   gptel-reinforce-state-root))
         (root-dir (file-name-as-directory
                    (expand-file-name
                     (or (plist-get plist :root-dir) name)
                     gptel-reinforce-config-root)))
         (legacy-root-dir (when-let* ((legacy (plist-get plist :legacy-root-dir)))
                            (file-name-as-directory
                             (expand-file-name legacy gptel-reinforce-config-root))))
         (database (gptel-reinforce-database-create
                    :name name
                    :context-fn context-fn
                    :db-path db-path
                    :root-dir root-dir)))
    (unless (functionp context-fn)
      (user-error ":context-fn must be a function"))
    (gptel-reinforce--ensure-directory (file-name-directory db-path))
    (gptel-reinforce--maybe-migrate-root-dir legacy-root-dir root-dir)
    (gptel-reinforce--ensure-directory root-dir)
    (puthash name database gptel-reinforce--databases)
    (gptel-reinforce-db-ensure-schema database)
    database))

(defun gptel-reinforce-register-artifact (&rest plist)
  "Register an artifact from PLIST.

Required keys:
  :name      Unique string identifier for this artifact.
  :database  Name of the parent database (must already be registered).

Optional keys:
  :type       Informational type string: \"prompt\", \"code\", \"rules\", etc.
              Passed to the LLM as context.
  :auto-update  When non-nil, skip the diff review step during updates.
                Can also be set per-artifact in current.org (AUTO_UPDATE: t).

  :summarizer-system-prompt  Full system prompt for summarization.
                             Defaults to `gptel-reinforce-default-summarizer-prompt'.
  :summarizer-user-prompt    Task-specific guidance appended to the system
                             prompt.  Defaults to \"\".  Can also be set in
                             current.org under the * Summarizer User Prompt heading.
  :updater-system-prompt     Full system prompt for artifact updates.
                             Defaults to `gptel-reinforce-default-updater-prompt'.
  :updater-user-prompt       Task-specific guidance appended to the system
                             prompt.  Defaults to \"\".  Can also be set in
                             current.org under the * Updater User Prompt heading.

  :pre-update-hook   Function or list of functions called before applying an
                     update.  Signature: (artifact current-record candidate-text).
                     Return nil to reject the candidate.
  :post-update-hook  Function or list of functions called after writing an
                     accepted update.  Signature:
                     (artifact version-ref current-record candidate-text).
                     Errors are surfaced as warnings; all hooks run regardless.

Returns the created `gptel-reinforce-artifact' struct.
Calling again with the same :name replaces the existing registration."
  (let* ((name (or (plist-get plist :name)
                   (user-error "Artifact registration requires :name")))
         (database-name (or (plist-get plist :database)
                            (user-error "Artifact registration requires :database")))
         (database (gptel-reinforce-resolve-database database-name))
         (artifact (gptel-reinforce-artifact-create
                    :name name
                    :database (gptel-reinforce-database-name database)
                    :type (plist-get plist :type)
                    :auto-update (plist-get plist :auto-update)
                    :summarizer-system-prompt
                    (or (plist-get plist :summarizer-system-prompt)
                        gptel-reinforce-default-summarizer-prompt)
                    :summarizer-user-prompt (or (plist-get plist :summarizer-user-prompt) "")
                    :updater-system-prompt
                    (or (plist-get plist :updater-system-prompt)
                        gptel-reinforce-default-updater-prompt)
                    :updater-user-prompt (or (plist-get plist :updater-user-prompt) "")
                    :pre-update-hooks
                    (gptel-reinforce--normalize-hook-list
                     (plist-get plist :pre-update-hook))
                    :post-update-hooks
                    (gptel-reinforce--normalize-hook-list
                     (plist-get plist :post-update-hook)))))
    (puthash name artifact gptel-reinforce--artifacts)
    (gptel-reinforce-org-initialize-artifact artifact)
    artifact))

(defun gptel-reinforce-current-artifact-version-ref (artifact)
  "Return ARTIFACT's current version reference."
  (plist-get (gptel-reinforce-org-read-current artifact) :version-ref))

(defun gptel-reinforce--output-provenance (artifact &optional output-id)
  "Build provenance plist for ARTIFACT using OUTPUT-ID."
  (let* ((artifact (gptel-reinforce-resolve-artifact artifact))
         (artifact-name (gptel-reinforce-artifact-name artifact))
         (database (gptel-reinforce-artifact-database artifact)))
    (list :output-id (or output-id (format "%s-%s" artifact-name (float-time)))
          :artifact-name artifact-name
          :artifact-version-ref (gptel-reinforce-current-artifact-version-ref artifact)
          :database database)))

(defun gptel-reinforce-track-output-region (artifact start end &optional output-id)
  "Annotate the region START..END as output produced by ARTIFACT.
Adds text properties recording the artifact name, current version reference,
and database.  After annotation, `gptel-reinforce-like' and friends at that
region automatically record output-feedback rather than item-feedback.
Also sets `gptel-reinforce-last-output-provenance' as a buffer-local fallback.

OUTPUT-ID is an optional opaque string; one is generated if omitted.
Returns the provenance plist."
  (let* ((provenance (gptel-reinforce--output-provenance artifact output-id))
         (database (plist-get provenance :database)))
    (add-text-properties
     start end
     (list 'gptel-reinforce-output-id (plist-get provenance :output-id)
           'gptel-reinforce-artifact-name (plist-get provenance :artifact-name)
           'gptel-reinforce-artifact-version-ref (plist-get provenance :artifact-version-ref)
           'gptel-reinforce-database database))
    (setq-local gptel-reinforce-last-output-provenance provenance)
    provenance))

(defun gptel-reinforce-propertize-output (artifact text &optional output-id)
  "Return a copy of TEXT annotated with output provenance for ARTIFACT.
Like `gptel-reinforce-track-output-region' but for text not yet inserted
into a buffer.  The returned string carries text properties recording the
artifact name, version, and database.  Pass the result to
`gptel-reinforce-like' and friends to record output-feedback."
  (let ((copy (copy-sequence text))
        (provenance (gptel-reinforce--output-provenance artifact output-id)))
    (add-text-properties
     0 (length copy)
     (list 'gptel-reinforce-output-id (plist-get provenance :output-id)
           'gptel-reinforce-artifact-name (plist-get provenance :artifact-name)
           'gptel-reinforce-artifact-version-ref (plist-get provenance :artifact-version-ref)
           'gptel-reinforce-database (plist-get provenance :database))
     copy)
    copy))

(defun gptel-reinforce-output-at-point ()
  "Return output provenance from point or the latest buffer-local output."
  (or (let ((output-id (get-text-property (point) 'gptel-reinforce-output-id)))
        (when output-id
          (list :output-id output-id
                :artifact-name (get-text-property (point) 'gptel-reinforce-artifact-name)
                :artifact-version-ref (get-text-property (point) 'gptel-reinforce-artifact-version-ref)
                :database (get-text-property (point) 'gptel-reinforce-database))))
      gptel-reinforce-last-output-provenance))

(provide 'gptel-reinforce-core)

;;; gptel-reinforce-core.el ends here
