;;; gptel-reinforce-db.el --- SQLite storage for gptel-reinforce -*- lexical-binding: t; -*-

;;; Commentary:

;; SQLite persistence for append-only feedback events.

;;; Code:

(require 'cl-lib)
(unless (require 'sqlite nil t)
  (error "gptel-reinforce requires Emacs with SQLite support"))
(require 'gptel-reinforce-core)

(defmacro gptel-reinforce-db-with-connection (database &rest body)
  "Run BODY with a SQLite connection for DATABASE."
  (declare (indent 1) (debug t))
  `(let ((connection (gptel-reinforce-db--open ,database)))
     (unwind-protect
         (progn ,@body)
       (sqlite-close connection))))

(defun gptel-reinforce-db--open (database)
  "Open DATABASE's SQLite file and ensure the schema exists."
  (let* ((database (gptel-reinforce-resolve-database database))
         (db-path (gptel-reinforce-database-db-path database)))
    (gptel-reinforce--ensure-directory (file-name-directory db-path))
    (let ((connection (sqlite-open db-path))
          (ok nil))
      (unwind-protect
          (progn
            (gptel-reinforce-db--initialize connection)
            (setq ok t)
            connection)
        (unless ok
          (sqlite-close connection))))))

(defun gptel-reinforce-db--initialize (connection)
  "Create the required tables in CONNECTION."
  (sqlite-execute connection
                  "CREATE TABLE IF NOT EXISTS feedback_events (
                     id INTEGER PRIMARY KEY,
                     created_at TEXT NOT NULL,
                     event_type TEXT NOT NULL,
                     item_key TEXT,
                     score REAL NOT NULL,
                     title TEXT,
                     primary_text TEXT,
                     meta_json TEXT,
                     note TEXT,
                     artifact_name TEXT,
                     artifact_version_ref TEXT,
                     output_id TEXT
                   )")
  (sqlite-execute connection
                  "CREATE INDEX IF NOT EXISTS feedback_events_event_type_idx
                   ON feedback_events(event_type)")
  (sqlite-execute connection
                  "CREATE INDEX IF NOT EXISTS feedback_events_artifact_name_idx
                   ON feedback_events(artifact_name)")
  connection)

(defun gptel-reinforce-db-ensure-schema (database)
  "Ensure DATABASE's SQLite file exists and has the expected schema."
  (gptel-reinforce-db-with-connection database t))

(defun gptel-reinforce-db--last-insert-id (connection)
  "Return the last inserted row id from CONNECTION."
  (caar (sqlite-select connection "SELECT last_insert_rowid()")))

(defun gptel-reinforce-db-record-feedback (database event)
  "Insert feedback EVENT into DATABASE and return the new row id.

EVENT is a plist with these keys:
  :event-type   Required.  \"item-feedback\" or \"output-feedback\".
  :score        Required.  Numeric score (e.g. -1, 0, 1, or any float).
  :item-key     Stable unique identifier from the context function.
  :title        Human-readable label for the item.
  :primary-text Content excerpt for the item.
  :meta         Plist of additional metadata; JSON-encoded for storage.
  :note         Optional free-text note from the user.
  :artifact-name      Artifact name (output-feedback only).
  :artifact-version-ref  History filename of the producing artifact version.
  :output-id    Opaque ID linking this event to a specific output region.
  :created-at   ISO-8601 timestamp; defaults to now."
  (gptel-reinforce-db-with-connection database
    (sqlite-execute
     connection
     "INSERT INTO feedback_events
      (created_at, event_type, item_key, score, title, primary_text, meta_json,
       note, artifact_name, artifact_version_ref, output_id)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     (list (or (plist-get event :created-at) (gptel-reinforce--timestamp))
           (or (plist-get event :event-type)
               (user-error "Feedback event missing :event-type"))
           (plist-get event :item-key)
           (or (plist-get event :score)
               (user-error "Feedback event missing :score"))
           (plist-get event :title)
           (plist-get event :primary-text)
           (when-let* ((meta (plist-get event :meta)))
             (gptel-reinforce-json-encode meta))
           (plist-get event :note)
           (plist-get event :artifact-name)
           (plist-get event :artifact-version-ref)
           (plist-get event :output-id)))
    (gptel-reinforce-db--last-insert-id connection)))

(defun gptel-reinforce-db-feedback-since (database last-event-id artifact-name)
  "Return feedback events in DATABASE newer than LAST-EVENT-ID for ARTIFACT-NAME.
Returns a list of event plists (see `gptel-reinforce-db-record-feedback').

All item-feedback events in the database are included (they are database-
scoped, so every artifact in the database shares item signal).
Only output-feedback events whose artifact_name matches ARTIFACT-NAME are
included (output-feedback is artifact-scoped).

LAST-EVENT-ID is the LAST_EVENT_ID from summary.org; pass 0 to read all events."
  (gptel-reinforce-db-with-connection database
    (mapcar
       (lambda (row)
       (pcase-let ((`(,id ,created-at ,event-type ,item-key ,score ,title ,primary-text
                      ,meta-json ,note ,artifact-name-row ,artifact-version-ref ,output-id)
                    row))
         (list :id id
               :created-at created-at
               :event-type event-type
               :item-key item-key
               :score score
               :title title
               :primary-text primary-text
               :meta (gptel-reinforce-json-decode meta-json)
               :note note
               :artifact-name artifact-name-row
               :artifact-version-ref artifact-version-ref
               :output-id output-id)))
     (sqlite-select
      connection
      "SELECT id, created_at, event_type, item_key, score, title, primary_text,
              meta_json, note, artifact_name, artifact_version_ref, output_id
       FROM feedback_events
       WHERE id > ?
         AND (event_type = 'item-feedback'
              OR (event_type = 'output-feedback' AND artifact_name = ?))
       ORDER BY id ASC"
      (list (or last-event-id 0) artifact-name)))))

(defun gptel-reinforce-db-feedback-event-count (database)
  "Return the number of feedback events stored in DATABASE."
  (gptel-reinforce-db-with-connection database
    (caar (sqlite-select connection "SELECT COUNT(*) FROM feedback_events"))))

(provide 'gptel-reinforce-db)

;;; gptel-reinforce-db.el ends here
