;;; gptel-reinforce-backend.el --- Backend abstraction for gptel-reinforce -*- lexical-binding: t; -*-

;;; Commentary:

;; Backend request objects and the default gptel transport.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel-reinforce-core)

(declare-function gptel-request "gptel" (prompt &rest args))

(cl-defstruct (gptel-reinforce-summary-request
               (:constructor gptel-reinforce-summary-request-create))
  database-name
  artifact-name
  artifact-type
  existing-summary
  events
  system-prompt)

(cl-defstruct (gptel-reinforce-update-request
               (:constructor gptel-reinforce-update-request-create))
  database-name
  artifact-name
  artifact-type
  current-text
  summary-body
  system-prompt)

(defcustom gptel-reinforce-backend-function
  #'gptel-reinforce-backend-send-with-gptel
  "Function used to execute summarize and update requests."
  :group 'gptel-reinforce
  :type 'function)

(defun gptel-reinforce-backend-send (request callback)
  "Send REQUEST to the active backend and invoke CALLBACK with the response."
  (funcall gptel-reinforce-backend-function request callback))

(defun gptel-reinforce-backend--event-block (event)
  "Render EVENT as text for prompt construction."
  (let ((lines
         (list (format "- id: %s" (plist-get event :id))
               (format "  created_at: %s" (plist-get event :created-at))
               (format "  event_type: %s" (plist-get event :event-type))
               (format "  score: %s" (plist-get event :score)))))
    (dolist (field '((:item-key . "item_key")
                     (:title . "title")
                     (:primary-text . "primary_text")
                     (:artifact-name . "artifact_name")
                     (:artifact-version-ref . "artifact_version_ref")
                     (:output-id . "output_id")
                     (:note . "note")))
      (when-let* ((value (plist-get event (car field))))
        (push (format "  %s: %s" (cdr field) value) lines)))
    (when-let* ((meta (plist-get event :meta)))
      (push (format "  meta: %s" (gptel-reinforce-json-encode meta)) lines))
    (string-join (nreverse lines) "\n")))

(defun gptel-reinforce-backend--summary-user-prompt (request)
  "Build a user prompt string for summary REQUEST."
  (format
   (string-join
    '("Database: %s"
      "Artifact: %s"
      "Artifact type: %s"
      ""
      "Existing summary:"
      "%s"
      ""
      "New feedback events:"
      "%s"
      ""
      "Update the summary conservatively and keep it useful for future edits.")
    "\n")
   (gptel-reinforce-summary-request-database-name request)
   (gptel-reinforce-summary-request-artifact-name request)
   (or (gptel-reinforce-summary-request-artifact-type request) "")
   (or (gptel-reinforce-summary-request-existing-summary request) "")
   (mapconcat #'gptel-reinforce-backend--event-block
              (gptel-reinforce-summary-request-events request)
              "\n\n")))

(defun gptel-reinforce-backend--update-user-prompt (request)
  "Build a user prompt string for update REQUEST."
  (format
   (string-join
    '("Database: %s"
      "Artifact: %s"
      "Artifact type: %s"
      ""
      "Current artifact text:"
      "%s"
      ""
      "Feedback summary:"
      "%s"
      ""
      "Revise the artifact with the smallest useful change.")
    "\n")
   (gptel-reinforce-update-request-database-name request)
   (gptel-reinforce-update-request-artifact-name request)
   (or (gptel-reinforce-update-request-artifact-type request) "")
   (or (gptel-reinforce-update-request-current-text request) "")
   (or (gptel-reinforce-update-request-summary-body request) "")))

(defun gptel-reinforce-backend--sanitize-response (response)
  "Trim RESPONSE and drop simple Markdown fences."
  (let ((text (string-trim (or response ""))))
    (if (string-match
         "\\````[[:alpha:]-]*\n\\([[:ascii:][:nonascii:]\n]*?\\)\n```\\'"
         text)
        (string-trim (match-string 1 text))
      text)))

(defun gptel-reinforce-backend-send-with-gptel (request callback)
  "Send REQUEST through gptel and invoke CALLBACK with the response."
  (require 'gptel)
  (let ((prompt (cond
                 ((gptel-reinforce-summary-request-p request)
                  (gptel-reinforce-backend--summary-user-prompt request))
                 ((gptel-reinforce-update-request-p request)
                  (gptel-reinforce-backend--update-user-prompt request))
                 (t
                  (user-error "Unsupported request type: %S" request))))
        (system (cond
                 ((gptel-reinforce-summary-request-p request)
                  (gptel-reinforce-summary-request-system-prompt request))
                 ((gptel-reinforce-update-request-p request)
                  (gptel-reinforce-update-request-system-prompt request))
                 (t nil))))
    (gptel-request
     prompt
     :system system
     :stream nil
     :callback
     (lambda (response info)
       (if (not response)
           (message "gptel-reinforce backend error: %s" (plist-get info :status))
         (funcall callback
                  (gptel-reinforce-backend--sanitize-response response)
                  info))))))

(provide 'gptel-reinforce-backend)

;;; gptel-reinforce-backend.el ends here
