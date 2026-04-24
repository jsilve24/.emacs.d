;;; anki.el --- JDS anki config -*- lexical-binding: t -*-

(use-package anki-editor
  :straight (:repo "anki-editor/anki-editor")
  :defer t
  ;; :after org
  :commands (anki-editor-mode
	     anki-editor-push-notes
	     anki-editor-push-note-at-point
	     anki-editor-push-tree
	     anki-editor-cloze-dwim
	     anki-editor-cloze-region-dont-incr
	     anki-editor-cloze-region-auto-incr
	     anki-editor-reset-cloze-number
	     anki-editor-delete-notes
	     anki-editor-insert-note)
  :hook (org-mode . jds/anki-editor-maybe-enable)
  :init
  ;; Change this if AnkiConnect is listening elsewhere.
  (setq anki-editor-api-host "127.0.0.1"
	anki-editor-api-port "8765")
  (defvar jds/anki-startup-poll-interval 1
    "Seconds between AnkiConnect readiness checks.")
  (defvar jds/anki-startup-timeout 30
    "Maximum seconds to wait for AnkiConnect after launching Anki.")
  (defvar jds/anki-background-retry-interval 15
    "Seconds between background retries after startup polling times out.")
  (defvar jds/anki-startup-process nil
    "The most recent background process used to launch Anki.")
  (defvar-local jds/anki-startup-timer nil
    "Timer used to retry enabling `anki-editor-mode' in the current buffer.")

  (defun jds/anki-connect-running-p ()
    "Return non-nil when AnkiConnect is reachable."
    (condition-case nil
	(let ((proc (make-network-process
		     :name "jds-anki-connect-probe"
		     :buffer nil
		     :host anki-editor-api-host
		     :service anki-editor-api-port
		     :nowait nil
		     :noquery t
		     :coding 'no-conversion)))
	  (delete-process proc)
	  t)
      (error nil)))

  (defun jds/start-anki ()
    "Start the Anki application unless it is already launching."
    (unless (jds/anki-connect-running-p)
      (let ((anki-executable (executable-find "anki")))
	(unless anki-executable
	  (user-error "Cannot start Anki automatically because `anki` was not found"))
	(unless (and jds/anki-startup-process
		     (process-live-p jds/anki-startup-process))
	  (setq jds/anki-startup-process
		(start-process "anki" nil anki-executable))
	  (set-process-query-on-exit-flag jds/anki-startup-process nil)
	  (message "Starting Anki...")))))

  (defun jds/anki-editor-enable-when-ready (buffer deadline)
    "Enable `anki-editor-mode' in BUFFER before DEADLINE when possible."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(when jds/anki-startup-timer
	  (cancel-timer jds/anki-startup-timer)
	  (setq jds/anki-startup-timer nil))
	(cond
	 ((not (derived-mode-p 'org-mode))
	  nil)
	 ((jds/anki-connect-running-p)
	  (unless anki-editor-mode
	    (anki-editor-mode 1)))
	 ((and deadline
	       (time-less-p deadline (current-time)))
	  (message "Timed out waiting for AnkiConnect after starting Anki; continuing background retries")
	  (setq jds/anki-startup-timer
		(run-with-timer
		 jds/anki-background-retry-interval
		 nil
		 #'jds/anki-editor-enable-when-ready
		 buffer
		 nil)))
	 (t
	  (setq jds/anki-startup-timer
		(run-with-timer
		 (if deadline
		     jds/anki-startup-poll-interval
		   jds/anki-background-retry-interval)
		 nil
		 #'jds/anki-editor-enable-when-ready
		 buffer
		 deadline)))))))
  ;; Store media generated from org exports here before pushing.
  ;; Optional, but nice to keep explicit.
  (setq anki-editor-create-decks t)
  ;; LaTeX fragments are common for your use case.
  ;; This makes it easy to preview math before pushing.
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq anki-editor-latex-style 'mathjax)
  ;; (setq anki-editor-latex-style 'builtin)


  ;; A small convenience: default tags used by card files.
  (defvar jds/anki-file-mode-map (make-sparse-keymap)
    "Keymap for `jds/anki-file-mode'.")

  (define-minor-mode jds/anki-file-mode
    "Minor mode for Org files that belong to the Anki workflow."
    :keymap jds/anki-file-mode-map
    :lighter nil)

  (defun jds/anki-editor-maybe-enable ()
    (let ((anki-root (expand-file-name "~/Dropbox/org/anki/")))
      (when (and buffer-file-name
		 (file-in-directory-p buffer-file-name anki-root))
	(jds/anki-file-mode 1)
	(if (jds/anki-connect-running-p)
	    (anki-editor-mode 1)
	  (jds/start-anki)
	  (when jds/anki-startup-timer
	    (cancel-timer jds/anki-startup-timer))
	  (setq-local jds/anki-startup-timer
		      (run-with-timer
		       0
		       nil
		       #'jds/anki-editor-enable-when-ready
		       (current-buffer)
		       (time-add (current-time)
				 (seconds-to-time jds/anki-startup-timeout)))))))))


(jds/localleader-def
  :keymaps '(jds/anki-file-mode-map anki-editor-mode-map)
  "m" '(:ignore t :which-key "anki")
  "mp" #'anki-editor-push-notes
  "mP" #'anki-editor-push-note-at-point
  "mr" #'jds/anki-editor-clear-stale-duplicate-failures
  "mR" #'jds/anki-editor-repair-duplicate-note
  "mc" #'anki-editor-cloze-dwim
  "md" #'anki-editor-delete-notes
  "mi" #'anki-editor-insert-note)

;;; custom repair function for duplicates

(require 'json)
(require 'subr-x)
(require 'org)
(require 'url)
(require 'url-http)

;;;###autoload
(defun jds/anki-editor--interactive-scope ()
  "Return the interactive scope used by `anki-editor' push commands."
  (cond
   ((region-active-p) 'region)
   ((equal current-prefix-arg '(4)) 'tree)
   ((equal current-prefix-arg '(16)) 'file)
   ((equal current-prefix-arg '(64)) 'agenda)
   (t nil)))

;;;###autoload
(defun jds/anki-editor-clear-stale-duplicate-failures (&optional scope)
  "Clear stale duplicate failures for notes that already exist in Anki.

Only headings whose `ANKI_FAILURE_REASON' contains:

  cannot create note because it is a duplicate

and that also have a non-empty `ANKI_NOTE_ID' are considered.

Each candidate note ID is checked with AnkiConnect first.  Failure
reasons are cleared only for note IDs that still exist in Anki."
  (interactive (list (jds/anki-editor--interactive-scope)))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (let ((candidates nil)
        (modified-buffers nil)
        (cleared 0)
        (missing 0))
    (anki-editor-map-note-entries
     (lambda ()
       (let ((reason (org-entry-get nil "ANKI_FAILURE_REASON"))
             (note-id (org-entry-get nil "ANKI_NOTE_ID")))
         (when (and reason
                    (string-match-p
                     "cannot create note because it is a duplicate"
                     reason)
                    note-id
                    (not (string-blank-p note-id)))
           (push (cons (string-to-number note-id) (point-marker))
                 candidates))))
     nil
     scope)
    (setq candidates (nreverse candidates))
    (when candidates
      (let* ((response
              (js/anki-connect-request
               "notesInfo"
               `((notes . ,(mapcar #'car candidates)))))
             (err (alist-get 'error response))
             (result (alist-get 'result response)))
        (when err
          (user-error "AnkiConnect error while checking note IDs: %s" err))
        (cl-loop for (_note-id . marker) in candidates
                 for info in result
                 do
                 (when (and marker (marker-buffer marker))
                   (with-current-buffer (marker-buffer marker)
                     (save-excursion
                       (goto-char marker)
                       (if info
                           (when (org-entry-get nil "ANKI_FAILURE_REASON")
                             (org-entry-delete nil "ANKI_FAILURE_REASON")
                             (cl-incf cleared)
                             (cl-pushnew (current-buffer) modified-buffers))
                         (cl-incf missing))))
                   (set-marker marker nil)))))
    (dolist (buffer modified-buffers)
      (with-current-buffer buffer
        (save-buffer)))
    (message
     "Cleared %d stale duplicate failure(s)%s"
     cleared
     (if (zerop missing)
         ""
       (format "; %d note ID(s) were missing in Anki" missing)))))

;;;###autoload
(defun js/anki-editor--nearest-duplicate-failure-heading ()
  "Return point of nearest ancestor heading with duplicate ANKI_FAILURE_REASON."
  (save-excursion
    (org-back-to-heading t)
    (let ((found nil)
          (keep-going t))
      (while (and keep-going (not found))
        (let ((reason (org-entry-get nil "ANKI_FAILURE_REASON")))
          (when (and reason
                     (string-match-p
                      "cannot create note because it is a duplicate"
                      reason))
            (setq found (point))))
        (unless found
          (setq keep-going (org-up-heading-safe))))
      found)))

;;;###autoload
(defun js/anki-connect-request (action params)
  "Send ACTION with PARAMS to AnkiConnect and return parsed JSON alist."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode
           `((action . ,action)
             (version . 6)
             (params . ,params))))
         (buf
          (url-retrieve-synchronously "http://localhost:8765" t t 5)))
    (unless buf
      (user-error "Could not contact AnkiConnect at localhost:8765"))
    (unwind-protect
        (with-current-buffer buf
          ;; Skip HTTP headers.
          (goto-char (point-min))
          (unless (search-forward "\n\n" nil t)
            (user-error "Malformed response from AnkiConnect"))
          (let* ((json-array-type 'list)
                 (json-object-type 'alist)
                 (json-key-type 'symbol)
                 (body (string-trim
                        (buffer-substring-no-properties
                         (point)
                         (point-max)))))
            (condition-case err
                (json-read-from-string body)
              (error
               (user-error "Could not parse AnkiConnect JSON response: %S\nRaw body: %s"
                           err body)))))
      (kill-buffer buf))))

;;;###autoload
(defun jds/anki-editor-repair-duplicate-note (&optional search-string)
  "Repair an anki-editor duplicate failure using region or prompt text.

If region is active, use it as the exact Anki search string.
Otherwise prompt for the search string.

Search upward from point for an Org heading whose ANKI_FAILURE_REASON
contains:

  cannot create note because it is a duplicate

If exactly one Anki note is found, remove ANKI_FAILURE_REASON, add
ANKI_NOTE_ID, then run `anki-editor-push-note-at-point`.

If zero or multiple notes are found, warn and do not modify anything."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))

  (unless (fboundp 'anki-editor-push-note-at-point)
    (user-error "anki-editor-push-note-at-point is not available"))

  (let* ((selected-or-prompted
          (or search-string
              (if (use-region-p)
                  (buffer-substring-no-properties
                   (region-beginning)
                   (region-end))
                (read-string "Anki search string: "))))
         (selected
          (string-trim selected-or-prompted))
         (note-heading
          (js/anki-editor--nearest-duplicate-failure-heading)))

    (when (string-empty-p selected)
      (user-error "Search string is empty"))

    (unless note-heading
      (user-error "No duplicate ANKI_FAILURE_REASON found on this heading or its ancestors"))

    (let* (;; Anki search syntax wants the exact string surrounded by quotes.
           ;; We only need to escape internal quotes for Anki's query language.
           (anki-query-string
            (concat "\""
                    (replace-regexp-in-string "\"" "\\\\\"" selected)
                    "\""))
           (parsed
            (js/anki-connect-request
             "findNotes"
             `((query . ,anki-query-string))))
           (err
            (alist-get 'error parsed))
           (result
            (alist-get 'result parsed)))

      (cond
       (err
        (display-warning
         'anki-editor
         (format "AnkiConnect error: %s" err)
         :warning)
        nil)

       ((null result)
        (display-warning
         'anki-editor
         (format "No Anki note found for search string: %S" selected)
         :warning)
        nil)

       ((not (= (length result) 1))
        (display-warning
         'anki-editor
         (format "Multiple Anki notes found; refusing to modify Org note. IDs: %S" result)
         :warning)
        nil)

       (t
        (let ((note-id (number-to-string (car result))))
          (save-excursion
            (goto-char note-heading)
            (org-entry-delete nil "ANKI_FAILURE_REASON")
            (org-entry-put nil "ANKI_NOTE_ID" note-id)
            (anki-editor-push-note-at-point))
          (message "Repaired anki-editor note with ANKI_NOTE_ID: %s" note-id)))))))

;;;###autoload
(defun jds/anki-editor-save-current-buffer-after-push (orig-fn &rest args)
  "Run ORIG-FN with ARGS and save the current buffer if it changed.

This preserves cleared `ANKI_FAILURE_REASON' properties for skipped
notes, which `anki-editor-push-notes' otherwise leaves only in memory."
  (let ((buffer (current-buffer)))
    (prog1
        (apply orig-fn args)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and buffer-file-name
                     (buffer-modified-p))
            (save-buffer)))))))

(advice-add 'anki-editor-push-notes :around
            #'jds/anki-editor-save-current-buffer-after-push)

;; fix basic cards

;;;###autoload
(defun jds/anki-basic-back-extra-to-extra ()
  "For Basic anki-editor notes, convert a Back Extra field heading to plain text.

Example:

*** Back Extra
Source: ...

becomes:

Extra:
Source: ...

This prevents Basic notes from having a Back Extra field."
  (interactive)
  (let (hits)
    (org-with-wide-buffer
     (org-map-entries
      (lambda ()
        (when (string= (org-entry-get nil "ANKI_NOTE_TYPE") "Basic")
          (let* ((note-level (org-current-level))
                 (field-level (1+ note-level))
                 (note-end (save-excursion
                             (org-end-of-subtree t t)))
                 (field-re (format "^\\*\\{%d\\}[ \t]+Back Extra[ \t]*$"
                                   field-level)))
            (save-excursion
              (forward-line 1)
              (while (re-search-forward field-re note-end t)
                (push (match-beginning 0) hits)))))))
      nil 'file)

     ;; Edit from bottom to top so positions stay valid.
     (dolist (pos (sort hits #'>))
       (goto-char pos)
       (delete-region (line-beginning-position) (line-end-position))
       (insert "Extra:")))

    (message "Converted %d Basic note Back Extra field(s)." (length hits)))
