;;; calendar-exchange.el --- href-aware Org/Exchange calendar sync -*- lexical-binding: t; -*-

;; DavMail/Exchange does not preserve the client-selected CalDAV resource
;; name.  org-caldav assumes that resource name, iCalendar UID, and Org ID are
;; identical, so its sync engine cannot safely drive this calendar.  This
;; module keeps those identities separate and uses org-caldav only for its
;; iCalendar conversion helpers.

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-id)
(require 'org-caldav)
(require 'subr-x)
(require 'url-dav)
(require 'url-http)
(require 'url-util)

(defvar url-http-response-status)
(declare-function jds/calendar--install-davmail-auth "calendar")

(defgroup jds/exchange-calendar nil
  "Synchronize one Org file with an Exchange calendar through DavMail."
  :group 'calendar)

(defcustom jds/exchange-calendar-file
  (expand-file-name "~/Dropbox/org/calendar.org")
  "Org file synchronized with the dedicated Exchange calendar."
  :type 'file
  :group 'jds/exchange-calendar)

(defcustom jds/exchange-calendar-url
  "http://127.0.0.1:1080/users/jds6696@psu.edu/calendar/EmacsSync/"
  "DavMail URL for the dedicated Exchange calendar collection."
  :type 'string
  :group 'jds/exchange-calendar)

(defcustom jds/exchange-calendar-state-file
  (expand-file-name "var/calendar-exchange/state.sexp" user-emacs-directory)
  "Local synchronization state.

This file contains data only and is read with `read', never evaluated."
  :type 'file
  :group 'jds/exchange-calendar)

(defcustom jds/exchange-calendar-timezone "Eastern Standard Time"
  "Timezone identifier sent through DavMail to Exchange EWS."
  :type 'string
  :group 'jds/exchange-calendar)

(defcustom jds/exchange-calendar-allow-full-sync nil
  "When non-nil, allow `jds/exchange-calendar-sync'.

Keep this nil until single-entry create, no-op, update, import, and deletion
tests have succeeded."
  :type 'boolean
  :group 'jds/exchange-calendar)

(defconst jds/exchange-calendar--state-version 1)
(defconst jds/exchange-calendar--href-property "EXCHANGE_HREF")
(defconst jds/exchange-calendar--etag-property "EXCHANGE_ETAG")
(defconst jds/exchange-calendar--remote-uid-property "EXCHANGE_REMOTE_UID")

(defun jds/exchange-calendar--legacy-sync-disabled (&rest _ignored)
  "Prevent use of org-caldav's incompatible synchronization engine."
  (interactive)
  (user-error
   "org-caldav sync is disabled for EmacsSync; use jds/exchange-calendar-dry-run"))

(unless (advice-member-p #'jds/exchange-calendar--legacy-sync-disabled
                         #'org-caldav-sync)
  (advice-add 'org-caldav-sync :override
              #'jds/exchange-calendar--legacy-sync-disabled))

(defun jds/exchange-calendar--install-auth ()
  "Install DavMail credentials without persisting plaintext."
  (unless (fboundp 'jds/calendar--install-davmail-auth)
    (user-error "DavMail authentication helper is not loaded"))
  (jds/calendar--install-davmail-auth))

(defun jds/exchange-calendar--header (name header-end)
  "Return HTTP header NAME before HEADER-END in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward
             (concat "^" (regexp-quote name) ":[ \t]*\\([^\r\n]+\\)")
             header-end t)
        (string-trim (match-string-no-properties 1))))))

(cl-defun jds/exchange-calendar--request
    (method url &key data headers acceptable-statuses)
  "Send METHOD to URL and return a response plist.

DATA and HEADERS become `url-request-data' and
`url-request-extra-headers'.  ACCEPTABLE-STATUSES defaults to all 2xx
responses.  The returned plist contains :status, :headers, :body, :location,
and :etag."
  (jds/exchange-calendar--install-auth)
  (let* ((url-request-method method)
         (url-request-data data)
         (url-request-extra-headers headers)
         (url-show-status nil)
         (buffer (url-retrieve-synchronously url t t 30)))
    (unless buffer
      (error "DavMail returned no response for %s %s" method url))
    (unwind-protect
        (with-current-buffer buffer
          (let* ((status url-http-response-status)
                 (header-end (or (and (boundp 'url-http-end-of-headers)
                                      url-http-end-of-headers)
                                 (save-excursion
                                   (goto-char (point-min))
                                   (when (re-search-forward "\r?\n\r?\n" nil t)
                                     (point)))))
                 (allowed (or (and acceptable-statuses
                                   (memq status acceptable-statuses))
                              (and (null acceptable-statuses)
                                   (integerp status)
                                   (<= 200 status) (< status 300)))))
            (unless allowed
              (error "DavMail returned HTTP %s for %s %s"
                     (or status "unknown") method url))
            (list :status status
                  :headers (buffer-substring-no-properties
                            (point-min) (or header-end (point-min)))
                  :body (buffer-substring-no-properties
                         (or header-end (point-min)) (point-max))
                  :location (and header-end
                                 (jds/exchange-calendar--header
                                  "Location" header-end))
                  :etag (and header-end
                             (jds/exchange-calendar--header
                              "ETag" header-end)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun jds/exchange-calendar--absolute-href (href)
  "Return HREF as an absolute URL below the configured collection."
  (cond
   ((string-match-p "\\`https?://" href) href)
   ((string-prefix-p "/" href)
    (let ((urlobj (url-generic-parse-url jds/exchange-calendar-url)))
      (format "%s://%s%s"
              (url-type urlobj)
              (url-host urlobj)
              (if (url-port urlobj)
                  (format ":%d%s" (url-port urlobj) href)
                href))))
   (t (url-expand-file-name href jds/exchange-calendar-url))))

(defun jds/exchange-calendar--remote-inventory ()
  "Return remote resources as plists with :href and :etag.

The collection request is read-only."
  (jds/exchange-calendar--install-auth)
  (let* ((request-data
          "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n<DAV:propfind xmlns:DAV=\"DAV:\"><DAV:prop><DAV:getetag/></DAV:prop></DAV:propfind>\n")
         (url-request-method "PROPFIND")
         (url-request-data request-data)
         (url-request-extra-headers
          '(("Depth" . "1") ("Content-type" . "text/xml")))
         (url-show-status nil)
         (buffer (url-retrieve-synchronously
                  jds/exchange-calendar-url t t 30)))
    (unless buffer
      (error "DavMail returned no response to PROPFIND"))
    (unwind-protect
        (with-current-buffer buffer
          (unless (and (integerp url-http-response-status)
                       (<= 200 url-http-response-status)
                       (< url-http-response-status 300))
            (error "DavMail returned HTTP %s to PROPFIND"
                   url-http-response-status))
          (let ((properties
                 (url-dav-process-response buffer jds/exchange-calendar-url))
                resources)
            (dolist (property properties (nreverse resources))
              (let ((href (car property))
                    (etag (plist-get (cdr property) 'DAV:getetag)))
                (when (and (stringp href)
                           (string-match-p "\\.EML/?\\'" href)
                           (stringp etag))
                  (push (list :href (jds/exchange-calendar--absolute-href href)
                              :etag (string-trim etag "\\\"" "\\\""))
                        resources))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun jds/exchange-calendar--state-empty ()
  "Return a new empty state plist."
  (list :version jds/exchange-calendar--state-version
        :calendar-url jds/exchange-calendar-url
        :entries nil))

(defun jds/exchange-calendar--load-state ()
  "Read and validate synchronization state."
  (if (not (file-exists-p jds/exchange-calendar-state-file))
      (jds/exchange-calendar--state-empty)
    (with-temp-buffer
      (insert-file-contents jds/exchange-calendar-state-file)
      (let ((state (read (current-buffer))))
        (unless (and (listp state)
                     (equal (plist-get state :version)
                            jds/exchange-calendar--state-version)
                     (equal (plist-get state :calendar-url)
                            jds/exchange-calendar-url)
                     (listp (plist-get state :entries)))
          (error "Invalid or mismatched Exchange calendar state: %s"
                 jds/exchange-calendar-state-file))
        state))))

(defun jds/exchange-calendar--save-state (state)
  "Atomically write STATE as non-executable Lisp data."
  (let ((directory (file-name-directory jds/exchange-calendar-state-file)))
    (make-directory directory t)
    (let ((temporary (make-temp-file
                      (expand-file-name ".state-" directory))))
      (unwind-protect
          (progn
            (with-temp-file temporary
              (let ((print-length nil)
                    (print-level nil))
                (insert ";; calendar-exchange data; read as data, never eval\n")
                (pp state (current-buffer))))
            (set-file-modes temporary #o600)
            (rename-file temporary jds/exchange-calendar-state-file t))
        (when (file-exists-p temporary)
          (delete-file temporary))))))

(defun jds/exchange-calendar--state-entry (state local-id)
  "Return STATE entry for LOCAL-ID."
  (cl-find local-id (plist-get state :entries)
           :key (lambda (entry) (plist-get entry :local-id))
           :test #'equal))

(defun jds/exchange-calendar--state-put (state entry)
  "Add or replace ENTRY in STATE and return STATE."
  (let* ((local-id (plist-get entry :local-id))
         (entries (cl-remove local-id (plist-get state :entries)
                             :key (lambda (item) (plist-get item :local-id))
                             :test #'equal)))
    (plist-put state :entries (cons entry entries))))

(defun jds/exchange-calendar--state-remove (state local-id)
  "Remove LOCAL-ID from STATE and return STATE."
  (plist-put state :entries
             (cl-remove local-id (plist-get state :entries)
                        :key (lambda (item) (plist-get item :local-id))
                        :test #'equal)))

(defun jds/exchange-calendar--normalize-event (event)
  "Normalize volatile fields in EVENT before hashing."
  (let ((text (replace-regexp-in-string "\r\n" "\n" event)))
    (dolist (regexp '("^DTSTAMP:.*\n" "^LAST-MODIFIED:.*\n"
                      "^CREATED:.*\n" "^SEQUENCE:.*\n"))
      (setq text (replace-regexp-in-string regexp "" text)))
    text))

(defun jds/exchange-calendar--event-hash (event)
  "Return a stable hash for EVENT."
  (secure-hash 'sha256 (jds/exchange-calendar--normalize-event event)))

(defun jds/exchange-calendar--event-property (event property)
  "Return unfolded PROPERTY value from iCalendar EVENT."
  (with-temp-buffer
    (insert event)
    (goto-char (point-min))
    (while (re-search-forward "\r?\n[ \t]" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward
             (format "^%s\\(?:;[^:]*\\)?:\\(.*\\)$"
                     (regexp-quote property)) nil t)
        (string-trim-right (match-string-no-properties 1) "\r")))))

(defun jds/exchange-calendar--replace-event-uid (event uid)
  "Return EVENT with its UID replaced by UID."
  (with-temp-buffer
    (insert event)
    (goto-char (point-min))
    (unless (re-search-forward "^UID\\(?:;[^:]*\\)?:.*\r?$" nil t)
      (error "VEVENT has no UID"))
    (replace-match (concat "UID:" uid) t t)
    (buffer-string)))

(defun jds/exchange-calendar--org-export-uid (uid)
  "Remove Org iCalendar's timestamp-type prefix from UID."
  (if (string-match
       "\\`\\(?:DL\\|SC\\|TS\\|TODO\\|DS\\)[0-9]*-\\(.*\\)\\'" uid)
      (match-string 1 uid)
    uid))

(defun jds/exchange-calendar--wrap-event (event)
  "Wrap EVENT in a VCALENDAR accepted by DavMail."
  (concat "BEGIN:VCALENDAR\r\n"
          "VERSION:2.0\r\n"
          "PRODID:-//jds//Emacs Exchange Calendar//EN\r\n"
          "CALSCALE:GREGORIAN\r\n"
          "X-WR-TIMEZONE:" jds/exchange-calendar-timezone "\r\n"
          (replace-regexp-in-string "\(?:\r?\n\)*\\'" "\r\n" event)
          "END:VCALENDAR\r\n"))

(defun jds/exchange-calendar--org-markers ()
  "Return an alist mapping IDs to markers in the configured Org file."
  (with-current-buffer (find-file-noselect jds/exchange-calendar-file)
    (org-with-wide-buffer
     (let (markers)
       (org-map-entries
        (lambda ()
          (when-let* ((id (org-entry-get nil "ID")))
            (push (cons id (point-marker)) markers)))
        nil 'file)
       markers))))

(defun jds/exchange-calendar--export-events ()
  "Export the configured Org file and return local event plists."
  (let* ((org-caldav-files (list jds/exchange-calendar-file))
         (org-caldav-inbox jds/exchange-calendar-file)
         (org-caldav-sync-direction 'twoway)
         (org-caldav-save-buffers t)
         (org-icalendar-timezone jds/exchange-calendar-timezone)
         (markers (jds/exchange-calendar--org-markers))
         (buffer (org-caldav-generate-ics))
         (temporary (buffer-file-name buffer))
         events)
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (while (re-search-forward "^BEGIN:VEVENT\r?$" nil t)
            (let ((begin (match-beginning 0)))
              (unless (re-search-forward "^END:VEVENT\r?$" nil t)
                (error "Unterminated VEVENT in local export"))
              (let* ((raw-event (buffer-substring-no-properties
                                 begin (line-beginning-position 2)))
                     (raw-id (jds/exchange-calendar--event-property
                              raw-event "UID"))
                     (local-id (jds/exchange-calendar--org-export-uid raw-id))
                     (event (jds/exchange-calendar--replace-event-uid
                             raw-event local-id))
                     (marker (cdr (assoc local-id markers))))
                (unless marker
                  (error "Exported event has no matching Org ID: %s" local-id))
                (with-current-buffer (marker-buffer marker)
                  (save-excursion
                    (goto-char marker)
                    (push (list
                           :local-id local-id
                           :title (org-get-heading t t t t)
                           :marker marker
                           :event event
                           :hash (jds/exchange-calendar--event-hash event)
                           :href (org-entry-get nil
                                                jds/exchange-calendar--href-property)
                           :etag (org-entry-get nil
                                                jds/exchange-calendar--etag-property)
                           :remote-uid
                           (org-entry-get
                            nil jds/exchange-calendar--remote-uid-property))
                          events)))))))
      (when (buffer-live-p buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))
    (nreverse events)))

(defun jds/exchange-calendar--remote-index (inventory)
  "Return a hash table indexing INVENTORY by href."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (item inventory table)
      (puthash (plist-get item :href) item table))))

(defun jds/exchange-calendar--local-index (events)
  "Return a hash table indexing local EVENTS by ID."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (event events table)
      (puthash (plist-get event :local-id) event table))))

(defun jds/exchange-calendar--plan (events inventory state)
  "Classify EVENTS and INVENTORY against STATE.

Return a list of action plists."
  (let ((remote-index (jds/exchange-calendar--remote-index inventory))
        (local-index (jds/exchange-calendar--local-index events))
        (claimed (make-hash-table :test #'equal))
        actions)
    (dolist (event events)
      (let* ((local-id (plist-get event :local-id))
             (record (jds/exchange-calendar--state-entry state local-id))
             (href (or (plist-get record :href) (plist-get event :href)))
             (remote (and href (gethash href remote-index))))
        (when href (puthash href t claimed))
        (cond
         ((null href)
          (push (list :kind 'create-remote :local event) actions))
         ((null record)
          (push (list :kind 'conflict :reason 'untracked-href
                      :local event :remote remote) actions))
         ((null remote)
          (push (list :kind 'remote-deleted :local event :record record)
                actions))
         (t
          (let ((local-changed
                 (not (equal (plist-get event :hash)
                             (plist-get record :local-hash))))
                (remote-changed
                 (not (equal (plist-get remote :etag)
                             (plist-get record :etag)))))
            (push
             (list :kind
                   (cond ((and local-changed remote-changed) 'conflict)
                         (local-changed 'update-remote)
                         (remote-changed 'update-local)
                         (t 'unchanged))
                   :reason (and local-changed remote-changed 'both-changed)
                   :local event :remote remote :record record)
             actions))))))
    (dolist (record (plist-get state :entries))
      (let* ((local-id (plist-get record :local-id))
             (href (plist-get record :href))
             (remote (gethash href remote-index)))
        (unless (gethash local-id local-index)
          (when href (puthash href t claimed))
          (push (list :kind (if remote 'local-deleted 'forget)
                      :record record :remote remote)
                actions))))
    (dolist (remote inventory)
      (unless (gethash (plist-get remote :href) claimed)
        (push (list :kind 'import-remote :remote remote) actions)))
    (nreverse actions)))

(defun jds/exchange-calendar--action-label (action)
  "Return a human-readable summary for ACTION."
  (let* ((kind (plist-get action :kind))
         (local (plist-get action :local))
         (record (plist-get action :record))
         (title (or (plist-get local :title)
                    (plist-get record :title)
                    (plist-get (plist-get action :remote) :href)
                    "unknown")))
    (format "%-14s %s%s"
            kind title
            (if-let* ((reason (plist-get action :reason)))
                (format " (%s)" reason)
              ""))))

(defun jds/exchange-calendar--display-plan (actions)
  "Display ACTIONS in a read-only plan buffer."
  (with-current-buffer (get-buffer-create "*Exchange calendar plan*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Exchange calendar plan (%s)\n\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")))
      (dolist (action actions)
        (insert (jds/exchange-calendar--action-label action) "\n"))
      (insert (format "\n%d action(s); this report made no changes.\n"
                      (length actions)))
      (special-mode))
    (display-buffer (current-buffer))))

;;;###autoload
(defun jds/exchange-calendar-status ()
  "Show a read-only local and remote inventory summary."
  (interactive)
  (let* ((events (jds/exchange-calendar--export-events))
         (inventory (jds/exchange-calendar--remote-inventory))
         (state (jds/exchange-calendar--load-state)))
    (message "Exchange calendar: %d local events, %d remote resources, %d tracked mappings"
             (length events) (length inventory)
             (length (plist-get state :entries)))
    (list :local (length events)
          :remote (length inventory)
          :tracked (length (plist-get state :entries)))))

;;;###autoload
(defun jds/exchange-calendar-dry-run ()
  "Fetch remote metadata and display the synchronization plan."
  (interactive)
  (let* ((events (jds/exchange-calendar--export-events))
         (inventory (jds/exchange-calendar--remote-inventory))
         (state (jds/exchange-calendar--load-state))
         (actions (jds/exchange-calendar--plan events inventory state)))
    (jds/exchange-calendar--display-plan actions)
    actions))

(defun jds/exchange-calendar--put (url event &optional etag)
  "PUT EVENT to URL, optionally conditional on ETAG."
  (jds/exchange-calendar--request
   "PUT" url
   :data (encode-coding-string
          (jds/exchange-calendar--wrap-event event) 'utf-8)
   :headers (append
             '(("Content-type" . "text/calendar; charset=UTF-8"))
             (when etag
               `(("If-Match" . ,(format "\"%s\"" etag)))))))

(defun jds/exchange-calendar--get-event (href)
  "Return the iCalendar body for HREF."
  (plist-get (jds/exchange-calendar--request "GET" href) :body))

(defun jds/exchange-calendar--inventory-item (href)
  "Return current inventory item for HREF."
  (cl-find href (jds/exchange-calendar--remote-inventory)
           :key (lambda (item) (plist-get item :href)) :test #'equal))

(defun jds/exchange-calendar--set-mapping-properties
    (event href etag remote-uid)
  "Set Exchange mapping properties on local EVENT."
  (let ((marker (plist-get event :marker)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (org-entry-put nil jds/exchange-calendar--href-property href)
        (org-entry-put nil jds/exchange-calendar--etag-property etag)
        (org-entry-put nil jds/exchange-calendar--remote-uid-property
                       remote-uid)
        (save-buffer)))))

(defun jds/exchange-calendar--record-for
    (event href etag remote-uid &optional remote-hash)
  "Build a state record for EVENT and its remote identity."
  (list :local-id (plist-get event :local-id)
        :href href
        :etag etag
        :remote-uid remote-uid
        :remote-hash remote-hash
        :local-hash (plist-get event :hash)
        :title (substring-no-properties (plist-get event :title))))

(defun jds/exchange-calendar--create-remote (event state)
  "Create remote EVENT and record its assigned identity in STATE."
  (let* ((local-id (plist-get event :local-id))
         (provisional
          (concat jds/exchange-calendar-url
                  (url-hexify-string local-id) ".EML"))
         (response (jds/exchange-calendar--put
                    provisional (plist-get event :event)))
         (location (plist-get response :location)))
    (unless location
      (error "DavMail created %s but returned no Location; refusing to guess"
             (plist-get event :title)))
    (let* ((href (jds/exchange-calendar--absolute-href location))
           (body (jds/exchange-calendar--get-event href))
           (remote-uid (or (jds/exchange-calendar--event-property body "UID")
                           (error "Created Exchange event has no UID")))
           ;; Exchange can revise the ETag immediately after creation; query
           ;; it after retrieving the finalized representation.
           (remote (or (jds/exchange-calendar--inventory-item href)
                       (error "Created resource is absent from inventory: %s" href)))
           (etag (plist-get remote :etag))
           (record (jds/exchange-calendar--record-for
                    event href etag remote-uid
                    (jds/exchange-calendar--event-hash body))))
      (jds/exchange-calendar--set-mapping-properties
       event href etag remote-uid)
      (jds/exchange-calendar--state-put state record))))

(defun jds/exchange-calendar--update-remote (action state)
  "Push the local side of ACTION and update STATE."
  (let* ((event (plist-get action :local))
         (record (plist-get action :record))
         (href (plist-get record :href))
         (remote-uid (plist-get record :remote-uid))
         (outgoing (if remote-uid
                       (jds/exchange-calendar--replace-event-uid
                        (plist-get event :event) remote-uid)
                     (plist-get event :event))))
    (jds/exchange-calendar--put href outgoing (plist-get record :etag))
    (let* ((remote (or (jds/exchange-calendar--inventory-item href)
                       (error "Updated resource disappeared: %s" href)))
           (etag (plist-get remote :etag))
           (body (jds/exchange-calendar--get-event href))
           (new-remote-uid
            (or (jds/exchange-calendar--event-property body "UID") remote-uid))
           (new-record (jds/exchange-calendar--record-for
                        event href etag new-remote-uid
                        (jds/exchange-calendar--event-hash body))))
      (jds/exchange-calendar--set-mapping-properties
       event href etag new-remote-uid)
      (jds/exchange-calendar--state-put state new-record))))

(defun jds/exchange-calendar--event-data (body)
  "Convert iCalendar BODY using org-caldav's parser."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (encode-coding-string
             (string-trim-left body "[\r\n]+") 'utf-8))
    (org-caldav-convert-event-or-todo--from-buffer nil)))

(defun jds/exchange-calendar--update-local-entry (event body)
  "Update EVENT title and timestamp from iCalendar BODY."
  (let ((data (jds/exchange-calendar--event-data body))
        (marker (plist-get event :marker)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (let-alist data
          (org-caldav-change-heading .summary)
          (org-narrow-to-subtree)
          (goto-char (point-min))
          (let ((range (org-caldav-create-time-range
                        .start-d .start-t .end-d .end-t
                        .e-type .rrule-props)))
            (unless (re-search-forward org-tsr-regexp nil t)
              (error "No timestamp found in local event %s"
                     (plist-get event :local-id)))
            (replace-match range nil t))
          (widen))
        (save-buffer)))))

(defun jds/exchange-calendar--update-local (action state)
  "Pull the remote side of ACTION and update STATE."
  (let* ((event (plist-get action :local))
         (record (plist-get action :record))
         (remote (plist-get action :remote))
         (href (plist-get record :href))
         (body (jds/exchange-calendar--get-event href))
         (remote-hash (jds/exchange-calendar--event-hash body))
         (remote-uid (or (jds/exchange-calendar--event-property body "UID")
                         (plist-get record :remote-uid))))
    ;; Exchange sometimes revises only the ETag shortly after a write.  Do
    ;; not round-trip its normalized text back into Org unless event content
    ;; actually changed.
    (unless (equal remote-hash (plist-get record :remote-hash))
      (jds/exchange-calendar--update-local-entry event body))
    (let* ((fresh (cl-find (plist-get event :local-id)
                           (jds/exchange-calendar--export-events)
                           :key (lambda (item) (plist-get item :local-id))
                           :test #'equal))
           (etag (plist-get remote :etag))
           (new-record (jds/exchange-calendar--record-for
                        fresh href etag remote-uid remote-hash)))
      (jds/exchange-calendar--set-mapping-properties
       fresh href etag remote-uid)
      (jds/exchange-calendar--state-put state new-record))))

(defun jds/exchange-calendar--calendar-insertion-point ()
  "Return a marker at the end of the top-level Calendar subtree."
  (with-current-buffer (find-file-noselect jds/exchange-calendar-file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (unless (re-search-forward "^\\* Calendar[ \t]*$" nil t)
       (error "No top-level Calendar heading in %s"
              jds/exchange-calendar-file))
     (org-back-to-heading t)
     (org-end-of-subtree t t)
     (point-marker))))

(defun jds/exchange-calendar--import-remote (action state)
  "Import the remote side of ACTION and update STATE."
  (let* ((remote (plist-get action :remote))
         (href (plist-get remote :href))
         (etag (plist-get remote :etag))
         (body (jds/exchange-calendar--get-event href))
         (remote-uid (or (jds/exchange-calendar--event-property body "UID")
                         (error "Remote Exchange event has no UID")))
         (local-id (org-id-new))
         (data (jds/exchange-calendar--event-data body))
         (point (jds/exchange-calendar--calendar-insertion-point)))
    (with-current-buffer (marker-buffer point)
      (goto-char point)
      (unless (bolp) (insert "\n"))
      (org-caldav-insert-org-event-or-todo
       (append data `((uid . ,local-id) (level . 2))))
      (org-back-to-heading t)
      (org-entry-put nil jds/exchange-calendar--href-property href)
      (org-entry-put nil jds/exchange-calendar--etag-property etag)
      (org-entry-put nil jds/exchange-calendar--remote-uid-property remote-uid)
      (save-buffer))
    (let ((event (cl-find local-id (jds/exchange-calendar--export-events)
                          :key (lambda (item) (plist-get item :local-id))
                          :test #'equal)))
      (jds/exchange-calendar--state-put
       state (jds/exchange-calendar--record-for
              event href etag remote-uid
              (jds/exchange-calendar--event-hash body))))))

(defun jds/exchange-calendar--delete-remote (action state)
  "Delete remote resource for locally deleted ACTION and update STATE."
  (let* ((record (plist-get action :record))
         (title (or (plist-get record :title) "unknown"))
         (href (plist-get record :href)))
    (if (yes-or-no-p (format "Delete Exchange event %S? " title))
        (progn
          (jds/exchange-calendar--request
           "DELETE" href
           :headers `(("If-Match" . ,(format "\"%s\""
                                                (plist-get record :etag)))))
          (jds/exchange-calendar--state-remove
           state (plist-get record :local-id)))
      state)))

(defun jds/exchange-calendar--delete-local (action state)
  "Handle remotely deleted ACTION conservatively."
  (let* ((event (plist-get action :local))
         (title (plist-get event :title)))
    (if (yes-or-no-p (format "Delete local Org event %S? " title))
        (let ((marker (plist-get event :marker)))
          (with-current-buffer (marker-buffer marker)
            (goto-char marker)
            (org-cut-subtree)
            (save-buffer))
          (jds/exchange-calendar--state-remove
           state (plist-get event :local-id)))
      state)))

(defun jds/exchange-calendar--apply-action (action state)
  "Apply one ACTION to STATE and return the updated state."
  (pcase (plist-get action :kind)
    ('create-remote
     (jds/exchange-calendar--create-remote (plist-get action :local) state))
    ('update-remote (jds/exchange-calendar--update-remote action state))
    ('update-local (jds/exchange-calendar--update-local action state))
    ('import-remote (jds/exchange-calendar--import-remote action state))
    ('local-deleted (jds/exchange-calendar--delete-remote action state))
    ('remote-deleted (jds/exchange-calendar--delete-local action state))
    ('forget
     (jds/exchange-calendar--state-remove
      state (plist-get (plist-get action :record) :local-id)))
    ('unchanged state)
    ('conflict
     (error "Exchange calendar conflict: %s"
            (jds/exchange-calendar--action-label action)))
    (_ (error "Unknown Exchange calendar action: %S"
              (plist-get action :kind)))))

(defun jds/exchange-calendar--sync-actions (actions state)
  "Apply ACTIONS, saving STATE after every successful operation."
  (dolist (action actions state)
    (setq state (jds/exchange-calendar--apply-action action state))
    (jds/exchange-calendar--save-state state)))

;;;###autoload
(defun jds/exchange-calendar-sync-entry ()
  "Synchronize only the exportable calendar entry at point."
  (interactive)
  (org-back-to-heading t)
  (let ((local-id (org-entry-get nil "ID")))
    (unless local-id
      (user-error "Entry has no ID and is not ready for synchronization"))
    (let* ((events (jds/exchange-calendar--export-events))
           (event (cl-find local-id events
                           :key (lambda (item) (plist-get item :local-id))
                           :test #'equal))
           (inventory (jds/exchange-calendar--remote-inventory))
           (state (jds/exchange-calendar--load-state))
           (actions (jds/exchange-calendar--plan events inventory state))
           (action (cl-find local-id actions
                            :key (lambda (item)
                                   (or (plist-get
                                        (plist-get item :local) :local-id)
                                       (plist-get
                                        (plist-get item :record) :local-id)))
                            :test #'equal)))
      (unless event
        (user-error "Entry at point does not export as a VEVENT"))
      (unless action
        (user-error "No synchronization action found for %s" local-id))
      (when (memq (plist-get action :kind)
                  '(import-remote local-deleted forget))
        (user-error "Entry command cannot apply %s"
                    (plist-get action :kind)))
      (jds/exchange-calendar--sync-actions (list action) state)
      (message "Exchange entry sync finished: %s"
               (jds/exchange-calendar--action-label action)))))

;;;###autoload
(defun jds/exchange-calendar-import-new ()
  "Import remote events that have no tracked local counterpart.

This command never pushes or deletes events."
  (interactive)
  (let* ((events (jds/exchange-calendar--export-events))
         (inventory (jds/exchange-calendar--remote-inventory))
         (state (jds/exchange-calendar--load-state))
         (actions (cl-remove-if-not
                   (lambda (action)
                     (eq (plist-get action :kind) 'import-remote))
                   (jds/exchange-calendar--plan events inventory state))))
    (if (null actions)
        (message "No new Exchange events to import")
      (jds/exchange-calendar--sync-actions actions state)
      (message "Imported %d new Exchange event(s)" (length actions)))))

;;;###autoload
(defun jds/exchange-calendar-sync ()
  "Synchronize the whole configured calendar after displaying its plan."
  (interactive)
  (unless jds/exchange-calendar-allow-full-sync
    (user-error
     "Full sync is locked until single-entry round-trip tests pass"))
  (let* ((events (jds/exchange-calendar--export-events))
         (inventory (jds/exchange-calendar--remote-inventory))
         (state (jds/exchange-calendar--load-state))
         (actions (jds/exchange-calendar--plan events inventory state)))
    (jds/exchange-calendar--display-plan actions)
    (when (cl-find 'conflict actions :key (lambda (a) (plist-get a :kind)))
      (user-error "Resolve conflicts shown in the plan before syncing"))
    (unless (yes-or-no-p (format "Apply %d Exchange calendar actions? "
                                 (length actions)))
      (user-error "Exchange calendar sync cancelled"))
    (jds/exchange-calendar--sync-actions actions state)
    (message "Exchange calendar sync finished")))

(provide 'calendar-exchange)

;;; calendar-exchange.el ends here
