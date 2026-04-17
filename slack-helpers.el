;;; slack-helpers.el --- slack helpers and compatibility shims -*- lexical-binding: t; -*-

(require 'auth-source)
(require 'cl-lib)
(require 'nadvice)
(require 'subr-x)

(declare-function ol/slack-follow-link-legacy "ol-emacs-slack-legacy")
(declare-function slack-buffer-add-reaction-to-message "slack-message-reaction")
(declare-function slack-buffer-goto "slack-buffer")
(declare-function slack-conversations-info "slack-conversations")
(declare-function slack-conversations-open "slack-conversations")
(declare-function slack-current-room-and-team "slack-buffer")
(declare-function slack-file-upload "slack-file")
(declare-function slack--file-upload-v2 "slack-file")
(declare-function slack-room-display "slack-room")
(declare-function slack-room-find "slack-room")
(declare-function slack-room-create "slack-room")
(declare-function slack-room-find-message "slack-room")
(declare-function slack-room-name "slack-room")
(declare-function slack-room-names "slack-room")
(declare-function slack-team-channels "slack-team")
(declare-function slack-team-groups "slack-team")
(declare-function slack-team-ims "slack-team")
(declare-function slack-team-set-room "slack-team")
(declare-function slack-start "slack")
(declare-function slack-team-connected-list "slack-team")
(declare-function slack-team-find "slack-team")
(declare-function slack-team-name "slack-team")
(declare-function slack-team-select "slack-team")
(declare-function slack-thread-show-messages "slack-thread")
(declare-function slack-thread-ts "slack-thread")
(declare-function slack-get-ts "slack-util")
(declare-function slack-user-hidden-p "slack-user")
(declare-function slack-user-names "slack-user")
(declare-function slack-user-self-p "slack-user")

(defvar ol/slack-follow-link-legacy-warn-user)
(defvar slack-current-buffer)

(defun jds~slack-request-response-object (response)
  "Return RESPONSE or Slack's wrapped request-response when possible."
  (cond
   ((and (fboundp 'request-response-p)
         (request-response-p response))
    response)
   ((ignore-errors (object-of-class-p response 'slack-request-request))
    (let ((slot-name 'response))
      (let ((inner (ignore-errors (slot-value response slot-name))))
        (when (and inner
                   (fboundp 'request-response-p)
                   (request-response-p inner))
          inner))))
   (t nil)))

(defun jds~slack-request-response-header (orig response field-name)
  "Call ORIG for RESPONSE when it is a `request-response' object."
  (let ((request-response (jds~slack-request-response-object response)))
    (when request-response
      (funcall orig request-response field-name))))

(defun jds~slack-request-response-done-p (orig response)
  "Handle Slack's request wrapper when checking whether RESPONSE is done."
  (let ((request-response (jds~slack-request-response-object response)))
    (when request-response
      (funcall orig request-response))))

(defun jds~slack-install-request-response-advice ()
  (when (and (fboundp 'request-response-header)
             (not (advice-member-p #'jds~slack-request-response-header
                                   'request-response-header)))
    (advice-add 'request-response-header :around #'jds~slack-request-response-header))
  (when (and (fboundp 'request-response-done-p)
             (not (advice-member-p #'jds~slack-request-response-done-p
                                   'request-response-done-p)))
    (advice-add 'request-response-done-p :around #'jds~slack-request-response-done-p)))

(if (featurep 'request)
    (jds~slack-install-request-response-advice)
  (with-eval-after-load 'request
    (jds~slack-install-request-response-advice)))

(defun jds~slack-stop (orig &optional force)
  "Call ORIG with FORCE defaulting to nil.

`slack-refresh-token' in the vendored package still calls `slack-stop'
with no arguments, while the current definition requires one.  Keep the
interactive refresh flow working by accepting the old arity as well."
  (funcall orig force))

(defun jds~slack-create-thread-message-buffer (orig room team thread-ts &optional has-more &rest _ignored)
  "Call ORIG with the thread buffer constructor's supported arguments."
  (funcall orig room team thread-ts has-more))

(defun jds~slack-install-thread-buffer-advice ()
  (when (and (fboundp 'slack-create-thread-message-buffer)
             (not (advice-member-p #'jds~slack-create-thread-message-buffer
                                   'slack-create-thread-message-buffer)))
    (advice-add 'slack-create-thread-message-buffer
                :around
                #'jds~slack-create-thread-message-buffer)))

(if (featurep 'slack-thread-message-buffer)
    (jds~slack-install-thread-buffer-advice)
  (with-eval-after-load 'slack-thread-message-buffer
    (jds~slack-install-thread-buffer-advice)))

(defun jds~slack-install-stop-advice ()
  (when (and (fboundp 'slack-stop)
             (not (advice-member-p #'jds~slack-stop 'slack-stop)))
    (advice-add 'slack-stop :around #'jds~slack-stop)))

(if (featurep 'slack)
    (jds~slack-install-stop-advice)
  (with-eval-after-load 'slack
    (jds~slack-install-stop-advice)))

(defun jds~slack-auth-source-password (host user)
  "Return the Slack auth-source password for HOST and USER.

Any auth-source or decrypt error is converted into a nil return so
Slack can skip startup cleanly during reloads."
  (condition-case err
      (let ((password (auth-source-pick-first-password :host host :user user)))
        (when (stringp password)
          password))
    (error
     (message "Slack auth-source lookup failed for %s/%s: %s"
              host user (error-message-string err))
     nil)))

(defun jds~slack-auth-source-cookie (host user)
  "Return the Slack cookie for HOST and USER."
  (jds~slack-auth-source-password host (concat user "^cookie")))

(defun jds~slack-ensure-team-ready (team-id &optional timeout-seconds)
  "Return the Slack team for TEAM-ID, starting Slack first if needed.

When Slack has not been started yet, this kicks off `slack-start' and then
waits briefly for the authorization flow to populate Slack's internal team
lookup tables."
  (let ((team (and (fboundp 'slack-team-find)
                   (slack-team-find team-id))))
    (when (and (null team)
               (fboundp 'slack-start))
      (if (and (fboundp 'slack-team-connected-list)
               (slack-team-connected-list))
          nil
        (condition-case err
            (slack-start)
          (error
           (user-error "Slack start failed while opening org link: %s"
                       (error-message-string err))))))
    (let ((deadline (+ (float-time) (or timeout-seconds 10.0))))
      (while (and (null team)
                  (< (float-time) deadline))
        (accept-process-output nil 0.2)
        (setq team (and (fboundp 'slack-team-find)
                        (slack-team-find team-id))))
      team)))

(defun jds~slack-parse-org-link (link)
  "Parse a Slack org LINK into its team, room, and timestamp components."
  (let* ((parts (split-string link "[|&]" t))
         (team-id (nth 0 parts))
         (room-id (nth 1 parts))
         (ts (cl-loop for part in (cddr parts)
                      when (string-prefix-p "ts:" part)
                      return (substring part 3))))
    (list :team-id team-id
          :room-id room-id
          :ts ts)))

(defun jds/slack-follow-org-link (link)
  "Follow an `emacs-slack' LINK, starting Slack first when necessary."
  (if (not (string-match-p "[|&]" link))
      (progn
        (require 'ol-emacs-slack-legacy)
        (ol/slack-follow-link-legacy link)
        (when ol/slack-follow-link-legacy-warn-user
          (warn (concat
                 "This was a legacy link,"
                 " please re run `M-x org-store-link'"
                 " and replace the legacy link."
                 " Or silence me by customizing"
                 " ol/slack-follow-link-legacy-warn-user."))))
    (let* ((parsed (jds~slack-parse-org-link link))
           (team-id (plist-get parsed :team-id))
           (room-id (plist-get parsed :room-id))
           (ts (plist-get parsed :ts))
           (team (or (and (fboundp 'slack-team-find)
                          (slack-team-find team-id))
                     (jds~slack-ensure-team-ready team-id)))
           (room (and team (slack-room-find room-id team)))
           (message (and room
                         ts
                         (ignore-errors
                           (slack-room-find-message room ts))))
           (thread-ts (and message
                           (ignore-errors (slack-thread-ts message))))
           (go-to-link-position `(lambda ()
                                   (message "-- attempting to get to slack position %s" ,ts)
                                   (slack-buffer-goto ,ts))))
      (unless team
        (user-error "Slack team %s is not ready yet" team-id))
      (if room
          (if thread-ts
              (slack-thread-show-messages message room team go-to-link-position)
            (slack-room-display room team go-to-link-position))
        (slack-conversations-info
         room-id
         team
         (lambda ()
           (let ((resolved-room (slack-room-find room-id team)))
             (if resolved-room
                 (slack-room-display resolved-room team go-to-link-position)
               (message "Slack room %s is not available yet" room-id)
               (user-error "Slack room %s is not available yet" room-id)))))))))

(defun jds~slack-default-modeline-formatter (alist)
  "Format Slack modeline state from ALIST."
  (mapconcat
   (lambda (entry)
     (let* ((summary (cdr entry))
            (channel (cdr (cl-assoc 'channel summary)))
            (channel-has-unreads (car channel))
            (channel-mention-count (cdr channel)))
       (format " S:%s "
               (if (or channel-has-unreads (< 0 channel-mention-count))
                   (propertize (number-to-string channel-mention-count)
                               'face 'slack-modeline-channel-has-unreads-face)
                 channel-mention-count))))
   alist " "))

(defun jds/slack-message-add-thumbsup ()
  "Add a +1 reaction to the current Slack message."
  (interactive)
  (when-let* ((buf slack-current-buffer))
    (slack-buffer-add-reaction-to-message buf
                                          "+1"
                                          (slack-get-ts))))

(defun jds~slack-group-dm-candidates (team)
  "Return completion candidates for TEAM as (LABEL . USER) pairs."
  (unless (fboundp 'slack-user-names)
    (user-error "Slack user completion is unavailable"))
  (let* ((users (slack-user-names
                 team
                 (lambda (users)
                   (if (fboundp 'slack-user-self-p)
                       (cl-remove-if
                        (lambda (user)
                          (slack-user-self-p (plist-get user :id) team))
                        users)
                     users))))
         (sorted-users (sort (copy-sequence users)
                             (lambda (a b)
                               (string-lessp (car a) (car b)))))
         (candidates (mapcar (lambda (entry)
                               (let ((name (car entry))
                                     (user (cdr entry)))
                                 (cons (format "%s <%s>" name (plist-get user :id))
                                       user)))
                             sorted-users)))
    candidates))

(defun jds~slack-room-candidates (team)
  "Return completion candidates for TEAM as (LABEL . ROOM) pairs."
  (unless (fboundp 'slack-room-names)
    (user-error "Slack room completion is unavailable"))
  (let* ((rooms (append (and (fboundp 'slack-team-ims)
                             (slack-team-ims team))
                        (and (fboundp 'slack-team-channels)
                             (slack-team-channels team))
                        (and (fboundp 'slack-team-groups)
                             (slack-team-groups team))))
         (room-candidates (cl-delete-duplicates
                           (slack-room-names rooms team)
                           :test #'string=
                           :key #'car)))
    (cl-remove-if-not #'cdr room-candidates)))

(defun jds~slack-visible-room-context (window)
  "Return the Slack room context visible in WINDOW, or nil."
  (let ((buffer (window-buffer window)))
    (with-current-buffer buffer
      (when (and (or (derived-mode-p 'slack-mode)
                     (string-prefix-p "slack-" (symbol-name major-mode)))
                 (fboundp 'slack-current-room-and-team))
        (ignore-errors
          (cl-destructuring-bind (room team)
              (slack-current-room-and-team)
            (when (and room team)
              (list :buffer buffer
                    :window window
                    :room room
                    :team team))))))))

(defun jds~slack-visible-room-contexts ()
  "Return visible Slack room contexts sorted by most recently used first."
  (sort (delq nil
              (mapcar #'jds~slack-visible-room-context
                      (apply #'append
                             (mapcar (lambda (frame)
                                       (window-list frame nil))
                                     (frame-list)))))
        (lambda (a b)
          (> (or (window-use-time (plist-get a :window)) 0)
             (or (window-use-time (plist-get b :window)) 0)))))

(defun jds~slack-room-context-label (context)
  "Return a human-readable label for CONTEXT."
  (let* ((room (plist-get context :room))
         (team (plist-get context :team))
         (room-name (or (and (fboundp 'slack-room-name)
                             (ignore-errors (slack-room-name room team)))
                        (buffer-name (plist-get context :buffer))))
         (team-name (and (fboundp 'slack-team-name)
                         (ignore-errors (slack-team-name team)))))
    (if team-name
        (format "%s (%s)" room-name team-name)
      room-name)))

(defun jds~slack-read-room-context ()
  "Prompt for a Slack room context and return it as a plist."
  (let ((team (if (fboundp 'slack-team-select)
                  (slack-team-select)
                (user-error "Slack team selection is unavailable"))))
    (let* ((candidates (jds~slack-room-candidates team))
           (choice (completing-read "Slack room: "
                                    (mapcar #'car candidates)
                                    nil t))
           (room (cdr (assoc choice candidates))))
      (unless room
        (user-error "Slack room %s is unavailable" choice))
      (list :room room
            :team team))))

(defun jds~slack-select-upload-context ()
  "Select the Slack room context that should receive a file upload."
  (let* ((contexts (jds~slack-visible-room-contexts)))
    (cond
     ((null contexts)
      (jds~slack-read-room-context))
     ((null (cdr contexts))
      (car contexts))
     ((y-or-n-p (format "Use %s for Slack file upload? "
                        (jds~slack-room-context-label (car contexts))))
      (car contexts))
     (t
      (jds~slack-read-room-context)))))

(defun jds~slack-upload-file-to-room (file room team &optional initial-comment)
  "Upload FILE to ROOM in TEAM.
When INITIAL-COMMENT is non-nil, send it as Slack's accompanying text."
  (unless (and (fboundp 'slack--file-upload-v2)
               (ignore-errors (oref room id)))
    (user-error "Slack file upload is unavailable"))
  (let ((channel-id (oref room id))
        (title (file-name-nondirectory file)))
    (slack--file-upload-v2 file title team channel-id initial-comment)))

(defun jds~slack-read-file-comment (file)
  "Prompt for a Slack comment to accompany FILE."
  (let ((comment (read-string (format "Slack comment for %s: "
                                      (file-name-nondirectory file)))))
    (unless (string-empty-p comment)
      comment)))

(defun jds/slack-dired-attach-files (&optional prompt-comment files)
  "Upload the marked Dired FILES to a Slack room.
When called interactively in Dired, use the marked file(s) or the file at
point.  The target Slack room is chosen from visible Slack buffers when
possible, otherwise the user is prompted.
With a prefix argument, prompt for a Slack comment separately for each file."
  (interactive "P")
  (unless (derived-mode-p 'dired-mode)
    (user-error "This command only works in Dired"))
  (let* ((files (or files (dired-get-marked-files nil nil)))
         (regular-files (cl-remove-if-not #'file-regular-p files))
         (context (jds~slack-select-upload-context))
         (room (plist-get context :room))
         (team (plist-get context :team)))
    (unless regular-files
      (user-error "No regular files selected"))
    (unless (and room team)
      (user-error "Slack room selection failed"))
    (message "Uploading %d file(s) to %s"
             (length regular-files)
             (jds~slack-room-context-label context))
    (dolist (file regular-files)
      (jds~slack-upload-file-to-room
       file
       room
       team
       (when prompt-comment
         (jds~slack-read-file-comment file))))
    (message "Queued %d file(s) for Slack upload to %s"
             (length regular-files)
             (jds~slack-room-context-label context))))

(defun jds~slack-open-group-dm-display (team channel selected-labels)
  "Register CHANNEL for TEAM and display the opened conversation."
  (let* ((channel-id (plist-get channel :id))
         (room-class (cond
                      ((eq t (plist-get channel :is_channel)) 'slack-channel)
                      ((eq t (plist-get channel :is_im)) 'slack-im)
                      ((or (eq t (plist-get channel :is_group))
                           (eq t (plist-get channel :is_mpim)))
                       'slack-group)))
         (room (and channel-id
                    room-class
                    (slack-room-create channel room-class))))
    (if room
        (progn
          (slack-team-set-room team room)
          (slack-room-display room team)
          (message "Opened Slack group DM with %s"
                   (string-join selected-labels ", ")))
      (message "Slack opened conversation %s but could not display it"
               (or channel-id "<unknown>")))))

(defun jds/slack-open-group-dm (&optional team)
  "Interactively select Slack users and open a group DM."
  (interactive)
  (let* ((team (or team
                   (if (fboundp 'slack-team-select)
                       (slack-team-select)
                     (user-error "Slack team selection is unavailable"))))
         (candidates (jds~slack-group-dm-candidates team))
         (labels (mapcar #'car candidates))
         (selected-labels (completing-read-multiple
                           "Slack users: "
                           labels nil t))
         (selected-user-ids
          (cl-remove-duplicates
           (delq nil
                 (mapcar (lambda (label)
                           (plist-get (cdr (assoc label candidates)) :id))
                         selected-labels))
           :test #'string=)))
    (unless selected-user-ids
      (user-error "No Slack users selected"))
    (slack-conversations-open
     team
     :user-ids selected-user-ids
     :on-success
     (lambda (data)
       (let ((channel (plist-get data :channel)))
         (if channel
             (jds~slack-open-group-dm-display team channel selected-labels)
           (message "Slack opened a conversation, but no channel was returned")))))))

(defalias 'my/slack-open-group-dm #'jds/slack-open-group-dm)

(provide 'slack-helpers)
;;; slack-helpers.el ends here
