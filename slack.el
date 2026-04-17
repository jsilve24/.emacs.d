;;; slack.el --- slack configuration              -*- lexical-binding: t; -*-

(require 'auth-source)
(require 'cl-lib)
(require 'nadvice)

(declare-function ol/slack-follow-link-legacy "ol-emacs-slack-legacy")

(defvar ol/slack-follow-link-legacy-warn-user)

(defun jds/slack--request-response-object (response)
  "Return RESPONSE or Slack's wrapped request-response when possible."
  (cond
   ((and (fboundp 'request-response-p)
         (request-response-p response))
    response)
   ((and (ignore-errors (object-of-class-p response 'slack-request-request))
         (slot-boundp response 'response))
    (let ((inner (oref response response)))
      (when (and inner
                 (fboundp 'request-response-p)
                 (request-response-p inner))
        inner)))
   (t nil)))

(defun jds/slack--request-response-header (orig response field-name)
  "Call ORIG for RESPONSE when it is a `request-response' object."
  (let ((request-response (jds/slack--request-response-object response)))
    (when request-response
      (funcall orig request-response field-name))))

(defun jds/slack--request-response-done-p (orig response)
  "Handle Slack's request wrapper when checking whether RESPONSE is done."
  (let ((request-response (jds/slack--request-response-object response)))
    (when request-response
      (funcall orig request-response))))

(defun jds/slack-install-request-response-advice ()
  (when (and (fboundp 'request-response-header)
             (not (advice-member-p #'jds/slack--request-response-header
                                   'request-response-header)))
    (advice-add 'request-response-header :around #'jds/slack--request-response-header))
  (when (and (fboundp 'request-response-done-p)
             (not (advice-member-p #'jds/slack--request-response-done-p
                                   'request-response-done-p)))
    (advice-add 'request-response-done-p :around #'jds/slack--request-response-done-p)))

(if (featurep 'request)
    (jds/slack-install-request-response-advice)
  (with-eval-after-load 'request
    (jds/slack-install-request-response-advice)))

(defun jds/slack--slack-stop (orig &optional force)
  "Call ORIG with FORCE defaulting to nil.

`slack-refresh-token' in the vendored package still calls `slack-stop'
with no arguments, while the current definition requires one.  Keep the
interactive refresh flow working by accepting the old arity as well."
  (funcall orig force))

(defun jds/slack--create-thread-message-buffer (orig room team thread-ts &optional has-more &rest _ignored)
  "Call ORIG with the thread buffer constructor's supported arguments."
  (funcall orig room team thread-ts has-more))

(defun jds/slack-install-thread-buffer-advice ()
  (when (and (fboundp 'slack-create-thread-message-buffer)
             (not (advice-member-p #'jds/slack--create-thread-message-buffer
                                   'slack-create-thread-message-buffer)))
    (advice-add 'slack-create-thread-message-buffer
                :around
                #'jds/slack--create-thread-message-buffer)))

(if (featurep 'slack-thread-message-buffer)
    (jds/slack-install-thread-buffer-advice)
  (with-eval-after-load 'slack-thread-message-buffer
    (jds/slack-install-thread-buffer-advice)))

(defun jds/slack-install-stop-advice ()
  (when (and (fboundp 'slack-stop)
             (not (advice-member-p #'jds/slack--slack-stop 'slack-stop)))
    (advice-add 'slack-stop :around #'jds/slack--slack-stop)))

(if (featurep 'slack)
    (jds/slack-install-stop-advice)
  (with-eval-after-load 'slack
    (jds/slack-install-stop-advice)))

(defun jds/slack-auth-source-password (host user)
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

(defun jds/slack-auth-source-cookie (host user)
  "Return the Slack cookie for HOST and USER."
  (jds/slack-auth-source-password host (concat user "^cookie")))

(defun jds/slack--ensure-team-ready (team-id &optional timeout-seconds)
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

(defun jds/slack--parse-org-link (link)
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
    (let* ((parsed (jds/slack--parse-org-link link))
           (team-id (plist-get parsed :team-id))
           (room-id (plist-get parsed :room-id))
           (ts (plist-get parsed :ts))
           (team (or (and (fboundp 'slack-team-find)
                          (slack-team-find team-id))
                     (jds/slack--ensure-team-ready team-id)))
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


(use-package slack
  ;; :disabled t
  :init
  (setq slack-buffer-emojify t
	slack-prefer-current-team t
	slack-file-dir "~/Downloads/")
  :config
  ;; make default same window
  (setq slack-buffer-function #'switch-to-buffer)

  ;; setup team
  (let* ((host "silvermanlabhq.slack.com")
         (user "justinsilverman@psu.edu")
         (token (jds/slack-auth-source-password host user))
         (cookie (jds/slack-auth-source-cookie host user)))
    (cond
     ((not (stringp token))
      (message "Slack token not found in auth-source for %s; skipping startup until credentials are added." user))
     ((not (stringp cookie))
      (message "Slack cookie not found in auth-source for %s; skipping startup until credentials are added." user))
     (t
      (slack-register-team
       :name "silvermanlabhq"
       :default t
       :token token
       :cookie cookie
       :subscribed-channels '((general))
       ;; :full-and-display-names t
       ;; :visible-threads t
       :modeline-enabled t
       :modeline-name "")
      (when (jds~internet-up-p)
        (condition-case err
            (slack-start)
          (error
           (message "Slack startup failed: %s" (error-message-string err))))))))

  ;; setup modeline
  ;; https://github.com/yuya373/emacs-slack/pull/179
  ;; (setq slack-modeline-formatter 'slack-default-modeline-formatter)
  (defun jds~slack-default-modeline-formatter (alist)
    "Element in ALIST is '((team-name . ((thread . (has-unreads .
mention-count)) (channel . (has-unreads . mention-count)))))"
    (mapconcat #'(lambda (e)
		   (let* ((team-name (car e))
			  (summary (cdr e))
			  (thread (cdr (cl-assoc 'thread summary)))
			  (channel (cdr (cl-assoc 'channel summary)))
			  (thread-has-unreads (car thread))
			  (channel-has-unreads (car channel))
			  (has-unreads (or thread-has-unreads
					   channel-has-unreads))
			  (thread-mention-count (cdr thread))
			  (channel-mention-count (cdr channel)))
		     (format " S:%s "
			     (if (or channel-has-unreads (< 0 channel-mention-count))
				 (propertize (number-to-string channel-mention-count)
					     'face 'slack-modeline-channel-has-unreads-face)
			       channel-mention-count))))
	       alist " "))


  (setq slack-modeline-formatter 'jds~slack-default-modeline-formatter)
  (setq slack-enable-global-mode-string t)

  ;; enable live markup in messages
  ;; (setq slack-enable-wysiwyg t)

  ;; add company support
  ;; (add-to-list 'company-backends 'company-slack-backend)
  ;; setup tab command - bind over lui mode completion 
  ;; (add-hook 'slack-mode-hook 'jds/completion-keys)
  ;; (setq lui-completion-function 'company-complete)

  ;; auto-start slack if connected to the internet and not already started
  ;; Startup already happens above when credentials are available.
  )

;; quick reactions with thumbs up

;;;###autoload
(defun jds/slack-message-add-thumbsup ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer)
                  ;; (team (slack-buffer-team buf))
                  (reaction "+1"))
      (slack-buffer-add-reaction-to-message buf
                                            reaction
                                            (slack-get-ts))))

(use-package ol-emacs-slack
  :straight (ol-emacs-slack :type git :host github :repo "ag91/ol-emacs-slack")
  :config
  (org-link-set-parameters "emacs-slack"
                           :follow #'jds/slack-follow-org-link
                           :export #'ol/slack-export
                           :store #'ol/slack-store-link))

(jds/localleader-def
  :keymaps 'slack-info-mode-map
  :state 'normal
  "u" #'slack-room-update-messages)

(jds/localleader-def
  :keymaps 'slack-mode-map
  :state 'normal
  "d" 'slack-buffer-kill
  "+"  'jds/slack-message-add-thumbsup
  "rr" 'slack-message-add-reaction
  "rR" 'slack-message-remove-reaction
  "rs" 'slack-message-show-reaction-users
  "pl" 'slack-room-pins-list
  "pa" 'slack-message-pins-add
  "pr" 'slack-message-pins-remove
  "s" 'slack-search-from-messages
  "fs" 'slack-search-from-files
  "fl" 'slack-file-list
  "mm" 'slack-message-write-another-buffer
  "me" 'slack-message-edit
  "md" 'slack-message-delete
  "u" 'slack-room-update-messages
  "M" 'slack-message-embed-mention
  "C" 'slack-message-embed-channel
  "a" 'slack-file-upload
  "A" 'slack-download-file-at-point)

(general-define-key
 :keymaps 'slack-mode-map
 :states 'n
 "C-j" 'slack-buffer-goto-next-message
 "C-k" 'slack-buffer-goto-prev-message)

(jds/localleader-def
  :keymaps 'slack-edit-message-mode-map
  :state 'normal
  "k" 'slack-message-cancel-edit
  "m" 'slack-message-send-from-buffer
  "M" 'slack-message-embed-mention
  "C" 'slack-message-embed-channel)

;; dependencies

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))
