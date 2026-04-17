;;; slack.el --- slack configuration              -*- lexical-binding: t; -*-

;; Parent module owns its helper commands so `init.el` stays focused on
;; loading modules rather than on Slack package internals.
(load (expand-file-name "slack-helpers.el" user-emacs-directory) nil t)


(use-package slack
  ;; Use the jsilve24 fork while the Slack request-wrapper fix is unmerged upstream.
  :straight (slack :type git :host github :repo "emacs-slack/emacs-slack"
                   :fork t
                   :branch "fix/request-wrapper-unwrapping")
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
         (token (jds~slack-auth-source-password host user))
         (cookie (jds~slack-auth-source-cookie host user)))
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
       :subscribed-channels '((general lab-meeting ml-scale))
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
  (setq slack-modeline-formatter #'jds~slack-default-modeline-formatter)
  (setq slack-enable-global-mode-string t)

  ;; enable live markup in messages
  (setq slack-enable-wysiwyg t)
  )

;; quick reactions with thumbs up

(use-package ol-emacs-slack
  :straight (ol-emacs-slack :type git :host github :repo "ag91/ol-emacs-slack")
  :config
  (org-link-set-parameters "emacs-slack"
                           :follow #'jds/slack-follow-org-link
                           :export #'ol/slack-export
                           :store #'ol/slack-store-link))

(defun jds~slack-capture-target-file ()
  "Return the Org file used for Slack captures."
  (expand-file-name (or (and (boundp '+org-capture-emails-file)
                             +org-capture-emails-file)
                        "mail.org")
                    (or (and (boundp 'org-directory) org-directory)
                        user-emacs-directory)))

(defun jds~slack-message-at-point ()
  "Return the Slack message, room, and team at point.
The result is a plist with :message, :room, :team, and :ts keys."
  (when-let* ((buffer (bound-and-true-p slack-current-buffer))
              (team (ignore-errors (slack-buffer-team buffer)))
              (room (ignore-errors (slack-buffer-room buffer)))
              (ts (slack-get-ts))
              (message (and room ts (ignore-errors (slack-room-find-message room ts)))))
    (list :message message
          :room room
          :team team
          :ts ts)))

(defun jds~slack-capture-message-label (message team)
  "Return a short agenda-friendly label for MESSAGE in TEAM."
  (let* ((sender (string-trim
                  (or (ignore-errors (slack-message-sender-name message team))
                      "Slack")))
         (body (or (ignore-errors (slack-message-body message team))
                   (ignore-errors (slack-message-get-text message team))
                   ""))
         (body (if (stringp body) (substring-no-properties body) ""))
         (body (replace-regexp-in-string "[[:space:]\r\n]+" " "
                                         (string-trim body)))
         (sender (truncate-string-to-width sender 24 nil nil t))
         (body (truncate-string-to-width body 36 nil nil t))
         (label (if (string-empty-p body)
                    sender
                  (format "%s - %s" sender body))))
    (truncate-string-to-width label 64 nil nil t)))

;;;###autoload
(defun +slack/capture-msg-to-agenda (arg)
  "Capture the Slack message at point in `mail.org' with a TODO heading.
The heading uses a short message label plus an `emacs-slack' link so it stays
readable in agenda buffers.  With one prefix, schedule for tomorrow.  With no
prefix, schedule for today."
  (interactive "p")
  (let* ((context (jds~slack-message-at-point))
         (message (plist-get context :message))
         (room (plist-get context :room))
         (team (plist-get context :team))
         (ts (plist-get context :ts))
         (link (and team room ts
                    (format "emacs-slack:%s"
                            (ol/slack-format-link team room ts))))
         (label (and message team
                     (jds~slack-capture-message-label message team))))
    (unless message
      (user-error "No Slack message at point"))
    (unless link
      (user-error "No Slack message link available"))
    ;; Keep the capture shape aligned with the existing mu4e binding.
    (with-current-buffer (find-file-noselect
                          (jds~slack-capture-target-file))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^\\* Email" nil t)
          (let (org-M-RET-may-split-line
                (lev (org-outline-level))
                (folded-p (invisible-p (point-at-eol))))
            (when folded-p (show-branches))
            (org-end-of-meta-data)
            (org-insert-todo-heading 1)
            (when (= (org-outline-level) lev)
              (org-do-demote))
            (insert (concat "Respond to "
                            "[[" link "][" label "]] "))
            (org-schedule nil
                          (cond ((= arg 1) (format-time-string "%Y-%m-%d"))
                                ((= arg 4) "+1d")))
            (org-update-parent-todo-statistics)
            (jds/org-capture-set-last-stored-marker)
            (if folded-p
                (progn
                  (org-up-heading-safe)
                  (hide-subtree))
              (hide-entry))
            (save-buffer)))))))

(with-eval-after-load 'link-hint
  (defun jds~slack-file-link-name (thing)
    "Return a sensible filename for THING."
    (cond
     ((stringp thing)
      (let* ((clean-url (car (split-string thing "[?#]" t)))
             (name (file-name-nondirectory clean-url)))
        (if (and name (not (string= name "")))
            name
          "slack-file")))
     ((ignore-errors (oref thing name)))
     ((ignore-errors (oref thing title)))
     ((ignore-errors (oref thing id)))
     (t "slack-file")))

  (defun jds~slack-file-link-url (thing)
    "Return a Slack download URL for THING or nil."
    (or (and (stringp thing)
             (string-match-p "\\`https?://" thing)
             thing)
        (ignore-errors (oref thing url-private-download))
        (ignore-errors (oref thing url-download))))

  (defun jds~slack-file-link-describe (thing)
    "Return a short label for THING."
    (or (ignore-errors (slack-file-title thing))
        (jds~slack-file-link-name thing)))

  (defun jds~slack-file-link-normalize-target (thing)
    "Return THING if non-nil, otherwise use the Slack file target at point."
    (or thing
        (jds~slack-file-link-target-at-point)))

  (defun jds~slack-download-known-file (file team)
    "Download Slack FILE for TEAM into `slack-file-dir`."
    (let* ((url (jds~slack-file-link-url file)))
      (unless url
        (user-error "No Slack file download URL available"))
      (let* ((dir (file-name-as-directory (expand-file-name slack-file-dir)))
             (path (expand-file-name (jds~slack-file-link-name file) dir)))
        (unless (file-directory-p dir)
          (make-directory dir t))
        (slack-url-copy-file url path team
                             :token (slack-team-token team)
                             :cookie (slack-team-cookie team)
                             :success (lambda ()
                                        (message "Downloaded Slack file to %s" path))))))

  (defun jds~slack-file-link-target-at-point ()
    "Return the Slack file target at point, if any."
    (or (get-text-property (point) 'file-id)
        (get-text-property (point) 'file)
        (get-text-property (point) 'slack-file-url)))

  (defun jds~slack-file-link-at-point-p ()
    "Return the Slack file link at point, if any."
    (jds~slack-file-link-target-at-point))

  (defun jds~slack-file-link-next (bound)
    "Find the next Slack file link before BOUND."
    (let ((positions (delq nil
                           (list (link-hint--next-property 'file-id bound)
                                 (link-hint--next-property 'file bound)
                                 (link-hint--next-property 'slack-file-url bound)))))
      (when positions
        (apply #'min positions))))

  (defun jds~slack-download-file-link (&optional thing &rest _ignore)
    "Download Slack file into `slack-file-dir`."
    (let* ((buffer (bound-and-true-p slack-current-buffer))
           (team (or (and buffer (ignore-errors (slack-buffer-team buffer)))
                     (and (boundp 'slack-current-team) slack-current-team)))
           (thing (jds~slack-file-link-normalize-target thing))
           (file (and (stringp thing)
                      (not (string-match-p "\\`https?://" thing))
                      team
                      (ignore-errors (slack-file-find thing team))))
           (url (and (not file)
                     (jds~slack-file-link-url thing))))
      (unless team
        (user-error "No Slack team available"))
      (cond
       (file
        (jds~slack-download-known-file file team))
       (url
        (let* ((dir (file-name-as-directory (expand-file-name slack-file-dir)))
               (path (expand-file-name (jds~slack-file-link-name thing) dir)))
          (unless (file-directory-p dir)
            (make-directory dir t))
          (slack-url-copy-file url path team
                               :token (slack-team-token team)
                               :cookie (slack-team-cookie team)
                               :success (lambda ()
                                          (message "Downloaded Slack file to %s" path)))))
       ((and (stringp thing) team)
        (message "Fetching Slack file metadata for %s..." thing)
        (slack-file-request-info
         thing 1 team
         (lambda (resolved-file _team)
           (jds~slack-download-known-file resolved-file team))))
       (t
        (user-error "No Slack file at point")))))

  ;; Files show up as `file`/`file-id` text in message and list/info buffers and
  ;; as `slack-file-url` thumbnails in message buffers.
  (link-hint-define-type 'slack-file-link
    :next #'jds~slack-file-link-next
    :at-point-p #'jds~slack-file-link-at-point-p
    :vars '(slack-message-buffer-mode
            slack-thread-message-buffer-mode
            slack-file-list-buffer-mode
            slack-file-info-buffer-mode)
    :describe #'jds~slack-file-link-describe
    :open #'jds~slack-download-file-link
    :open-message "Downloaded"
    :aw-select #'jds~slack-download-file-link
    :aw-select-message "Downloaded")

  (add-to-list 'link-hint-types 'link-hint-slack-file-link))

(jds/localleader-def
  :keymaps 'slack-info-mode-map
  :state 'normal
  "u" #'slack-room-update-messages)

(jds/localleader-def
  :keymaps '(slack-mode-map slack-thread-message-buffer-mode-map)
  :state 'normal
  "+" 'jds/slack-message-add-thumbsup
  "rr" 'slack-message-add-reaction
  "rR" 'slack-message-remove-reaction
  "rs" 'slack-message-show-reaction-users
  "pl" 'slack-room-pins-list
  "pa" 'slack-message-pins-add
  "t" 'slack-thread-show-or-create
  "pr" 'slack-message-pins-remove
  "s" 'slack-search-from-messages
  "fs" 'slack-search-from-files
  "fl" 'slack-file-list
  "'" 'slack-message-write-another-buffer
  "me" 'slack-message-edit
  "md" 'slack-message-delete
  "M" 'slack-message-embed-mention
  "C" 'slack-message-embed-channel
  "q" 'slack-quote-and-reply
  "a" 'slack-file-upload
  "A" 'slack-download-file-at-point
  "l" #'+slack/capture-msg-to-agenda)

(general-define-key
 :keymaps 'slack-mode-map
 :states 'n
 "C-j" 'slack-buffer-goto-next-message
 "C-k" 'slack-buffer-goto-prev-message)

(general-define-key
 :keymaps '(slack-mode-map slack-edit-message-mode-map)
 :states 'i
 "@" 'slack-message-embed-mention
 "#" 'slack-message-embed-channel)


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
