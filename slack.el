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

(jds/localleader-def
  :keymaps 'slack-info-mode-map
  :state 'normal
  "u" #'slack-room-update-messages)

(jds/localleader-def
  :keymaps 'slack-mode-map
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
  "A" 'slack-download-file-at-point)

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
