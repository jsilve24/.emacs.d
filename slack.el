;;; slack.el --- slack configuration              -*- lexical-binding: t; -*-


(use-package slack
  :straight (:type git :host github :repo "isamert/emacs-slack" :branch "fix-curl-downloader")
  :defer t
  :init
  (setq slack-buffer-emojify t
	slack-prefer-current-team t
	slack-file-dir "~/Downloads/")
  ;; just here to avoid issues with autoloads below not sure why this is necessary
  (defun slack-thread-message-buffer ())
  :config
  ;; make default same window
  (setq slack-buffer-function #'switch-to-buffer)

  ;; setup team
  (slack-register-team
   :name "silvermanlabhq"
   :default t
   :token (auth-source-pick-first-password
	   :host "silvermanlabhq.slack.com"
	   :user "justinsilverman@psu.edu")
   :cookie (auth-source-pick-first-password
	    :host "silvermanlabhq.slack.com"
	    :user "justinsilverman@psu.edu^cookie")
   :subscribed-channels '((bacteremia fido-development general random interesting-papers))
   :modeline-enabled t
   :modeline-name "")

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
		     (format " [%s]"
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
  )

(use-package ol-emacs-slack
  :straight (ol-emacs-slack :type git :host github :repo "ag91/ol-emacs-slack"))

(jds/localleader-def
  :keymaps 'slack-info-mode-map
  :state 'normal
  "u" #'slack-room-update-messages)

(jds/localleader-def
  :keymaps 'slack-mode-map
  :state 'normal
  "d" 'slack-buffer-kill
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
  "\\" 'slack-message-send-from-buffer
  "M" 'slack-message-embed-mention
  "C" 'slack-message-embed-channel)

;; dependencies

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

