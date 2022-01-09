;;; slack.el --- slack configuration              -*- lexical-binding: t; -*-


(use-package slack
  :straight (:type git :host github :repo "isamert/emacs-slack" :branch "fix-curl-downloader")
  :commands slack-start 
  :init
  (setq slack-buffer-emojify t
	slack-prefer-current-team t)
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
  (add-to-list 'company-backends 'company-slack-backend)
  ;; setup tab command - bind over lui mode completion 
  ;; (add-hook 'slack-mode-hook 'jds/completion-keys)
  (setq lui-completion-function 'company-complete)

  ;; link-hint for files
  ;; Mandatory Keywords :next should be a function that returns the position of
  ;; the next link after the point (i.e. if there is a link at the point, it should not return the
  ;; point). It should take one argument that corresponds to the end bound for searching. Also, it
  ;; should not move the point.

  ;; :at-point-p should be a function that returns a non-nil value if there is a link at the point.
  ;; Its return value can be used in the action functions.
  ;; (defun jds~slack-at-file-p ()
  ;;   (get-text-property (point) 'file))

  ;; (defun jds~slack-next-file (end)
  ;;   (if (jds~slack-at-file-p)
  ;; 	nil
  ;;     (save-excursion
  ;; 	(re-search-forward "open in browser" end t)
  ;; 	(backward-word 4)
  ;; 	(point))))
  ;; (link-hint-define-type 'slack-file
  ;;   :next #'jds~slack-next-file
  ;;   :at-point-p #'jds~slack-at-file-p
  ;;   :open #'slack-file-display)
  ;; (push 'link-hint-slack-file link-hint-types)

  )

(with-eval-after-load 'slack
  (with-eval-after-load 'org
    ;; this code from here: https://ag91.github.io/blog/2020/09/12/org-mode-links-for-emacs-slack/
    ;; custom org-link type
    (org-link-set-parameters "emacs-slack"
			     :follow #'ol/slack-follow-link
			     :export #'ol/slack-export
			     :store #'ol/slack-store-link)))

;;;###autoload
(defun ol/slack-export (link description format)
  "Export a emacs-slack link from Org files."
  (let ((desc (or description link)))
    desc))

;;;###autoload
(defun ol/slack-store-link ()
  "Store a link to a man page."
  (when (eq major-mode 'slack-message-buffer-mode)
    (let* ((buf slack-current-buffer)
	   (team (slack-buffer-team buf))
	   (team-name (oref team name))
	   (room (slack-buffer-room buf))
	   (room-name (slack-room-name room team))
	   (link (funcall
		  slack-message-notification-title-format-function
		  team-name
		  room-name
		  (cl-typep buf 'slack-thread-message-buffer)))
	   (description))
      (org-link-store-props
       :type "emacs-slack"
       :link (concat "emacs-slack:" link)
       :description (concat "Slack message in #" room-name)))))

;;;###autoload
(defun ol/slack-follow-link (link)
  "Follow LINK with format `   team - channel'."
  (let* ((team (--> link
		    (s-split "-" it)
		    first
		    s-trim))
	 (team-object (ol/slack-string-to-team team)))
    (slack-room-display (ol/slack-string-to-room team-object link) team-object)))

;;;###autoload
(defun ol/slack-room-select (room rooms team)
  "Select ROOM from ROOMS and TEAM."
  (let* ((alist (slack-room-names
		 rooms team #'(lambda (rs) (cl-remove-if #'slack-room-hidden-p rs))))
	 (selected (cdr (cl-assoc room alist :test 'ol/room-name-equal))))
    selected))

;;;###autoload
(defun ol/room-name-equal (room channel-room)
  "Check ROOM is equal to CHANNEL-ROOM."
  (string=
   (s-downcase (s-trim room))
   (s-downcase
    (let ((trimmed (s-trim (s-chop-prefix " * " channel-room))))
      (if (> (length trimmed) (length room))
	  (substring trimmed 0 (length room))
	trimmed)))))


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
