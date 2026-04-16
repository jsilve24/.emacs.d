;;; slack.el --- slack configuration              -*- lexical-binding: t; -*-

(require 'auth-source)
(require 'cl-lib)
(require 'nadvice)
(require 'subr-x)

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

(defun jds/slack-install-stop-advice ()
  (when (and (fboundp 'slack-stop)
             (not (advice-member-p #'jds/slack--slack-stop 'slack-stop)))
    (advice-add 'slack-stop :around #'jds/slack--slack-stop)))

(if (featurep 'slack)
    (jds/slack-install-stop-advice)
  (with-eval-after-load 'slack
    (jds/slack-install-stop-advice)))


(use-package slack
  :straight (:type git :host github :repo "isamert/emacs-slack" :branch "master")
  ;; :disabled t
  :init
  (setq slack-buffer-emojify t
	slack-prefer-current-team t
	slack-file-dir "~/Downloads/")
  ;; just here to avoid issues with autoloads below not sure why this is necessary
  (defun slack-thread-message-buffer ())
  :config
  (defvar jds/slack-team-host "silvermanlabhq.slack.com")
  (defvar jds/slack-team-user "justinsilverman@psu.edu")
  (defvar jds/slack-team-token nil)

  (defun jds/slack-auth-source-password (user &optional suffix)
    "Return the auth-source password for USER on the Slack host.
SUFFIX is appended to USER when present, which is how emacs-slack
distinguishes the cookie entry from the token entry."
    ;; In `~/.authinfo[.gpg]`, quote the cookie secret if it contains spaces or
    ;; semicolons. `auth-source` returns the unquoted string here.
    (let ((password (auth-source-pick-first-password
                     :host jds/slack-team-host
                     :user (if suffix (concat user suffix) user))))
      (when (stringp password)
        password)))

  (defun jds/slack-start-safely ()
    "Start Slack, but keep init alive if the package raises an error."
    (condition-case err
        (slack-start)
      (error
       (message "Slack startup failed: %s" (error-message-string err)))))

  ;; make default same window
  (setq slack-buffer-function #'switch-to-buffer)

  ;; setup team
  (let* ((token (jds/slack-auth-source-password jds/slack-team-user))
         (cookie (jds/slack-auth-source-password jds/slack-team-user "^cookie"))
         (team-plist (append
                      (list :name "silvermanlabhq"
                            :default t
                            :token token
                            :subscribed-channels '((general))
                            :modeline-enabled t
                            :modeline-name "")
                      (when cookie (list :cookie cookie)))))
    (setq jds/slack-team-token token)
    (if token
        (progn
          (apply #'slack-register-team team-plist)
          (cond
           ((and (string-prefix-p "xoxc" token) (null cookie))
            (message "Slack token for %s requires a cookie; skipping auto-start until the cookie is added." jds/slack-team-user))
           ((jds~internet-up-p)
            (jds/slack-start-safely))))
      (message "Slack token not found in auth-source for %s; skipping startup until credentials are added." jds/slack-team-user)))

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
  :straight (ol-emacs-slack :type git :host github :repo "ag91/ol-emacs-slack"))

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
