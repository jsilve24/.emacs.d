;;; calendar.el --- calendar config -*- lexical-binding: t; -*-

;; (use-package org-gcal
;;   :commands (org-gcal-sync org-gcal-fetch org-gcal-post-at-point org-gcal-request-token org-gcal-delete-at-point)
;;   :config
;;   (load-file "~/.org-caldav-secrets.el.gpg")
;;   ;; (setq org-gcal-auto-archive nil)
;;   )
;; ;
					; (add-hook 'after-init-hook #'org-gcal-fetch)

;;;###autoload
(defun jds/calendar--run-sync (name script output-file)
  "Run calendar sync NAME using SCRIPT, then normalize OUTPUT-FILE."
  (if (not (file-executable-p script))
      (message "Skipping %s calendar sync: script missing or not executable (%s)"
	       name script)
    (let ((origin-buffer (current-buffer))
	  (auto-revert-use-notify nil))
      (set-process-sentinel
       (start-process name (format "*%s-output*" name) script)
       (lambda (process _event)
	 (when (eq (process-status process) 'exit)
	   (if (zerop (process-exit-status process))
	       (if (file-exists-p output-file)
		   (with-current-buffer (find-file-noselect output-file)
		     (jds/convert-zoom-url-to-org-link)
		     (save-buffer))
		 (message "Finished %s calendar sync but output file is missing: %s"
			  name output-file))
	     (message "Calendar sync failed for %s (exit %s)"
		      name (process-exit-status process)))
	   (when (buffer-live-p origin-buffer)
	     (switch-to-buffer origin-buffer))))))))

(defcustom jds/calendar-sync-at-startup t
  "When non-nil, schedule local calendar sync jobs after Emacs starts."
  :type 'boolean
  :group 'calendar)

(defcustom jds/calendar-startup-sync-delay 10
  "Seconds to wait before kicking off startup calendar sync jobs.
Delay keeps startup responsive while still syncing early in the session."
  :type 'integer
  :group 'calendar)

(defun jds/calendar--queue-startup-sync (name sync-fn)
  "Queue SYNC-FN for startup when `jds/calendar-sync-at-startup' is non-nil."
  (if (not jds/calendar-sync-at-startup)
      (message "Skipping startup %s calendar sync (jds/calendar-sync-at-startup=nil)" name)
    (run-with-idle-timer
     jds/calendar-startup-sync-delay nil
     (lambda ()
       (condition-case err
	   (funcall sync-fn)
	 (error
	  (message "Startup %s calendar sync failed: %s"
		   name (error-message-string err))))))))

(defun jds/async-exchange-calendar-fetch ()
  (interactive)
  (jds/calendar--run-sync "exchange"
			  (expand-file-name "~/bin/get_exchange_cal.sh")
			  (expand-file-name "~/Dropbox/org/cal-psu.org")))
(add-hook 'after-init-hook
	  (lambda ()
	    (jds/calendar--queue-startup-sync "exchange"
					      #'jds/async-exchange-calendar-fetch)))

;;;###autoload
(defun jds/async-google-calendar-fetch ()
  (interactive)
  (jds/calendar--run-sync "gcal"
			  (expand-file-name "~/bin/get_gcal.sh")
			  (expand-file-name "~/Dropbox/org/cal-gmail.org")))
(add-hook 'after-init-hook
	  (lambda ()
	    (jds/calendar--queue-startup-sync "gcal"
					      #'jds/async-google-calendar-fetch)))
