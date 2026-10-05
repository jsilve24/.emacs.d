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

;;; caldav emacssync -----------------------------------------------------------
(defun jds/calendar--install-davmail-auth (&rest _ignored)
  "Load the PSU password into Emacs's in-memory DavMail auth cache.

The encrypted source remains the single persistent copy of the password."
  (require 'subr-x)
  (require 'url-auth)
  (let* ((password-file (expand-file-name "~/.psu_mailpass.gpg"))
         (server-port "127.0.0.1:1080")
         (realm "DavMail Gateway")
         (password
          (with-temp-buffer
            ;; `epa-file' decrypts the existing .gpg file through its file
            ;; handler; the plaintext exists only in this temporary buffer.
            (insert-file-contents password-file)
            (string-trim (buffer-string))))
         (encoded
          (base64-encode-string
           (concat "jds6696@psu.edu:" password) t)))
    (when (string-empty-p password)
      (user-error "PSU password file is empty: %s" password-file))
    ;; `url-basic-auth-storage' names the actual cache variable.  Preloading
    ;; this realm avoids a second encrypted password copy in authinfo.
    (set url-basic-auth-storage
         (cons (list server-port (cons realm encoded))
               (assoc-delete-all
                server-port (symbol-value url-basic-auth-storage))))))

;; Keep org-caldav as an iCalendar conversion dependency only.  Its sync
;; engine assumes that CalDAV hrefs and iCalendar UIDs are identical, which is
;; not true for events created through DavMail/Exchange.
(use-package org-caldav :demand t)
(load-config "calendar-exchange.el")
;; Enabled only after isolated create, no-op, update, import, conflict, and
;; deletion tests succeeded against the dedicated EmacsSync calendar.
(setq jds/exchange-calendar-allow-full-sync t)
