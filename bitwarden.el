(use-package bitwarden
  :straight (bitwarden :type git :host github :repo "jsilve24/emacs-bitwarden")
  :config
  (setq bitwarden-user "jsilve24@gmail.com")
  (setq bitwarden-api-secret-key
	(plist-get (car (auth-source-search :host "bitwarden.jsilve24.key"))
		   :secret))
  (setq bitwarden-api-client-id
	(plist-get (car (auth-source-search :host "bitwarden.jsilve24.id"))
		   :secret))
  (setq bitwarden-automatic-unlock
	(let* ((matches (auth-source-search :user "jsilve24@gmail.com"
					    :host "bitwarden.jsilve24.login"
					    :require '(:secret)
					    :max 1))
	       (entry (nth 0 matches)))
	  (plist-get entry :secret)))
  (bitwarden-auth-source-enable)
  (bitwarden-login)
  (bitwarden-unlock))


(defvar bitwarden-time-to-store "2 min"
  "Length of time to store last selected username and password before deleting. String should be recognized by the command run-at-time.")

;;;###autoload
(defun bitwarden--parse-hash-to-plist (e)
  "Pass emacs-bitwarden hash-table format to a plist."
  (let* ((login (gethash "login" e)))
    (message (gethash "name" e))
    (if login
	(list (gethash "name" e)
	      (list
	       :username (gethash "username" login)
	       :password (gethash "password" login)))
      nil)))

;;;###autoload
(defun bitwarden-list-completing-read ()
  "A completing read interface built on top of emacs-bitwarden."
  (interactive)
  (let* ((vault (bitwarden-search))
	 (vault (remq nil (mapcar 'bitwarden--parse-hash-to-plist vault)))
	 (select (completing-read "Select:" vault))
	 (select (car (cdr  (assoc select vault)))))
    (if (and  (boundp 'bitwarden--unstore-timer)
	      (timerp bitwarden--unstore-timer))
	(progn 
	  (cancel-timer bitwarden--unstore-timer)
	  (makunbound 'bitwarden--unstore-timer)))
    (setq bitwarden--username (plist-get select :username)
	  bitwarden--password (plist-get select :password)
	  bitwarden--unstore-timer (run-at-time
				    bitwarden-time-to-store nil
				    (lambda ()
				      (makunbound 'bitwarden--username)
				      (makunbound 'bitwarden--password)))))
  (message  "Username and Passwored temporarily stored."))


;;;###autoload
(defun bitwarden-kill-password (&optional arg)
  "Yank password from bitwarden--username.

With prefix argument, repeat completin-read selection even if there was a recent selection (e.g., the variable bitwarden--password is bound)."
  (interactive "P")
  (if (or  (not (boundp 'bitwarden--password))
	   arg)
      (bitwarden-list-completing-read))
  (kill-new bitwarden--password))

;;;###autoload
(defun bitwarden-kill-username (&optional arg)
  "Yank password from bitwarden--username.

With prefix argument, repeat completin-read selection even if there was a recent selection (e.g., the variable bitwarden--username is bound)."
  (interactive "P")
  (if (or  (not (boundp 'bitwarden--username))
	   arg)
      (bitwarden-list-completing-read))
  (kill-new bitwarden--username))



