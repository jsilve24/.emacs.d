;;;###autoload
(defun async-shell-command-no-window (command)
  (interactive)
  (let ((display-buffer-alist
         (list (cons "\\*Async Shell Command\\*.*"
                     (cons #'display-buffer-no-window nil)))))
    (async-shell-command command)))

;;;###autoload
(defun jds/dragon (path)
  "Launch Dragon on PATH. PATH should be escaped already"
  (async-shell-command-no-window
   (concat "~/bin/dragon -a -x " path
           " --and-exit")))

;;;###autoload
(defun jds/dragon-dired ()
  "Launch Dragon on Marked or Current File in Dired/Ranger."
  (interactive)
  (let* ((paths (mapc 'shell-quote-argument (dired-get-marked-files)))
         (path (combine-and-quote-strings paths " ")))
    (jds/dragon path)))


;;;###autoload
(defun jds/make-dated-directory ()
  "Make new directory prefixed with YYYY-mm-dd."
  (interactive)
  (make-directory (concat
                   (format-time-string "%Y-%m-%d")
                   "_"
                   (read-string "Directory Name:"))))

;;;###autoload
(defun jds/dired-jump-and-kill-buffer (&optional arg)
  "Kill current buffer after dired-jump."
  (interactive "P")
  (let ((buffer (current-buffer)))
    (dired-jump)
    (if arg
	(kill-buffer buffer))))

(require 'dash)
(require 'dired-x)
;;;###autoload
(defun jds/dired-jump-reuse-dired (&optional arg)
  "Dired-jump but first check if an existing dired buffer exists and reuse it.
Prefix argument, don't kill prior dired buffers."
  (interactive "P")
  (unless arg 
    (let ((buffers-to-kill (-filter
			    (lambda (buffer) (string= (buffer-local-value 'major-mode buffer) "dired-mode"))
			    (buffer-list))))
      (-map 'kill-buffer buffers-to-kill)))
  (dired-jump))

;;;###autoload
(defun jds/dired-screenshot (&optional filename)
    "Promp for filename, take screenshot and save to current directory."
    (interactive)
    (let* ((fn (if filename
		   filename
		 (read-string "Filename: ")))
	   (fn (expand-file-name fn default-directory))
	   (command (format "import %s" fn)))
      (message command)
      (shell-command command)))


;;;###autoload
(defun jds/screenshot-dragon-temp-file (&optional extension)
  "Take screenshot, save to temporary file name, then present with dragon. Default filetype is PNG.
Prompt for filetype with universal-prefix."
  (interactive "P")
  (let* ((fn (if extension
		 (read-string "Filetype: ")
	       "png"))
	 (fn (concat "." fn))
	 (fn (make-temp-file "screenshot" nil fn)))
    (jds/dired-screenshot fn)
    (jds/dragon fn)))
