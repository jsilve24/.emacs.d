;;; dired.el --- dired functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/dired
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  dired functions

;;
;;; Code:

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
(defun ranger-go (path)
  "Go subroutine"
  (interactive
   (list
    (read-char-choice
     "e   : /etc
u   : /usr
d   : ~/Downloads
D   : /dev
l   : follow directory link
L   : follow selected file
o   : /opt
v   : /var
h   : ~/
m   : /media
M   : /mnt
s   : /srv
r,/ : /
R   : ranger . el location
> "
     '(?a ?q ?e ?u ?d ?l ?L ?o ?v ?m ?M ?s ?r ?R ?/ ?h ?g ?D ?j ?k ?T ?t ?n ?c))))
  (message nil)
  (let* ((c (char-to-string path))
         (new-path
          (cl-case (intern c)
            ('e "/etc")
            ('u "/usr")
            ('d "~/Downloads")
            ('D "/dev")
            ('l (file-truename default-directory))
            ('L (file-truename (dired-get-filename)))
            ('o "/opt")
            ('v "/var")
            ('m "/media")
            ('M "/mnt")
            ('s "/srv")
            ('r "/")
            ('R (file-truename (file-name-directory (find-library-name "ranger.el"))))
            ('h  "~/")
            ('/ "/")))
         (alt-option
          (cl-case (intern c)
            ;; Subdir Handlng
            ('a 'ace-window)
            ('j 'ranger-next-subdir)
            ('k 'ranger-prev-subdir)
            ;; Tab Handling
            ('n 'ranger-new-tab)
            ('T 'ranger-prev-tab)
            ('t 'ranger-next-tab)
            ('c 'ranger-close-tab)
            ('g 'ranger-goto-top))))
    (when (string-equal c "q")
      (keyboard-quit))
    (when (and new-path (file-directory-p new-path))
      (ranger-find-file new-path))
    (when (eq system-type 'windows-nt)
      (when (string-equal c "D")
        (ranger-show-drives)))
    (when alt-option
      (call-interactively alt-option))))

;;;###autoload
(defun jds/deer-downloads ()
  "Open deer at ~/Downloads."
  (interactive)
  (progn
    (deer "~/Downloads/")
    (ranger-sort-criteria ?c)
    (ranger-go ?g)))

;;;###autoload
(defun jds/dired-jump-and-kill-buffer ()
  "Kill current buffer after dired-jump."
  (interactive)
  (let ((buffer (current-buffer)))
    (dired-jump)
    (kill-buffer buffer)))

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

(provide 'dired)
;;; dired.el ends here
