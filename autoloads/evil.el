;;; autoloads/evil.el --- random evil stuff -*- lexical-binding: t; -*-

;; from doom
;;;###autoload
(defun +evil/reselect-paste ()
  "Return to visual mode and reselect the last pasted region."
  (interactive)
  (cl-destructuring-bind (_ _ _ beg end &optional _)
      evil-last-paste
    (evil-visual-make-selection
     (save-excursion (goto-char beg) (point-marker))
     end)))

;; from spacemacs
;;;###autoload
(defun jds/evil-insert-line-above (count)
  "Insert one or several lines above the current point's line without changing
the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

;;;###autoload
(defun jds/evil-insert-line-below (count)
  "Insert one or several lines below the current point's line without changing
the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))


;;;###autoload
(defun jds/evil-ex-compress-outline ()
  "Removes Justin's + PX: style outline format and makes text ready to insert into a plain-text document."
  (interactive)
  (let ((visual-state (evil-visual-state-p)))
    (evil-ex "g/\\+ P/s/.*//g")
    (if visual-state
	(progn
	  (evil-visual-restore)
	  (evil-ex "'<,'>s/\\+ //g"))
      (evil-ex "%s/\\+ //g"))))

