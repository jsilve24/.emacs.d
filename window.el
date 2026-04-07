;;; window.el --- window movement and setup -*- lexical-binding: t; -*-

(load-config "autoloads/window.el")

;;;###autoload
(defun jds/ace-window-save-excursion ()
  "Ace-window but return to window selected when calling function."
  (interactive)
  (let ((window (selected-window)))
    (call-interactively #'ace-window)
    (select-window window)))



(use-package ace-window
  :straight t
  :init
  (setq aw-background nil)
  (setq aw-keys '(?a ?d ?g ?h ?k ?l ?y ?r ?q ?w ?b ?c ?j ?n))
  :config
  ;; get more consistent bindings with my setup everywhere else
  ;; customize movement action
  (setq aw-dispatch-always t
	aw-minibuffer-flag nil)
  (setq aw-dispatch-alist
	'((?x aw-delete-window "Delete Window")
	  (?m aw-swap-window "Swap Windows")
	  (?p aw-move-window "Move Window")
	  (?P aw-delete-and-move-window "Move Window and Delete")
	  (?v aw-move-window-split-right "Move Window to right")
	  (?V aw-delete-and-move-window-split-right "Move Window to right and Delete")
	  (?s aw-move-window-split-below "Move Window below")
	  (?S aw-delete-and-move-window-split-below "Move Window below and Delete")
	  (?f aw-move-window-split-fair "Move Window fair split")
	  (?F aw-delete-and-move-window-split-fair "Move Window fair split")
	  ;; (?c aw-copy-window "Copy Window")
	  ;; (?j aw-switch-buffer-in-window "Select Buffer")
	  ;; (?n aw-flip-window "Jump to previous window")
	  (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
	  (?e aw-execute-command-other-window "Execute Command Other Window")
	  ;; (?= aw-split-window-fair "Split Fair Window")
	  ;; (?s aw-split-window-vert "Split Vert Window")
	  ;; (?v aw-split-window-horz "Split Horz Window")
	  (?o delete-other-windows "Delete Other Windows")
	  ;; (?T aw-transpose-frame "Transpose Frame")
	  ;; ?i ?r ?t are used by hyperbole.el
	  (?? aw-show-dispatch-help)))

  (setq aw-scope 'visible)
  (ace-window-display-mode 1) ; display labels in mode line -- works for x windows
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 2.5 :foreground "red")))))

  ;; from claude to help fix ace-window not showing letters in modeline
  (defun jds/aw-exwm-active-frame-p (frame)
    "Return t if FRAME is an actively displayed EXWM workspace."
    (eq t (frame-parameter frame 'exwm-active)))

  (advice-add 'aw-window-list :filter-return
	      (lambda (windows)
		(cl-remove-if-not
		 (lambda (w) (jds/aw-exwm-active-frame-p (window-frame w)))
		 windows)))

  (advice-add 'aw-update :after
	      (lambda ()
		(walk-windows (lambda (w)
				(with-selected-window w
				  (force-mode-line-update)))
			      nil t)))
  ;; ;; Kill stale buffer-local mode-line-format that predates ace-window-display-mode
  (defun jds/aw-fix-mode-line-format ()
    "Ensure ace-window entry is present in mode-line-format for all buffers."
    (dolist (buf (buffer-list))
      (let ((fmt (buffer-local-value 'mode-line-format buf)))
	(when (and (local-variable-p 'mode-line-format buf)
		   (listp fmt)
		   (not (assq 'ace-window-display-mode fmt)))
	  (with-current-buffer buf
	    (kill-local-variable 'mode-line-format))))))

  (jds/aw-fix-mode-line-format)
  (add-hook 'after-change-major-mode-hook #'jds/aw-fix-mode-line-format))

;; (use-package ace-window-posframe
;;   :straight (ace-window-posframe :type git :host github :repo "abo-abo/ace-window")
;;   :config (ace-window-posframe-mode 1))

;;;###autoload
(defun aw-move-window-split-fair (window)
  "Move current buffer to WINDOW using the standard split policy."
  (let ((buffer (current-buffer)))
    (switch-to-buffer (other-buffer))
    (aw-switch-to-window window)
    (jds/window-split-auto t nil window)
    (switch-to-buffer buffer)))


;;;###autoload
(defun aw-delete-and-move-window-split-fair (window)
  "Delete current window, then move current buffer to WINDOW using standard split policy."
  (let ((buffer (current-buffer)))
    (delete-window)
    (aw-switch-to-window window)
    (jds/window-split-auto t nil window)
    (switch-to-buffer buffer)))


;;;###autoload
(defun aw-delete-and-move-window (window)
  "Move the current buffer to WINDOW, deleting current window. 
Switch the current window to the previous buffer."
  (let ((buffer (current-buffer)))
    (delete-window)
    (aw-switch-to-window window)
    (switch-to-buffer buffer)))

;;;###autoload
(defun aw-move-window-split-right (window)
  "Like the default aw-move-window but splits and puts on right."
  (let ((buffer (current-buffer)))
    (switch-to-buffer (other-buffer))
    (aw-switch-to-window window)
    (jds/window-split-right t nil window)
    (switch-to-buffer buffer)))

;;;###autoload
(defun aw-move-window-split-below (window)
  "Like the default aw-move-window but splits and puts on below."
  (let ((buffer (current-buffer)))
    (switch-to-buffer (other-buffer))
    (aw-switch-to-window window)
    (jds/window-split-below t nil window)
    (switch-to-buffer buffer)))


;;;###autoload
(defun aw-delete-and-move-window-split-right (window)
  "Like the default aw-move-window but splits and puts on right."
  (let ((buffer (current-buffer)))
    (delete-window)
    (aw-switch-to-window window)
    (jds/window-split-right t nil window)
    (switch-to-buffer buffer)))

;;;###autoload
(defun aw-delete-and-move-window-split-below (window)
  "Like the default aw-move-window but splits and puts on below."
  (let ((buffer (current-buffer)))
    (delete-window)
    (aw-switch-to-window window)
    (jds/window-split-below t nil window)
    (switch-to-buffer buffer)))

;; from here https://github.com/abo-abo/ace-window/issues/125
;;;###autoload
(defun aw-previous-window ()
  "Toggle between the last two selected windows."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found."))
    (aw-switch-to-window win)))
