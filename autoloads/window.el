;;; window.el --- window functions -*- lexical-binding: t; -*-
;;; functions taken from doom

;; stolen from here: https://www.emacswiki.org/emacs/MiniBuffer#minibuffer
;;;###autoload
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))


;;;###autoload
(defvar jds/window-auto-balance nil
  "When non-nil, rebalance windows after split commands.")

;;;###autoload
(defun jds/window--post-split (new-window follow norebalance)
  "Handle shared post-split behavior for NEW-WINDOW.
If FOLLOW is non-nil, select NEW-WINDOW.  Unless NOREBALANCE is non-nil,
rebalance windows when `jds/window-auto-balance' is non-nil."
  (when new-window
    ;; EXWM windows can lag one event behind without a brief pause.
    (when (frame-parameter (window-frame new-window) 'exwm-active)
      (sit-for .01))
    (when follow
      (select-window new-window))
    (when (and jds/window-auto-balance
               (not norebalance))
      (balance-windows)))
  new-window)

;;;###autoload
(defun jds/window-split-right (&optional follow norebalance window)
  "Split WINDOW to the right and return the new window.
If FOLLOW is non-nil, select the new window.  If NOREBALANCE is non-nil,
skip automatic balancing."
  (interactive)
  (let* ((window (or window (selected-window)))
         (new-window (with-selected-window window
                       (split-window-right))))
    (jds/window--post-split new-window follow norebalance)))

;;;###autoload
(defun jds/window-split-below (&optional follow norebalance window)
  "Split WINDOW below and return the new window.
If FOLLOW is non-nil, select the new window.  If NOREBALANCE is non-nil,
skip automatic balancing."
  (interactive)
  (let* ((window (or window (selected-window)))
         (new-window (with-selected-window window
                       (split-window-below))))
    (jds/window--post-split new-window follow norebalance)))

;;;###autoload
(defun jds/window-split-auto (&optional follow norebalance window)
  "Split WINDOW using the standard split policy and return the new window.
If FOLLOW is non-nil, select the new window.  If NOREBALANCE is non-nil,
skip automatic balancing."
  (interactive)
  (let* ((window (or window (selected-window)))
         (new-window
          (or (with-selected-window window
                (split-window-sensibly window))
              (ignore-errors
                (with-selected-window window
                  (split-window-right))))))
    (jds/window--post-split new-window follow norebalance)))

;;;###autoload
(defun +evil--window-swap (direction)
  "Move current window to the next window in DIRECTION.
If there are no windows there and there is only one window, split in that
direction and place this window there. If there are no windows and this isn't
the only window, use evil-window-move-* (e.g. `evil-window-move-far-left')."
  (when (window-dedicated-p)
    (user-error "Cannot swap a dedicated window"))
  (let* ((this-window (selected-window))
         (this-buffer (current-buffer))
         (that-window (windmove-find-other-window direction nil this-window))
         (that-buffer (window-buffer that-window)))
    (when (or (minibufferp that-buffer)
              (window-dedicated-p this-window))
      (setq that-buffer nil that-window nil))
    (if (not (or that-window (one-window-p t)))
        (funcall (pcase direction
                   ('left  #'evil-window-move-far-left)
                   ('right #'evil-window-move-far-right)
                   ('up    #'evil-window-move-very-top)
                   ('down  #'evil-window-move-very-bottom)))
      (unless that-window
        (setq that-window
              (split-window this-window nil
                            (pcase direction
                              ('up 'above)
                              ('down 'below)
                              (_ direction))))
        (with-selected-window that-window
          (switch-to-buffer (get-buffer "*scratch*")))
        (setq that-buffer (window-buffer that-window)))
      (with-selected-window this-window
        (switch-to-buffer that-buffer))
      (with-selected-window that-window
        (switch-to-buffer this-buffer))
      (select-window that-window))))

;;;###autoload
(defun +evil/window-move-left ()
  "Swap windows to the left."
  (interactive) (+evil--window-swap 'left))
;;;###autoload
(defun +evil/window-move-right ()
  "Swap windows to the right"
  (interactive) (+evil--window-swap 'right))
;;;###autoload
(defun +evil/window-move-up ()
  "Swap windows upward."
  (interactive) (+evil--window-swap 'up))
;;;###autoload
(defun +evil/window-move-down ()
  "Swap windows downward."
  (interactive) (+evil--window-swap 'down))

;;;###autoload
(defun +evil/window-split-and-follow (&optional arg)
  "Split current window below, then focus the new window.
With optional ARG, skip automatic balancing."
  (interactive "P")
  (jds/window-split-below t arg))

;;;###autoload
(defun +evil/window-vsplit-and-follow (&optional arg)
  "Split current window to the right, then focus the new window.
With optional ARG, skip automatic balancing."
  (interactive "P")
  (jds/window-split-right t arg))

;; function to toggle vertical horizontal splits
;; from wiki: https://www.emacswiki.org/emacs/ToggleWindowSplit
;; modified by Justin Silverman to automatically rebalace
;;;###autoload
(defun window-toggle-split-direction (&optional arg)
  "Switch window split from horizontally to vertically, or vice versa.
i.e. change right window to bottom, or change bottom window to right.
With optional arg, don't automatically rebalance windows."
  (interactive "P")
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;; (message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))
    (if (not arg)
	(balance-windows))))

;;;###autoload
;;;###autoload
(defun jds~new-frame-or-new-window (&optional norebalance)
  "New Frame and Focus unless using EXWM then new window.
If norebalance then don't automatically rebalance windows after split."
  (if (frame-parameter (selected-frame) 'exwm-active)
      (progn
        (unless (jds/window-split-auto t norebalance)
          (jds/window-split-right t norebalance))
        (selected-window))
    (select-frame (make-frame))))

;; make sure window really closes when killing exwm buffer
;;;###autoload
(defun jds/kill-buffer-delete-window ()
  "Simpler than kill-buffer-and-window but that was not working for EXWM windows."
  (interactive)
  (if (not (string= major-mode "exwm-mode"))
      (kill-buffer-and-window)
    (let ((buffer (current-buffer))
	  (window (selected-window)))
      (kill-buffer buffer)
      (delete-window window))))


;; stolen from here: https://kfx.fr/e/koe-utils.el
;;;###autoload
(defun jds/quiet-async-shell-commands (cmd &rest cmds)
  "Run async shell CMD with optional CMDS in a oneliner silently."
  (interactive)
  (let
      ((display-buffer-alist
	(list
	 (cons
	  "\\*Async Shell Command\\*.*"
	  (cons #'display-buffer-no-window nil))))
       ;; don't ask for confirmation before running in new buffer
       (async-shell-command-buffer 'new-buffer))
    (async-shell-command
     (concat cmd " " (string-join cmds " ")))))

;; largely templated off of the exwm cookbook (on the github wiki)
;;;###autoload
(defun jds~set-window-dedicated (&optional arg)
  "Toggle loose window dedication.  If prefix ARG, set strong."
  (interactive "P")
  (let* ((dedicated (if arg t (if (window-dedicated-p) nil "loose"))))
    (message "setting window dedication to %s" dedicated)
    (set-window-dedicated-p (selected-window) dedicated)))
