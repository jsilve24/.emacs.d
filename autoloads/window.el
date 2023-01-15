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
  "Split current window horizontally, then focus new window.
If `evil-split-window-below' is non-nil, the new window isn't focused.

With optional arg, don't automatically balance windows."
  (interactive "P")
  (split-window-below)
  (sit-for .01)				; added for exwm buffers
  (other-window 1)
  (unless arg
    (balance-windows)))

;;;###autoload
(defun +evil/window-vsplit-and-follow (&optional arg)
  "Split current window vertically, then focus new window.
If `evil-vsplit-window-right' is non-nil, the new window isn't focused.

With optional arg, don't automatically balance windows."
  (interactive "P")
  (split-window-right)
  (sit-for .01)				; added for exwm buffers
  (other-window 1)
  (unless arg
    (balance-windows)))

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
(defun split-window-sensibly-prefer-horizontal-internal (&optional window)
  "Based on split-window-sensibly, but designed to prefer a horizontal split,
i.e. windows tiled side-by-side."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally
             (with-selected-window window
               (split-window-right)))
	(and (window-splittable-p window)
             ;; Split window vertically
             (with-selected-window window
               (split-window-below)))
	(and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it horizontally disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
	 (not (window-minibuffer-p window))
	 (let ((split-width-threshold 0))
	   (when (window-splittable-p window t)
             (with-selected-window window
               (split-window-right))))))))

;;;###autoload
(defun split-window-sensibly-prefer-horizontal (&optional window norebalance)
"Based on split-window-sensibly, but designed to prefer a horizontal split,
i.e. windows tiled side-by-side. If norebalance then don't automatically rebalance windows after splitting.
This is a wrapper around split-window-sensibly-prefer-horizontal-internal that adds the rebalancing functionality."
(let ((window (split-window-sensibly-prefer-horizontal-internal window)))
  (if window
      (unless norebalance
	(balance-windows)))))

;;;###autoload
(defun jds~new-frame-or-new-window (&optional norebalance)
  "New Frame and Focus unless using EXWM then new window.
If norebalance then don't automatically rebalance windows after split."
  (if (frame-parameter (selected-frame) 'exwm-active)
      (progn
	(let ((split-width-threshold 150)
	      (split-height-threshold 20))
	  (if (not (split-window-sensibly-prefer-horizontal nil norebalance))
	      (unless norebalance 
		(split-window-right)
		(balance-windows))))
	(other-window 1))
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
