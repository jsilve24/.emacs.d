;;; window.el --- window movement and setup -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/window
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  window movement and setup
;;
;;; Code:

;;;###autoload
(defun jds/ace-window-save-excursion ()
  "Ace-window but return to window selected when calling function."
  (interactive)
  (let ((window (selected-window)))
    (call-interactively #'ace-window)
    (select-window window)))

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

(use-package ace-window
  :straight t
  :init
  (setq aw-background nil)
  (setq aw-keys '(?a ?d ?f ?g ?h  ?k ?l ?y ?r ?q ?w ?b))
  :config
  ;; get more consistent bindings with my setup everywhere else
  ;; customize movement action
  (setq aw-dispatch-always t
	aw-minibuffer-flag nil)
  (setq aw-dispatch-alist
  '((?x aw-delete-window "Delete Window")
    (?m aw-swap-window "Swap Windows")
    (?p aw-move-window "Move Window")
    (?v aw-move-window-split-right "Move Window to right")
    (?s aw-move-window-split-below "Move Window below")
    (?F aw-move-window-split-fair "Move Window fair split")
    (?c aw-copy-window "Copy Window")
    (?j aw-switch-buffer-in-window "Select Buffer")
    (?n aw-flip-window "Jump to previous window")
    (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
    (?e aw-execute-command-other-window "Execute Command Other Window")
    ;; (?= aw-split-window-fair "Split Fair Window")
    ;; (?s aw-split-window-vert "Split Vert Window")
    ;; (?v aw-split-window-horz "Split Horz Window")
    (?o delete-other-windows "Delete Other Windows")
    (?T aw-transpose-frame "Transpose Frame")
    ;; ?i ?r ?t are used by hyperbole.el
    (?? aw-show-dispatch-help)))
  
  (setq aw-scope 'visible)
  (ace-window-display-mode t)		; display labels in mode line -- works for x windows
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 2.5 :foreground "red"))))))

;;;###autoload
(defun aw-move-window-split-fair (window)
  "Like the default aw-move-window but splits based on target window dimension.
Controlled by `aw-fair-aspect-ratio'."
  (let ((buffer (current-buffer))
	(w (window-body-width window))
	(h (window-body-height window)))
    (aw-switch-to-window window)
    (if (< (* h aw-fair-aspect-ratio) w)
	(aw-split-window-horz window)
      (aw-split-window-vert window))
    (call-interactively #'other-window)
    (switch-to-buffer buffer)))

;;;###autoload
(defun aw-move-window-split-right (window)
  "Like the default aw-move-window but splits and puts on right."
  (let ((buffer (current-buffer)))
    (switch-to-buffer (other-buffer))
    (aw-switch-to-window window)
    (split-window-right)
    (call-interactively #'other-window)
    (switch-to-buffer buffer)))

;;;###autoload
(defun aw-move-window-split-below (window)
  "Like the default aw-move-window but splits and puts on below."
  (let ((buffer (current-buffer)))
    (switch-to-buffer (other-buffer))
    (aw-switch-to-window window)
    (split-window-below)
    (call-interactively #'other-window)
    (switch-to-buffer buffer)))


;; from here https://github.com/abo-abo/ace-window/issues/125
;;;###autoload
(defun aw-previous-window ()
  "Toggle between the last two selected windows."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found."))
    (aw-switch-to-window win)))


(use-package transpose-frame)

;; save and restore window and frame configurations
(use-package burly)

;; delete-frame when delete-window called and window is last one
;; just loading this package advises the function delete-window
;; don't want this if using exwm
;; (use-package frame-cmds
;;   :straight (frame-cmds :type git :host github :repo "emacsmirror/frame-cmds")
;;   :ensure t)

;; (defadvice delete-window (around delete-frame-if-one-win activate)
;;   "If WINDOW is the only one in its frame, then `delete-frame' too."
;;   (if (fboundp 'with-selected-window)   ; Emacs 22+
;;       (with-selected-window
;;           (or (ad-get-arg 0)  (selected-window))
;;         (if (one-window-p t) (delete-frame) ad-do-it))
;;     (save-current-buffer
;;       (select-window (or (ad-get-arg 0)  (selected-window)))
;;       (if (one-window-p t) (delete-frame) ad-do-it))))


;;; how to handle last window in frame



(provide 'window)
;;; window.el ends here
