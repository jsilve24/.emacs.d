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
(defun jds~new-frame-or-new-window ()
  "New Frame and Focus unless using EXWM then new window."
    (if (frame-parameter (selected-frame) 'exwm-active)
      (progn
	(let ((split-width-threshold 20)
	      (split-height-threshold 20))
	  (if (not (split-window-sensibly))
	      (split-window-right)))
	(call-interactively #'other-window))
    (select-frame (make-frame))))

(use-package ace-window
  :straight t
  :init
  (setq aw-background nil)
  (setq aw-keys '(?a ?d ?f ?g ?h  ?k ?l ?y ?r ?q ?w ?b))
  :config
  ;; get more consistent bindings with my setup everywhere else
  ;; customize movement action


  (setq aw-dispatch-alist
  '((?x aw-delete-window "Delete Window")
    (?m aw-swap-window "Swap Windows")
    (?p aw-move-window "Move Window")
    (?v aw-move-window-split-right "Move Window to right")
    (?s aw-move-window-split-below "Move Window below")
    (?= aw-move-window-split-fair "Move Window fair split")
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
  
  (setq aw-scope 'global)
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

(use-package transpose-frame)

;; save and restore window and frame configurations
(use-package burly)

;; delete-frame when delete-window called and window is last one
;; just loading this package advises the function delete-window
(use-package frame-cmds
  :straight (frame-cmds :type git :host github :repo "emacsmirror/frame-cmds")
  :ensure t)

(provide 'window)
;;; window.el ends here
