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
  (setq aw-keys '(?a ?d ?f ?g ?h  ?k ?l ?y ?p ?r ?q ?w ?b))
  :config
  ;; get more consistent bindings with my setup everywhere else
  (setcar (assoc ?v aw-dispatch-alist) ?s)
  (setcar (assoc ?b aw-dispatch-alist) ?v)
  
  (setq aw-scope 'global)
  (ace-window-display-mode t)		; display labels in mode line -- works for x windows
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 2.5 :foreground "red"))))))


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
