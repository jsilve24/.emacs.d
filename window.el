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


(use-package ace-window
  :straight t
  :init
  (setq aw-background nil)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (setq aw-scope 'global)
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
