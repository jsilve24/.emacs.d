;;; init.el --- Justin Silverman Config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jds6696@psu.edu>
;; Created: October 20, 2021
;; Modified: October 20, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Justin Silverman Config
;;  With inspiration from
;;      emacs-from-scratch
;;      doom emacs
;;      spacemacs
;;
;;; Code:

;;; Simple macros

(defun load-config (fn)
  (load (expand-file-name fn user-emacs-directory)))

;;; Core
(load-config "core.el")
(load-config "wm.el")
(load-config "evil.el")
(load-config "autoloads/evil.el")
(load-config "autoloads/textobjects.el")
(load-config "themes.el")
;; (load-config "splash.el")
(load-config "popups.el")
(load-config "completing-read.el")
(load-config "company.el")
(load-config "org.el")
(load-config "autoloads/org.el")
(load-config "projects.el")
(load-config "git.el")
(load-config "term.el")
(load-config "system-jump.el")
(load-config "avy.el")
(load-config "autoloads/avy.el")
(load-config "autoloads/window.el")
(load-config "window.el")
(load-config "eval.el")
(load-config "dired.el")
(load-config "autoloads/dired.el")
(load-config "spelling.el")
(load-config "latex.el")
(load-config "pdf.el")
(load-config "autoloads/email.el")
(load-config "email.el")
(load-config "calendar.el")
(load-config "autoloads/movement.el")
;; (load-config "frames.el")
(load-config "editor.el")
(load-config "snippets.el")
;; ;; after everything else
(load-config "bindings.el")


;;; languages
(load-config "python.el")
(load-config "ess.el")
(load-config "autoloads/ess-autoloads.el")

;; Start Server if not already running
(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

(setq-default fill-column 100)


;;; update package archives to include melpa
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;; miscellaneous

(use-package comment-dwim-2
  :straight (comment-dwim-2 :type git :host github :repo "remyferre/comment-dwim-2" :branch "master")
  :commands (comment-dwim-2 org-comment-dwim-2)
  :bind (("M-;" . comment-dwim-2)
         :map org-mode-map
         ("M-;" . org-comment-dwim-2)))


;; easy way to try out packages
;; M-x try RET package-name
;; (use-package try
;;   :straight t)

(use-package beacon
  :straight t
  :config
  (add-to-list 'beacon-dont-blink-major-modes 'kisses-mode)
  (beacon-mode 1))


(setq epg-pinentry-mode 'loopback)


;;; profiling and debugging
(use-package esup
  :ensure t
  :pin melpa
  :config
  (setq esup-depth 0))


(provide 'init)
;;; init.el ends here
(put 'narrow-to-page 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.5 :foreground "red")))))
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
