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
(load-config "evil.el")
(load-config "autoloads/textobjects.el")
(load-config "themes.el")
(load-config "completing-read.el")
(load-config "org.el")
(load-config "projects.el")
(load-config "git.el")
(load-config "term.el")
(load-config "system-jump.el")
(load-config "bindings.el")
(load-config "autoloads/window.el")
(load-config "window.el")
(load-config "eval.el")




(provide 'init)
;;; init.el ends here
