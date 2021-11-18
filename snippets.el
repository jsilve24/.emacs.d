;;; snippets.el --- setup yasnippets and such -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/snippets
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  setup yasnippets and such
;;
;;; Code:

;;; setup consult-yassnippet

(use-package yasnippet
  :straight t
  :ensure t
  :bind ("C-SPC" . yas-expand)
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"))
  (yas-global-mode 1))


;;; abbreviations

(provide 'snippets)
;;; snippets.el ends here
