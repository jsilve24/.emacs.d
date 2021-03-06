;;; projects.el --- projectile related config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/projects
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  projectile related config
;;
;;; Code:

;;; setup consult-projectile

(straight-use-package 'projectile)
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode +1)

  (setq projectile-other-file-alist
	(append projectile-other-file-alist
		'(("Rmd" "pdf" "html")
		  ("tex" "pdf")))))


(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))




(provide 'projects)
;;; projects.el ends here
