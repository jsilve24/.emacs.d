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

  ;; (setq projectile-other-file-alist
  ;; 	(append projectile-other-file-alist
  ;; 		'(("Rmd" "pdf" "html")
  ;; 		  ("tex" "pdf"))))
  )


(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))


;;; better switch to other file (simpler at least) -----------------------------

(use-package ruled-switch-buffer
  :straight (ruled-switch-buffer :type git :host github :repo "kzkn/ruled-switch-buffer")
  :config

  (defun ruled-switch-buffer-other-window ()
    "ruled-switch-buffer but in other-window"
    (interactive)
    (let* ((buffer (current-buffer))
	   (window (selected-window)))
      (ruled-switch-buffer)
      (let* ((new-buffer (current-buffer)))
	(other-window 1)
	(switch-to-buffer new-buffer))
      (other-window 1)
      (switch-to-buffer buffer)
      (other-window 1)))

  ;; latex
  (ruled-switch-buffer-define tex-to-pdf
    :matcher (lambda (fn) (string-match ".tex$" fn))
    :mappers (lambda (fn) (replace-regex-in-string "\\.tex$" ".pdf" fn)))

  ;; rmd
  (ruled-switch-buffer-define rmd-to
    :matcher (lambda (fn) (string-match ".[rR]md$" fn))
    :mappers ((lambda (fn) (replace-regexp-in-string "\\.[rR]md$" ".pdf" fn))
	      (lambda (fn) (replace-regexp-in-string "\\.[rR]md$" ".html" fn))))

  ;; org
  (ruled-switch-buffer-define org-to
    :matcher (lambda (fn) (string-match ".org$" fn))
    :mappers ((lambda (fn) (replace-regexp-in-string "\\.org$" ".pdf" fn))
	      (lambda (fn) (replace-regexp-in-string "\\.org$" ".html" fn))
	      (lambda (fn) (replace-regexp-in-string "\\.org$" ".tex" fn))))

  ;; rules for c/c++
  (ruled-switch-buffer-define h-to-c
    :matcher (lambda (fn) (string-match ".h$" fn))
    :mappers ((lambda (fn) (replace-regexp-in-string "\\.h$" ".c" fn))
	      (lambda (fn) (replace-regexp-in-string "\\.h$" ".cc" fn))
	      (lambda (fn) (replace-regexp-in-string "\\.h$" ".cxx" fn))))

  (ruled-switch-buffer-define c-to-h
    :matcher (lambda (fn) (string-match ".c\\(c\\|xx\\)?$" fn))
    :mappers (lambda (fn) (replace-regexp-in-string "\\.c.*$" ".h" fn))))


(provide 'projects)
;;; projects.el ends here
