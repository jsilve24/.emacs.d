;;; latex.el --- latex related config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/latex
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  latex related config
;;
;;; Code:

;;; setup auctex

(use-package auctex
  :straight t;;(auctex :type git :host github :repo "emacs-stright/auctex" :branch "master")
  :defer t
  :config
  (setq TeX-parse-self t ;; parse on load
        TeX-auto-save t  ;; parse on save
        TeX-auto-local ".auctex-auto"
        TeX-style-local ".auctex-style"
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        ;; don't start the emacs server when correlating sources
        TeX-source-correlate-start-server nil
        ;; automatically insert braces after sub/superscript in math mode
        TeX-electric-sub-and-superscript t
        ;; just save, dont ask me before each compilation
        TeX-save-query nil)
  ;; Finally, if you often use \include or \input, you should make AUCTeX aware of the multi-file
  ;; document structure. You can do this by inserting
  ;; however doing this makes it promp for master every time (doom sets this to t)
  (setq-default TeX-master nil)

  ;; set-up chktex -- from doom
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")
  (add-hook 'TeX-mode-hook (lambda ()
                             (setq ispell-parser 'tex
                                   fill-nobreak-predicate (cons #'texmathp fill-nobreak-predicate))))

  ;; Enable word wrapping
  (add-hook 'TeX-mode-hook #'visual-line-mode))



;;; setup cdlatex

(use-package cdlatex
  :hook (LaTeX-mode . cdlatex-mode)
  :hook (org-mode . org-cdlatex-mode)
  :bind
  ;; smartparens takes care of inserting closing delimiters, and if you
  ;; don't use smartparens you probably won't want these also.
  ;; also auctex takes care of inserting _ and ^
  ;; also auctex already provides `LaTeX-insert-item' so C-ret not needed
  (:map cdlatex-mode-map
   ("$" . nil)
   ("(" . nil)
   ("{" . nil)
   ("[" . nil)
   ("|" . nil)
   ("<" . nil)
   ("_" . nil)
   ("^" . nil)
   ([(control return)] . nil)))

;;; setup latexmk

(use-package auctex-latexmk
  :straight t
  :init
  ;; Pass the -pdf flag when TeX-PDF-mode is active
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  ;; Add latexmk as a TeX target
  (auctex-latexmk-setup)
 ;; Set LatexMk as the default
  (add-hook 'latex-mode
            (lambda () (setq TeX-command-default "LatexMk"))))

;;; setup tecosaurs thing...

;;; setup evil-tex

;;; setup bibtex-actions

;;; setup company-auctex and company-reftex (and perhaps company-math)

;;; setup latex-preview plane (perhaps)


;;; setup adaptive-wrap for nicer look

;; Nicely indent lines that have wrapped when visual line mode is activated
(use-package adaptive-wrap
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init (setq-default adaptive-wrap-extra-indent 0))

(provide 'latex)
;;; latex.el ends here
