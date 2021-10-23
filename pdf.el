;;; pdf.el --- pdf tools setup -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 23, 2021
;; Modified: October 23, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/pdf
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  pdf tools setup
;;
;;; Code:
;;;
;;; Remember you have to call pdf-tools-install to make this work

(use-package pdf-tools
  ;; :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :after evil-collection
  :config
  (pdf-loader-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)

  ;; setup printing
  (setq lpr-command "gtklp")
  (setq ps-lpr-commant "gtklp")

  ;; load my own version of evil-collection-pdf setup that doesn't have
  ;; the SPC binding.
  (evil-collection-pdf-setup))

(jds/localleader-def
 :keymaps '(pdf-view-mode-map)
 "p" #'pdf-misc-print-document)

(provide 'pdf)
;;; pdf.el ends here
