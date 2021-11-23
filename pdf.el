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
  ;; (setq lpr-command "gtklp")
  ;; (setq ps-lpr-command "gtklp")
  
  ;; see cups help page for lots on how to print with lpr and lp
  (setq pdf-misc-print-program-executable "/usr/bin/lpr"
	pdf-misc-print-program-args (list "-o sides=two-sided-long-edge"
					  ;; "-o fit-to-page"
					  ))
  
  ;; load my own version of evil-collection-pdf setup that doesn't have
  ;; the SPC binding.
  (evil-collection-pdf-setup)

  ;; unbind SPC from pdf-view-mode-map
  (general-define-key
   :keymaps 'pdf-view-mode-map
   "SPC" nil))


;;; print-helper -- not enough to make a stand-alone package
;;;###autoload
(require 'dash)
(defun ph--get-list-of-priters ()
  "Return list of printer with default in position 1."
  (let* ((printers (shell-command-to-string "lpstat -p -d"))
	 (printers (split-string printers "\n"))
	 (default (-filter (lambda (x) (string-match "default destination" x))
		   printers))
	 (printers (-filter (lambda (x) (string-match "printer " x))
		    printers))
	 (default (last (split-string (car default) " ")))
	 (printers (-map (lambda (x) (nth 1 (split-string x " ")))
			 printers))
	 ;; move to front of list
	 (printers (cons (car default) (remove (car default) printers))))))


;;;###autoload
(defun ph-pdf-misc-print-document (&optoinal arg)
  "Wrapper around pdf-misc-print-document that allows you to
  select printer (pulling priter list and default from lpstat -p
  -d command) using completing-read. Also doesn't ask what
  printer to use instead assumes that
  `pdf-misc-print-program-executable' is already set."
  (interactive "P")
  (if arg 
      (let ((printer (completing-read "Choose a printer:" (ph--get-list-of-priters)))
	    (pdf-misc-print-program-args
	     ;; should this not have a space after P?
	     (cons (concat "-P " printer) pdf-misc-print-program-args))) 
	(pdf-misc-print-document
	 (pdf-view-buffer-file-name)
	 ;; dont' prompt for program to print with (nil)
	 nil))
    ;; no prefix -- just use default printer
    (pdf-misc-print-document
     (pdf-view-buffer-file-name)
     ;; dont' prompt for program to print with (nil)
     nil)))



(jds/localleader-def
 :keymaps '(pdf-view-mode-map)
 ;; prefix-argument to select which printer to use
 "p" #'ph-pdf-misc-print-document)



;;; setup printing


(provide 'pdf)
;;; pdf.el ends here
