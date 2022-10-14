;;; pdf.el --- pdf tools setup -*- lexical-binding: t; -*-


(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :after evil-collection
  :config
  ;; install pdf-tools, don't ask for confirmation
  (pdf-loader-install t)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  ;; (setq pdf-annot-activate-created-annotations t)

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
   "SPC" nil)

  (defun pdf-misc-print-document (filename &optional interactive-p)
    "Print the PDF doc FILENAME.

`pdf-misc-print-program' handles the print program, which see for
definition of INTERACTIVE-P."
    (interactive
     (list (pdf-view-buffer-file-name) t))
    (cl-check-type filename (and string (satisfies file-readable-p)))
    (let* ((program pdf-misc-print-program-executable)
	   (args (string-join (append pdf-misc-print-program-args (list (shell-quote-argument filename))) " "))
	   (cmd (string-join (list program args) " ")))
      ;; (message args)
      (unless program
	(error "No print program available"))
      (start-process-shell-command "printing" nil cmd)
      ;; (apply #'start-process "printing" nil program args)
      (message "Print job started: %s" cmd))))

;;; print-helper -- not enough to make a stand-alone package
(require 'dash)
;;;###autoload
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
	 (printers (cons (car default) (remove (car default) printers))))
    printers))


;;;###autoload
(defun ph-pdf-misc-print-document (&optional arg)
  "Wrapper around pdf-misc-print-document that allows you to
  select printer (pulling priter list and default from lpstat -p
  -d command) using completing-read. Also doesn't ask what
  printer to use instead assumes that
  `pdf-misc-print-program-executable' is already set."
  (interactive "P")
  (if arg
      (let* ((printer (completing-read "Choose a printer:" (ph--get-list-of-priters)))
	     (pdf-misc-print-program-args
	      ;; should this not have a space after P?
	      ;; (add-to-list  'pdf-misc-print-program-args (concat "-P " printer))
	      (append pdf-misc-print-program-args (list (concat "-P " printer)))))
	(pdf-misc-print-document
	 (pdf-view-buffer-file-name)
	 ;; dont' prompt for program to print with (nil)
	 nil))
    ;; no prefix -- just use default printer
    (pdf-misc-print-document
     (pdf-view-buffer-file-name)
     ;; dont' prompt for program to print with (nil)
     nil)))


;;; try out lp-transient package -----------------------------------------------

(use-package transient-extras-lp
  :straight (transient-extras-lp :type git :host github :repo "haji-ali/transient-extras"))



(jds/localleader-def
  :keymaps '(pdf-view-mode-map)
  ;; prefix-argument to select which printer to use
  "p" #'ph-pdf-misc-print-document
  "P" #'transient-extras-lp-menu
  "l" #'pdf-annot-list-annotations)

(general-define-key
 :states '(normal)
 :keymaps '(pdf-annot-list-mode-map)
 "RET" #'pdf-annot-list-display-annotation-from-id
 "M-RET" #'tablist-find-entry)
