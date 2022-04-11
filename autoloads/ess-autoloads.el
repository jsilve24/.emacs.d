;;; ess-autoloads.el ---  R setup -*- lexical-binding: t; -*-

;;; TIDYVERSE IDE
;;;###autoload
(defun r/insert-assign ()
  "Insert an assignment <-"
  (interactive)
  (just-one-space 1)
  (insert "<-")
  (just-one-space 1))

;;;###autoload
(defun r/insert-pipe ()
  "Insert a %>% and newline"
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (just-one-space 1))

;;;###autoload
(defun r/clear-environment ()
    "Clear the environment by running rm(list=ls())."
  (interactive)
  (ess-eval-linewise "rm(list=ls())"))

;;;###autoload
(defun r/draft-rmd ()
  "Draft a new Rmd file from a template interactively."
  (interactive)
  (setq rmd-file
	(read-from-minibuffer "Rmd Filename (draft_<date>.Rmd): "
			      nil nil t t
			      (format "draft_%s.Rmd"
				      (string-trim
				       (shell-command-to-string "date --iso-8601")))))
  (setq rmd-template
	(read-from-minibuffer
	 (format "Draft %s from template (mmmisc/basic): " rmd-file)
	 nil nil t t "mmmisc/basic"))
  (symbol-name rmd-template)
  (string-match "\\([^/]+\\)/\\([^/]+\\)"
		(symbol-name rmd-template))
  (setq template-pkg
	(substring
	 (symbol-name rmd-template)
	 (match-beginning 1)
	 (match-end 1)))
  (setq template-name
	(substring
	 (symbol-name rmd-template)
	 (match-beginning 2)
	 (match-end 2)))
  (message "Drafting using template %s from package %s" template-name template-pkg)
  (ess-eval-linewise
   (format "rmarkdown::draft(file = \"%s\", template = \"%s\",
		package = \"%s\", edit = FALSE)"
	   rmd-file template-name template-pkg)))

;;;###autoload
(defun r/new-gdev ()
  "create a new graphics device"
  (interactive)
  (ess-eval-linewise "dev.new()"))

;;;###autoload
(defun r/cur-gdev ()
  "return current graphics device"
  (interactive)
  (ess-eval-linewise "dev.cur()"))

;;;###autoload
(defun r/list-all-gdev ()
  "list all graphics devices"
  (interactive)
  (ess-eval-linewise "dev.list()"))

;;;###autoload
(defun r/switch-to-gdev ()
  "Prompt for the number of the graphics device to make current"
  (interactive)
  (setq dev-num
	(read-from-minibuffer "Select R graphics device: "
			      nil nil t t "1"))
  (ess-eval-linewise
   (format "dev.set(%s)" dev-num)))

;;;###autoload
(defun r/switch-next-gdev ()
  "switch to next available graphics device"
  (interactive)
  (ess-eval-linewise "dev.set(dev.next())"))

;;;###autoload
(defun r/switch-prev-gdev ()
  "switch to previous available graphics device"
  (interactive)
  (ess-eval-linewise "dev.set(dev.prev())"))

;;;###autoload
(defun r/save-gdev-pdf ()
  "Save current graphics device as pdf"
  (interactive)
  (ess-eval-linewise "dev.copy2pdf()"))

;;;###autoload
(defun r/capture-gdev ()
  "Capture current graphics device as image"
  (interactive)
  (ess-eval-linewise "dev.capture()"))


;; Devtools
;;;###autoload
(defun r/devtools-setup ()
  "setup R package in current working directory"
  (interactive)
  (ess-eval-linewise "devtools::setup()"))

;; Shiny
;;;###autoload
(defun r/shiny-run-app ()
  "Run a shiny app in the current working directory"
  (interactive)
  (ess-eval-linewise "shiny::runApp()"))

;; Rmarkdowm -- moved to below r/rmd-render command
;; (defun r/rmd-rend ()
;;   "Render rmarkdown files with an interactive selection prompt"
;;   (interactive)
;;   (ess-eval-linewise "mmmisc::rend()"))

;; Data Views
;;;###autoload
(defun df-at-point-to-buffer (&optional numrows)
  "output a sample of another data.frame to and jump to buffer."
  (let ((object (symbol-at-point))
	(r-process (ess-get-process))
	(r-output-buffer (get-buffer-create "*R-output*"))
	(numrows (or numrows 300)))
    (ess-command
     (format "mmmisc::df_preview(%s, %s)\n" object numrows)
     r-output-buffer nil nil nil r-process)
    (switch-to-buffer-other-window r-output-buffer)))

;;;###autoload
(defun r/df-sample-small ()
  "Sample and print 30 rows of a data.frame"
  (interactive)
  (df-at-point-to-buffer 30))

;;;###autoload
(defun r/df-sample-medium ()
  "Sample and print 300 rows of a data.frame"
  (interactive)
  (df-at-point-to-buffer 300))

;;;###autoload
(defun r/df-sample-large ()
  "Sample and print 3000 rows of a data.frame"
  (interactive)
  (df-at-point-to-buffer 3000))

;;; Rmd Mode
;;;###autoload
(defun r/rmd-insert-chunk (header)
  "Insert an r-chunk in markdown mode."
  (interactive "sLabel: ")
  (insert (concat "```{r " header "}\n\n```"))
  (forward-line -1))


;; The following is taken from: https://www.stefanavey.com/lessons/2018/01/04/ess-render
;; Global history list allows Emacs to "remember" the last
;; render commands and propose as suggestions in the minibuffer.
(defvar r~rmd-render-history nil "History list for r/rmd-render.")

;;;###autoload
(defun r/rmd-render (arg)
  "Render the current Rmd file to PDF output.
   With a prefix arg, edit the R command in the minibuffer"
  (interactive "P")
  ;; Build the default R render command
  (setq rcmd (concat "rmarkdown::render('" buffer-file-name "',"
		     "output_dir = '.',"
		     ;; "output_format = 'pdf_document')"))
		     ;; "output_format = 'html_document')"))
		     "output_format = 'all')"))
  ;; Check for prefix argument
  (if arg
      (progn
	;; Use last command as the default (if non-nil)
	(setq prev-history (car r~rmd-render-history))
	(if prev-history
	    (setq rcmd prev-history)
	  nil)
	;; Allow the user to modify rcmd
	(setq rcmd
	      (read-from-minibuffer "Run: " rcmd nil nil 'r~rmd-render-history))
	)
    ;; With no prefix arg, add default rcmd to history
    (setq r~rmd-render-history (add-to-history 'r~rmd-render-history rcmd)))
  ;; Build and evaluate the shell command
  (setq command (concat "echo \"" rcmd "\" | R --vanilla"))
  (compile command))

(provide 'ess-autoloads.el)
