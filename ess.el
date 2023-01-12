;;; ess.el ---  R setup -*- lexical-binding: t; -*-

;; setup projectile
(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files "DESCRIPTION"))

;;; main ess setup
(use-package ess
  :mode (("\\.[rR]\\'" . ess-r-mode))
  :commands R ess-r-mode
  :init (require 'ess-site)
  :config
  (setq ess-offset-continued 'straight
	ess-nuke-trailing-whitespace-p t
	ess-style 'RStudio
	ess-eval-visibly 'nowait
	ess-use-flymake nil)

  ;; most people say this is annoying -- I need to figure out what it is eventually
  (setq ess-smart-S-assign-key nil)

  ;; ess don't ask for startup directory
  (setq ess-ask-for-ess-directory nil
	ess-startup-directory 'default-directory)

  (add-hook 'ess-r-mode-hook #'display-fill-column-indicator-mode)

  ;; from here: https://github.com/SteveLane/dot-emacs/blob/master/ess-config.el
  ;; Add in company-mode helpers
  ;; (defun my-ess-company-hook ()
  ;; ;; ensure company-R-library is in ESS backends
  ;; (make-variable-buffer-local 'company-backends)
  ;; (cl-delete-if (lambda (x) (and (eq (car-safe x) 'company-R-args))) company-backends)
  ;; (add-to-list 'company-backends
  ;; '(company-R-args company-R-objects company-R-library
  ;; company-dabbrev-code :separate)))
  ;; (add-hook 'ess-mode-hook #'my-ess-company-hook)
  ;; 
  ;; (defun jds~ess-capf-hook ()
  ;; ;; ensure company-R-library is in ESS backends
  ;; (make-variable-buffer-local 'completion-at-point-functions)
  ;; (add-to-list 'company-backends (mapcar #'cape-company-to-capf
  ;; '(#'company-R-args
  ;; #'company-R-objects
  ;; #'company-R-library)))
  ;; (add-to-list 'company-backends
  ;; '(company-R-args company-R-objects company-R-library
  ;; company-dabbrev-code :separate)))
  ;; (add-hook 'ess-mode-hook #'my-ess-company-hook)


  ;; this also makes heading section headers available in consult-outline
  ;; (add-hook 'ess-mode-hook
  ;; '(lambda ()
  ;; (outline-minor-mode)
  ;; (setq outline-regexp ".*----$")))
  ;; turn off fancy comments which are really annoying
  (defun dont-like-fancy ()
    (setcdr (assoc 'ess-indent-with-fancy-comments (cdr (assoc 'DEFAULT ess-style-alist))) nil))
  (add-hook 'ess-mode-hook 'dont-like-fancy)
  (add-hook 'ess-r-mode-hook 'dont-like-fancy)

  ;; add dumb-jump as the default xref-backend
  (with-eval-after-load 'dumb-jump
    (add-hook 'ess-r-mode-hook
	      (lambda ()
		(add-hook 'xref-backend-functions #'eglot-xref-backend -100 'local)
		(add-hook 'xref-backend-functions #'dumb-jump-xref-activate -90 'local))))


  ;; better display-buffer defaulsts
  ;; (add-to-list 'display-buffer-alist '("^\\*R Dired"
  ;; (display-buffer-reuse-window display-buffer-in-side-window)
  ;; (side . right)
  ;; (slot . -1)
  ;; (window-width . 0.33)
  ;; (reusable-frames . nil)))

  ;; (add-to-list 'display-buffer-alist '("^\\*R"
  ;; (display-buffer-reuse-window display-buffer-in-side-window)
  ;; (side . right)
  ;; (slot . -1)
  ;; (window-width . 0.5)
  ;; (reusable-frames . nil)))

  ;; ;; (add-to-list 'display-buffer-alist '("^\\*Help"
  ;; ;; 				       (display-buffer-reuse-window display-buffer-in-side-window)
  ;; ;; 				       (side . right)
  ;; ;; 				       (slot . 1)
  ;; ;; 				       (window-width . 0.33)
  ;; ;; 				       (reusable-frames . nil)))

  ;; (add-to-list 'display-buffer-alist '("^R_x11"
  ;; (display-buffer-reuse-window display-buffer-in-side-window)
  ;; (side . right)
  ;; (slot . 1)
  ;; (window-width . 0.33)
  ;; (reusable-frames . nil)))


  ;; dont ask for startup directory just starup in wherever the script is.
  ;; (setq  ess-startup-directory nil
  ;; ess-ask-for-ess-directory nil)
  )


;;; setup polymode

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "markdown"))

(use-package polymode
  :ensure markdown-mode
  :ensure poly-R
  :ensure poly-noweb
  :config
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode)))


(use-package poly-markdown
  :ensure polymode
  :defer t)


;; poly-R
(use-package poly-R
  :ensure polymode
  :ensure poly-markdown
  :ensure poly-noweb
  :defer t)


;; Add yaml to markdown an .yml files
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)))

;;; ess-breakerofchains

(use-package ess-breakerofchains
  :straight (ess-breakerofchains
	     :type git :host github :repo "jsilve24/ess-breakerofchains"))
(general-define-key
 :keymaps 'ess-r-mode-map
 :states '(insert normal)
 "<C-S-return>" #'ess-boc-break-chain)


;;; ess add library import
(defun jds/ess-add-library-import (library-name)
  "Add a 'library' import statement for LIBRARY-NAME to the current R script buffer.

The function searches backwards from the end of the buffer
for the regular expression '^library(.*)$', which matches a
'library' import statement. If a match is found, the function
moves the point to the end of the matched line and inserts a
newline and a 'library' import statement for LIBRARY-NAME.
If no match is found, the function opens a new line above the
first line in the buffer and inserts a 'library' import statement
for LIBRARY-NAME.

The function raises an error if the current buffer is not an
R script buffer (i.e. the major mode is not `ess-r-mode`)."
  (interactive "sLibrary name: ")
  (if (eq major-mode 'ess-r-mode)
      (let ((library-import-regexp "^library(.*)$")
	    (proc (ess-get-process))
	    (fstring (format "library(%s)\n" library-name)))
	(save-excursion 
	  (goto-char (point-max))
	  (if (re-search-backward library-import-regexp nil t)
              (progn
		(goto-char (match-beginning 0))
		(match-string-no-properties 0)
		(end-of-line)
		(newline)
		(insert fstring))
	    (goto-char (point-min))
	    (beginning-of-line)
	    (newline)
	    (forward-line -1)
	    (insert fstring)))
	(if proc
	    (ess-send-string proc fstring)))
    (error "Buffer is not in ess-r-mode")))



;;; key bindings

(general-define-key
 :keymaps 'inferior-ess-mode-map
 "C-l" #'comint-clear-buffer)


(jds/localleader-def
  :keymaps '(ess-mode-map inferior-ess-mode-map)
  "<tab>" '(ess-switch-to-inferior-or-script-buffer :wk "REPL-script switch")
  "vs" #'r/df-sample-small
  "vm" #'r/df-sample-medium
  "vl" #'r/df-sample-large
  "l"  #'jds/ess-add-library-import
  ;; predefined keymaps
  "h" 'ess-doc-map
  "x" 'ess-extra-map
  "p" 'ess-r-package-dev-map
  "q" 'ess-dev-map
  "oo" #'ess-roxy-update-entry
  "oc" #'ess-roxy-toggle-roxy-region
  "ot" #'ess-roxy-preview-text
  "ow" #'ess-roxy-preview-HTML
  "on" #'ess-roxy-next-entry
  "op" #'ess-roxy-previous-entry
  "oh" #'ess-roxy-hide-all
  "cc" #'r/clear-environment)

(general-define-key
 :keymaps '(polymode-mode-map markdown-mode-map)
 "C-S-i"  #'r/rmd-insert-chunk)

(jds/localleader-def
  :keymaps '(polymode-mode-map markdown-mode-map)
  "rc" #'r/rmd-insert-chunk
  "rr" #'r/rmd-render
  "m" #'r/rmd-render
  "rd" #'r/draft-rmd
  "re" #'polymode-eval-chunk
  "rn" #'polymode-next-chunk)

(jds/localleader-def
  :keymaps 'ess-mode-map
  "'" #'org-edit-src-exit
  "k" #'org-edit-src-abort)


(general-define-key
 :keymaps 'ess-mode-map
 :states 'i
 [C-return] #'ess-eval-line-and-step)


(general-define-key
 :keymaps 'ess-mode-map
 :states '(n m)
 [C-return] #'ess-eval-function-or-paragraph-and-step)

(general-define-key
 :keymaps 'ess-mode-map
 :states 'v
 ;; leave point where it was originally
 [C-return] #'(lambda (start end vis) (interactive "r\nP")
		(let* ((mark (mark)))
                  (ess-eval-region start end vis)
		  (goto-char mark))))

  ;; (map! (:after ess-help
  ;;         (:map ess-help-mode-map
  ;;           :n "q"  #'kill-current-buffer
  ;;           :n "Q"  #'ess-kill-buffer-and-go
  ;;           :n "K"  #'ess-display-help-on-object
  ;;           :n "go" #'ess-display-help-in-browser
  ;;           :n "gO" #'ess-display-help-apropos
  ;;           :n "gv" #'ess-display-vignettes
  ;;           :m "]]" #'ess-skip-to-next-section
  ;;           :m "[[" #'ess-skip-to-previous-section)
  ;;         (:map ess-doc-map
  ;;           "h"    #'ess-display-help-on-object
  ;;           "p"    #'ess-R-dv-pprint
  ;;           "t"    #'ess-R-dv-ctable
  ;;           [up]   #'comint-next-input
  ;;           [down] #'comint-previous-input
  ;;           :i   [C-return] #'ess-eval-line-and-step
  ;;           :nom [C-return] #'ess-eval-function-or-paragraph-and-step
  ;;           :v    [C-return] #'ess-eval-region))
  ;;       (:after ess
  ;;        :map ess-mode-map
  ;;        :localleader
  ;;        "'" #'R
  ;;        [backtab] #'ess-switch-process
  ;;        ;; noweb
  ;;        :prefix "c"
  ;;        "C" #'ess-eval-chunk-and-go
  ;;        "c" #'ess-eval-chunk
  ;;        "d" #'ess-eval-chunk-and-step
  ;;        "m" #'ess-noweb-mark-chunk
  ;;        "p" #'ess-noweb-previous-chunk
  ;;        "n" #'ess-noweb-next-chunk)))

(provide  'ess)

