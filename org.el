;;; org.el --- org related config -*- lexical-binding: t; -*-

(load-config "autoloads/org.el")

(use-package org
  ;; :straight (org-mode :local-repo "~/Downloads/org-mode/")
  ;; :ensure t
  ;; :straight (org-mode :host github :repo "emacs-straight/org-mode")
  :diminish org-indent-mode
  :diminish org-cdlatex-mode
  :config


  ;; don't include files in .attach
  (setq org-agenda-files
	(seq-filter
	 (lambda (x) (and (not (string-match-p (rx "\.attach") x))
			  (not (string-match-p (rx "\.#") x))
			  (not (string-match-p (rx "ShoeTracking") x))
			  (not (string-match-p (rx "emacs cheatsheet") x))
			  (not (string-match-p (rx "meetings_anarres") x))))
	 (directory-files "~/Dropbox/org/" t "\\.org$")))

  (setq org-directory "~/Dropbox/org")

;;; high level config
  (setq org-default-notes-file "~/Dropbox/org/inbox.org")

  ;; latex highlighting
  (setq org-highlight-latex-and-related '(latex entities))

;;; make org-ret follow inks
  (setq org-return-follows-link t)

  ;; store link to files when attaching
  (setq org-attach-store-link-p 'attached)

  ;; setup org  attach directory so that I can easily refile
  (setq org-attach-directory "~/Dropbox/org/.attach")

  (setq org-complete-tags-always-offer-all-agenda-tags t
	org-tags-column -70)

  (setq org-hide-leading-stars t)

  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  (setq org-startup-indented t
	org-startup-truncated nil)

  ;; startup with drawers folded
  (defun jds~org-fold-drawers ()
    (org-cycle-hide-drawers 'all))
  (add-hook 'org-mode-hook #'jds~org-fold-drawers)
  (add-hook 'org-mode-hook (lambda ()
			     (run-with-idle-timer
			      20 nil
			      (lambda ()
				(let ((mod (buffer-modified-p)))
				  (org-align-all-tags)
				  (if mod nil
				    (save-buffer)))))))


  ;; allow alphabetical lists
  (setq org-list-allow-alphabetical t)

  ;; hide blank lines in folded views
  (setq org-cycle-separator-lines 0)
  ;; Prevent creating blank lines before headings, allow list items to adapt to existing blank lines
  ;; around the items:
  (setq org-blank-before-new-entry '((heading . auto)
				     (plain-list-item . auto)))

  ;; (add-hook 'org-trigger-hook 'save-buffer)

  ;; Prevent editing invisible (folded) text
  (setq org-catch-invisible-edits 'error)

;;; setup refile

  ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (setq org-refile-targets '((nil :maxlevel . 9)
			     (org-agenda-files :maxlevel . 9)))

  ;; make it work nicely with vertico
  (setq org-refile-use-outline-path 'file
	org-outline-path-complete-in-steps nil)

  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-refile-target-verify-function 'bh/verify-refile-target)

;;; setup agenda

  ;; More visible current time in Agenda
  (setq org-agenda-current-time-string ">>>>>>>>>> NOW <<<<<<<<<<")

  (setq org-agenda-start-day "0d")

  (setq org-todo-keywords
	'((sequence "TODO(t)" "WAITING(w)" "NEXT(n)" "PROJ(p)" "|" "DONE(d)" "HOLD(h)")))
  (setq org-todo-keyword-faces
	'(("TODO" :foreground "orange" :weight bold)
	  ("NEXT" :foreground "red" :weight bold)
	  ("DONE" :foreground "forest green" :weight bold)
	  ("PROJ" :foreground "light blue" :weight bold)
	  ("WAITING" :foreground "orange" :weight bold)
	  ("HOLD" :foreground "magenta" :weight bold)))
  ;; setup stuck-projects definitions
  (setq org-stuck-projects
	'("/PROJ"
	  ("NEXT" "TODO")
	  nil				; Tags that define a stuck project
	  "SCHEDULED:"))		; regex that denotes a not stuck project

  (setq org-agenda-compact-blocks nil)
  (setq org-agenda-block-separator nil)
  (setq org-priority-default ?C) ;; needed for proper ordering of next block

  ;; don't show same agenda item twice if two different time-stamps
  (setq org-agenda-skip-additional-timestamps-same-entry t)

  (setq org-agenda-custom-commands
	`(("d" "Custom Day View"
	   ((agenda "" ((org-agenda-span 'day)
			;; (org-agenda-overriding-header "Timestamped")
			(org-agenda-skip-function '(lambda () (interactive) (skip-tag "mail")))))
	    (todo "NEXT" ((org-agenda-sorting-strategy `(priority-down effort-down))
			  (org-agenda-overriding-header "\nNext\n")
			  (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
	    (agenda "" ((org-agenda-span 'day)
			(org-agenda-overriding-header "\nMail")
			(org-agenda-format-date "")
			(org-agenda-skip-function '(lambda () (interactive) (skip-not-tag "mail")))))
	    (tags-todo "REFILE" ((org-agenda-overriding-header "\nTo Refile\n")
				 (org-tags-match-list-sublevels nil)))))
	  ("n" "Next Items"
	   ((todo "NEXT" ((org-agenda-sorting-strategy `(priority-down effort-down))
			  (org-agenda-overriding-header "Next")))))))


  ;; Remove completed deadline and scheduled tasks from the agenda view
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)

;;; setup capture

  (require 'org-protocol)

  (add-hook 'org-capture-before-finalize-hook (lambda () (org-align-tags t)))
  (setq org-capture-templates
	`(("t" "todo")
	  ("ta" "todo with attachment" entry (file "~/Dropbox/org/inbox.org")
	   "* TODO %?\n %U\n %a")
	  ("te" "todo resource" entry (file+headline "~/Dropbox/org/resources.org" "Inbox")
	   "* TODO %? \n %U")
	  ("tt" "plain todo" entry (file "~/Dropbox/org/inbox.org")
	   "* TODO %?\n %U")
	  ("m" "meetings")
	  ("ma" "meeting anarres" entry (file+olp+datetree "~/Dropbox/org/meetings_anarres.org")
	   "* MEETING with %? - %u :MEETING:\n %U"
	   :jump-to-captured t
	   :tree-type month)
	  ("mp" "meeting psu" entry (file+olp+datetree "~/Dropbox/org/meetings_psu.org")
	   "* MEETING %u with %? :MEETING:\n "
	   :jump-to-captured t
	   :tree-type month)
	  ("mP" "meeting specific person")
	  ("mPm" "meeting Michelle" entry (file+olp+datetree "~/Dropbox/org/mtx-michelle.org")
	   ,(concat "* MEETING %u with Michelle :MEETING:w_michelle:\n"
		    "** TRAM Manuscript :p_tram:\n"
		    "** ALDEx2 Manuscript :p_tram:\n"
		    "** SRI Covariance :p_covariance:\n"
		    "** Decision Theory :p_bdt:\n"
		    "** Future Plans\n"
		    )
	   :jump-to-captured t
	   :tree-type month)
	  ("mPx" "meeting Maxwell" entry (file+olp+datetree "~/Dropbox/org/mtx-maxwell.org")
	   ,(concat "* MEETING %u with Maxwell :MEETING:w_maxwell:\n"
		    "** Future Plans\n"
		    )
	   :jump-to-captured t
	   :tree-type month)
	  ("mPa" "meeting Andrew" entry (file+olp+datetree "~/Dropbox/org/mtx-andrew.org")
	   ,(concat "* MEETING %u with Andrew :MEETING:w_andrew:\n  %?")
	   :jump-to-captured t
	   :tree-type month)
	  ("mPt" "meeting Tinghua" entry (file+olp+datetree "~/Dropbox/org/mtx-tinghua.org")
	   ,(concat "* MEETING %u with Tinghua :MEETING:w_tinghua:\n"
		    "** Fido Additive GPs :p_addgp:\n"
		    "** Causal Inference :p_tram:\n"
		    "** Future Plans \n")
	   :jump-to-captured t
	   :tree-type month)
	  ("mPd" "meeting DIHI" entry (file+olp+datetree "~/Dropbox/org/mtx-michelle.org")
	   ,(concat "* MEETING %u with DIHI :MEETING:w_michelle:p_bacteremia:\n"
		    "  %?")
	   :jump-to-captured t
	   :tree-type month)
	  ("mPk" "meeting Kyle" entry (file+olp+datetree "~/Dropbox/org/mtx-kyle.org")
	   ,(concat "* MEETING %u with Kyle :MEETING:w_kyle:\n"
		    "** Interval Null :p_intervalnull:\n"
		    "** ALDEx2 Mixed Effects :p_aldexme:\n"
		    "** Future Plans \n"
		    "  %?")
	   :jump-to-captured t
	   :tree-type month)
	  ("mPw" "meeting Won" entry (file+olp+datetree "~/Dropbox/org/mtx-won.org")
	   ,(concat "* MEETING %u with Won :MEETING:w_won:\n"
		    "  %?")
	   :jump-to-captured t
	   :tree-type month)
	  ;; ("mPz" "meeting Zhao Ma" entry (file+olp+datetree "~/Dropbox/org/mtx-zhaoma.org")
	  ;; ,(concat "* MEETING %u with Zhao Ma :MEETING:w_zhao:\n"
	  ;; "  %?")
	  ;; :jump-to-captured t
	  ;; :tree-type month)
	  ("c" "calendar event")
	  ("cc" "plain event" entry (file+headline "~/Dropbox/org/calendar.org" "Calendar")
	   "* %? \n %^T")
	  ("ca" "event with attachment" entry (file+headline "~/Dropbox/org/calendar.org" "Calendar")
	   "* %? \n %^T\n %a")
	  ("n" "note" entry (file+headline "~/Dropbox/org/notes.org" "Notes")
	   "* %? \n %U")
	  ("P" "Protocol")
	  ("Pw" "Capture Website" entry (file "~/Dropbox/org/inbox.org")
	   "* TODO %:annotation \n %i %?")))

  ;; make some templates only available in some modes
  ;; (setq org-capture-templates-contexts '(("e" ((in-mode . "message-mode")
  ;; (in-mode . "mu4e-headers-mode")
  ;; (in-mode . "mu4e-view-mode")))))

;;; appearance customizations
  (setq org-ellipsis " ▾")

  ;; don't wrap lines in org-agenda
  (add-hook 'org-agenda-mode-hook (lambda ()
				    (visual-line-mode -1)))

  ;; org-agenda window setup (don't always split frame)
  (setq org-agenda-window-setup 'current-window)

  ;; setup org latex export
  (setq org-latex-pdf-process (list "latexmk -shell-escape -f -pdf %f"))

  ;; setup async export
  (setq org-export-async-init-file
	(expand-file-name "org-async-init.el" user-emacs-directory)
	;; default to export in background
	org-export-in-background t)

  ;; active Babel languages
  (load-file "~/.emacs.d/ob-stan.el")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (emacs-lisp . t)
     (latex . t)
     (org . t)
     (shell . t)
     (stan . t)))

  (setq org-src-fontify-natively t
	org-export-with-smart-quotes t
	;; don't raise error for broken links
	org-export-with-broken-links 'mark)

  ;; get syntax highlighting working
  ;; taken from here: http://joonro.github.io/blog/posts/org-mode-outputdir-minted-latex-export/
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-src-block-backend 'minted)
  (setq org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


  ;; colorize code-blocks in org-export to latex
  ;; (add-to-list 'org-latex-packages-alist '("" "color"))
  ;; taken from here: https://stackoverflow.com/questions/46438516/how-to-encapsualte-code-blocks-into-a-frame-when-exporting-to-pdf/60396939#60396939
  (setq org-latex-minted-options
	'(;; ("bgcolor" "bg") ;; this line was just making the background color black... 
	  ("frame" "lines")))

  ;; avoid this bug: https://www.reddit.com/r/emacs/comments/u0lk2w/orgbabelexecutesrcblock_with_results_value_giving/
  (add-to-list 'org-export-backends 'org t)

  ;; add R as a designation to link to ess-r-mode
  ;; (add-to-list 'org-src-lang-modes '("R" . ess-r))

  ;; stop asking for confirmation to run src blocks
  (setq org-confirm-babel-evaluate nil)

  ;; better org-src buffer window placement
  (setq org-src-window-setup 'current-window)

  ;; allow binding in oeg export 
  ;; https://tex.stackexchange.com/questions/637386/org-mode-export-bold-text-to-beamer
  (setq org-export-allow-bind-keywords t)

  ;; org-mode allow resizing inline previews
  ;; from here: https://emacs.stackexchange.com/questions/30559/org-mode-inline-image-display-size
  ;; with this you can use #+ATTR_ORG: :width 400 to resize inline previews
  (setq org-image-actual-width nil)

  ;; nicer latex previews (slightly larger)
  (plist-put org-format-latex-options :scale 1.15)

  ;; don't always have math mode on in org
  (add-hook 'org-cdlatex-mode-hook
	    (lambda () (advice-remove 'texmathp 'org--math-always-on)))
  

  ;; for some reason this was needed when I first put this config together
  (org-reload))


;; not sure this is needed and it seemed to be trying to download a different version of emacs...
(straight-use-package '(org-contrib :type git :host github :repo "emacsmirror/org-contrib"))
(use-package org-screenshot
  :after org
  :config
  (setq org-screenshot-file-name-format "screenshot-%XXXXXXXXXX.png"))

;; make it easier to edit fontified/hidden text (I mostly just use this for org-links)
(use-package org-appear
  :after org
  :config
  ;; (setq org-hide-emphasis-markers t)

  ;; The manual option is useful for, e.g., integrating org-appear with evil-mode. Below is an
  ;; example configuration for toggling elements in Insert mode only. Note that org-appear expects
  ;; to be enabled in Org mode buffers only, which is why the example attaches evil-mode hooks to
  ;; the Org mode startup hook.
  (setq org-appear-trigger 'manual)
  (add-hook 'org-mode-hook (lambda ()
			     (add-hook 'evil-insert-state-entry-hook
				       #'org-appear-manual-start
				       nil
				       t)
			     (add-hook 'evil-insert-state-exit-hook
				       #'org-appear-manual-stop
				       nil
				       t)))
  (setq org-appear-autolinks t)
  (add-hook 'org-mode-hook 'org-appear-mode))


;;; Better latex org previews 
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package org-preview
  :straight (org-preview :type git :host github :repo "karthink/org-preview" :fork t)
  :disabled 
  :config
  (org-preview-mode 1))


;;; Setup Appointment / Calendar Notifications ---------------------------------

(require 'appt)
(appt-activate t)

(defun fw/org-agenda-to-appt ()
 "Rebuild all appt reminders using org."
 (interactive)
 (setq appt-time-msg-list nil)
 (org-agenda-to-appt))

(fw/org-agenda-to-appt)
(add-hook 'org-agenda-finalize-hook 'fw/org-agenda-to-appt)


;;; evil integration -----------------------------------------------------------
(use-package evil-org
  :straight (evil-org :type git :host github :repo "Somelauw/evil-org-mode"
		      :fork t)
  :diminish evil-org-mode
  :after evil
  :hook (org-mode . evil-org-mode)
  :hook (org-capture-mode . evil-insert-state)
  :hook (org-agenda-mode . evil-org-agenda-set-keys)
  :init

  (evil-define-key 'motion 'evil-org-mode
    (kbd "0") 'evil-org-beginning-of-line)
  (evil-define-key 'normal 'evil-org-mode
    (kbd "0") 'evil-org-beginning-of-line)

  (defvar evil-org-retain-visual-state-on-shift t)
  (defvar evil-org-special-o/O '(table-row item))
  (defvar evil-org-use-additional-insert t)
  :config
  (setq org-special-ctrl-a/e t
	evil-org-retain-visual-state-on-shift t)
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  (evil-org-set-key-theme '(textobjects insert additional todo)) ; removed heading, navigation, and shift
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


;;; org-super-links ------------------------------------------------------------
(use-package org-super-links
  :straight (org-super-links :type git :host github :repo "toshism/org-super-links" :branch "develop"))

;;;###autoload
(defun jds/org-super-links-link-or-delete (&optional arg)
  "Delete if prefixed otherwise insert link."
  (interactive "P")
  (if arg
      (org-super-links-delete-link)
    (org-super-links-link)))

;; use org-ids for links
(with-eval-after-load 'org
  (require 'org-id)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

;;; local bindings -------------------------------------------------------------

;;; undoing some messed up bindings from somewhere, not sure where these came from
(general-define-key
 :states '(n v m i e)
 :keymaps 'org-mode-map
 "M-<return>" #'org-meta-return
 "M-S-<return>" #'org-insert-todo-heading
 ;; "C-<return>" #'+org/insert-item-below
 "C-<return>" #'org-insert-heading-respect-content
 "C-S-<return>" #'org-insert-todo-heading-respect-content
 ;; "C-S-<return>" #'+org/insert-item-above
 )


(general-define-key
 :states '(n v m)
 :keymaps 'org-agenda-mode-map
 "f" #'avy-org-agenda
 "F" #'jds/avy-org-agenda-and-jump)

(jds/localleader-def
  :keymaps 'org-capture-mode-map
  "m" #'org-capture-finalize
  "C-m" #'org-capture-finalize
  "k" #'org-capture-kill
  "r" #'org-capture-refile
  "a" #'org-attach)


(jds/localleader-def
  :keymaps 'org-agenda-mode-map
  "d" '(ignore t :wk "date")
  "dd" #'org-agenda-deadline
  "ds" #'org-agenda-schedule
  "t"  #'org-agenda-todo
  "q"  #'org-agenda-set-tags
  "r" #'(lambda () (interactive) (jds/save-excursion-and-min-point #'org-agenda-refile)))

(jds/localleader-def
  :keymaps '(org-capture-mode-map org-mode-map)
  "t" #'org-todo
  "b" #'org-beamer-select-environment
  "F" #'org-sparse-tree
  "s" org-babel-map
  "S" #'org-screenshot-take
  "a" #'org-attach
  "q" #'org-set-tags-command
  "d" '(:ignore t :wk "date")
  "dd" #'org-deadline
  "ds" #'org-schedule
  "dt" #'org-time-stamp
  "dT" #'org-time-stamp-inactive
  ;; "c"  jds/citation-map ; reserved
  "'" #'org-edit-special)

(jds/localleader-def
  :keymaps 'org-src-mode-map
  :states '(me)
  "'" #'org-edit-src-exit
  "k" #'org-edit-src-abort)


(jds/localleader-def
  :keymaps 'org-capture-mode-map
  "r" #'org-capture-refile)

(jds/localleader-def
  :keymaps 'org-mode-map
  "A"  #'org-archive-subtree
  "r"  #'org-refile
  "R" #'jds/org-refile-current-buffer
  "e"  #'org-export-dispatch
  "n"  #'org-narrow-to-subtree
  "N"  #'widen
  "p"  #'org-set-property
  "b"  #'org-beamer-select-environment
  "o"  #'jds/evil-ex-compress-outline
  ","  #'jds/super-link-at-point-capture
  "i"  #'org-insert-structure-template)

(jds/localleader-def
  :keymaps 'org-beamer-mode-map
  "m" #'(lambda () (interactive) (org-beamer-export-to-pdf
				  org-export-in-background)))

(general-define-key
 :keymaps 'calendar-mode-map
 :states '(nv)
 "a" #'org-calendar-goto-agenda)

;; get better latex math pairs in org
(general-define-key
 :keymaps '(org-mode-map org-capture-mode-map)
 :states 'insert
 "(" #'LaTeX-insert-left-brace
 "[" #'LaTeX-insert-left-brace
 "{" #'LaTeX-insert-left-brace)

(provide 'config-org)
;;; org.el ends here

