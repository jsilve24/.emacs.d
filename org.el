;;; org.el --- org related config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jds6696@psu.edu>
;; Created: October 21, 2021
;; Modified: October 21, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/org
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  org related config org setup
;;
;;; Code:



;; get newest version of org
(straight-use-package 'org)
(straight-use-package 'org-contrib)

(use-package org
  ;; :commands (org-agenda)
  :init
  ;;; hack to avoid
  ;;; org-element-cache-map: Symbol’s function definition is void: native-comp-available-p
  (defun native-comp-available-p () nil)
  
  :config
  (setq org-directory "~/Dropbox/org")

  ;; don't include files in .attach
  (setq org-agenda-files
	(seq-filter
	 (lambda (x) (not (string-match-p (rx "\.attach") x)))
	 (directory-files-recursively "~/Dropbox/org/" "\\.org$")))


  ;;; high level config
  (setq org-default-notes-file "~/Dropbox/org/inbox.org")

  ;;; make org-ret follow inks
  (setq org-return-follows-link t)

  
  ;; setup org-habit
  ;; (require 'org-habit)
  ;; (add-to-list 'org-modules 'org-habit)

  (setq org-complete-tags-always-offer-all-agenda-tags t
        org-tags-column -100)

  (setq org-hide-leading-stars t)

  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  (setq org-startup-indented t
	org-startup-truncated nil)

  ;; hide blank lines in folded views
  (setq org-cycle-separator-lines 0)
  ;; Prevent creating blank lines before headings, allow list items to adapt to existing blank lines around the items:
  (setq org-blank-before-new-entry (quote ((heading)
                                           (plain-list-item . auto))))

  (add-hook 'org-trigger-hook 'save-buffer)

  ;; Prevent editing invisible (folded) text
  (setq org-catch-invisible-edits 'error)

  ;;; setup refile

  ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
				   (org-agenda-files :maxlevel . 9))))

  ;; make it work nicely with vertico
  (setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
  

  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  
  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-refile-target-verify-function 'bh/verify-refile-target)

  ;;; setup agenda

  ;; More visible current time in Agenda
  (setq org-agenda-current-time-string ">>>>>>>>>> NOW <<<<<<<<<<")

  (setq org-agenda-start-day "0d")

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w)" "HOLD(h)" "|" "MEETING(m)")))

  (setq org-todo-keyword-faces
        '(("TODO" :foreground "orange" :weight bold)
          ("NEXT" :foreground "red" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
          ("MEETING" :foreground "forest green" :weight bold)))


  (setq org-agenda-compact-blocks nil)
  (setq org-agenda-block-separator nil)
  (setq org-priority-default ?C) ;; needed for proper ordering of next block

  (setq org-agenda-custom-commands
        `(("d" "Custom Day View"
           ((agenda "" ((org-agenda-span 'day)
                        ;; (org-agenda-overriding-header "Timestamped")
                        (org-agenda-skip-function '(lambda () (interactive) (skip-tag "mail")))))
            (todo "NEXT" ((org-agenda-sorting-strategy `(priority-down effort-down))
                          (org-agenda-overriding-header "Next")
                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
            (agenda "" ((org-agenda-span 'day)
                        (org-agenda-overriding-header "Mail")
                        (org-agenda-format-date "")
                        (org-agenda-skip-function '(lambda () (interactive) (skip-not-tag "mail")))))
            (tags-todo "REFILE" ((org-agenda-overriding-header "To Refile")
                            (org-tags-match-list-sublevels nil)))))))


  ;; Remove completed deadline and scheduled tasks from the agenda view
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)

  ;;; setup capture

  (add-hook 'org-capture-before-finalize-hook (lambda () (org-align-tags t)))
  (setq org-capture-templates
        `(("t" "todo")
          ("ta" "todo with attachment" entry (file "~/Dropbox/org/inbox.org")
           "* TODO %?\n %U\n %a")
          ("te" "todo tweaks" entry (file+headline "~/Dropbox/org/tasks.org" "Emacs")
           "* TODO %? :TWEAKS:\n %U")
          ("tt" "plain todo" entry (file "~/Dropbox/org/inbox.org")
           "* TODO %?\n %U")
          ("m" "meetings")
          ("ma" "meeting anarres" entry (file "~/Dropbox/org/meetings_anarres.org")
           "* MEETING with %? :MEETING:\n %U"
           :jump-to-captured t)
          ("mp" "meeting psu" entry (file "~/Dropbox/org/meetings_psu.org")
           "* MEETING with %? :MEETING:\n %U"
           :jump-to-captured t)
          ("mP" "meeting specific person")
          ("mPm" "meeting Michelle" entry (file "~/Dropbox/org/meetings_psu.org")
           ,(concat "* MEETING with Michelle :MEETING:w_michelle:\n"
                    "  %U\n"
                    "** TRAM :p_tram:\n"
                    "** Bacteremia :p_bacteremia:\n"
                    "** Farhani\n"
                    "** ARL :p_arl:")
           :jump-to-captured t)
          ("mPd" "meeting DIHI" entry (file "~/Dropbox/org/meetings_psu.org")
           ,(concat "* MEETING with DIHI :MEETING:w_michelle:p_bacteremia:\n"
                    "  %U\n"
                    "  %?")
           :jump-to-captured t)
          ("mPk" "meeting kyle" entry (file "~/Dropbox/org/meetings_psu.org")
           ,(concat "* MEETING with Kyle :MEETING:w_kyle:\n"
                    "  %U\n"
                    "** cGSEA :p_cgsea:\n"
                    "  %?")
           :jump-to-captured t)

          ("c" "calendar event")
          ("cc" "plain event" entry (file+headline "~/Dropbox/org/calendar.org" "Calendar")
           "* %? \n %^T")
          ("ca" "event with attachment" entry (file+headline "~/Dropbox/org/calendar.org" "Calendar")
           "* %? \n %^T\n %a")
          ("n" "note" entry (file+headline "~/Dropbox/org/notes.org" "Notes")
           "* NOTE %? :NOTE:\n %U")
	  ("e" "email" entry (file+headline "~/Dropbox/org/mail.org" "Email")
	   "* TODO %:fromname: %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))"
	   :immediate-finish t)))

  ;; make some templates only available in some modes
  (setq org-capture-templates-contexts '(("e" ((in-mode . "message-mode")
					       (in-mode . "mu4e-headers-mode")
					       (in-mode . "mu4e-view-mode")))))
  
  ;;; appearnace customizatoins
  (setq org-ellipsis " ▾")

  ;; don't wrap lines in org-agenda
  (add-hook 'org-agenda-mode-hook  (lambda ()
                                     (visual-line-mode -1)))

  ;; org-agenda window setup (don't always split frame)
  (setq org-agenda-window-setup 'current-window)

  (add-hook 'window-state-change-hook (lambda ()
					(if (string= major-mode "org-agenda-mode")
						(org-agenda-redo)
					      nil)))

  
  ;; for some reason this was needed when I first put this config together
  (org-reload))


;;; lazyness -- use evil-org
(use-package evil-org
  :straight (evil-org :local-repo "~/.myvanilla.d/local-packages/evil-org-mode")
  :hook (org-mode . evil-org-mode)
  :hook (org-capture-mode . evil-insert-state)
  :hook (org-agenda-mode . evil-org-agenda-set-keys)
  :init

  ;; hack-fix for https://github.com/Somelauw/evil-org-mode/issues/93
  (fset 'evil-redirect-digit-argument 'ignore) 
  (add-to-list 'evil-digit-bound-motions 'evil-org-beginning-of-line)
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
  (evil-org-set-key-theme '(textobjects insert additional todo)) ;removed heading, navigation, and shift
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))




;;; local bindings

(general-define-key
 :states '(n v m)
 :keymaps 'org-agenda-mode-map
 "f" #'avy-org-agenda
 "F" #'jds/avy-org-agenda-and-jump)

(jds/localleader-def
  :keymaps 'org-capture-mode-map
  "\\" #'org-capture-finalize
  "k"  #'org-capture-kill
  "r"  #'org-capture-refile
  "a"  #'org-attach)


;; get q

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
  "t"  #'org-todo
  "s"  #'org-screenshot-take
  "a"  #'org-attach
  "q"  #'org-set-tags-command
  "d"  '(:ignore t :wk "date")
  "dd" #'org-deadline
  "ds" #'org-schedule
  "dt" #'org-time-stamp
  "dT" #'org-time-stamp-inactive)

(jds/localleader-def
  :keymaps 'org-capture-mode-map
  "r" #'org-capture-refile)

(jds/localleader-def
  :keymaps 'org-mode-map
  "r"  #'org-refile
  "n"  #'org-narrow-to-subtree
  "N"  #'widen)


  ;; (map! :after (org evil-org)
  ;;       :map org-capture-mode-map
  ;;       :localleader
  ;;                "\\" #'org-capture-finalize
  ;;                "k"  #'org-capture-kill
  ;;                "r"  #'org-capture-refile
  ;;       (:prefix ("d" . "date")
  ;;                "d"  #'org-deadline
  ;;                "s"  #'org-schedule)
  ;;                "a"  #'org-capture-attach
  ;;                "t"  #'org-set-tags-command
  ;;       (:prefix ("i" . "insert")
  ;;                "l"  #'org-insert-link
  ;;                "L"  #'org-insert-all-links))



(provide 'org)
;;; org.el ends here
