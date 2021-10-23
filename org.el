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
  :config
  (setq org-directory "~/Dropbox/org")
  (setq org-agenda-files (directory-files-recursively "~/Dropbox/org/" "\\.org$"))


  ;;; high level config
  (setq org-default-notes-file "~/Dropbox/org/inbox.org")

  ;; setup org-habit
  ;; (require 'org-habit)
  ;; (add-to-list 'org-modules 'org-habit)

  (setq org-complete-tags-always-offer-all-agenda-tags t
        org-tags-column -100)

  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  (setq org-startup-indented t)

  ;; hide blank lines in folded views
  (setq org-cycle-separator-lines 0)
  ;; Prevent creating blank lines before headings, allow list items to adapt to existing blank lines around the items:
  (setq org-blank-before-new-entry (quote ((heading)
                                           (plain-list-item . auto))))

  (add-hook 'org-trigger-hook 'save-buffer)

  ;; Prevent editing invisible (folded) text
  (setq org-catch-invisible-edits 'error)

  ;;; setup refile

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
  (setq org-agenda-block-separator " ")
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
                    "** Mouse Project with Laura\n"
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
           "* NOTE %? :NOTE:\n %U")))

  ;;; appearnace customizatoins
  (setq org-ellipsis " ▾"))


;;; local bindings

(general-define-key
 :states '(n v)
 :keymaps 'org-agenda-mode-map
 "f" #'avy-org-agenda
 "F" #'jds/avy-org-agenda-and-jump)

(provide 'org)
;;; org.el ends here
