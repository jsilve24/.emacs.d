;;; bindings.el -- only for general purpose high-level bindings -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jds6696@psu.edu>
;; Created: October 20, 2021
;; Modified: October 20, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/bindings
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; universal argument and other short stuff
(jds/leader-def
 "u" #'universal-argument
 ";" #'pp-eval-expression)

;;; filesystem bindings
(jds/leader-def
 "f" '(:ignore t :which-key "files")
 "ff" #'find-file
 "fr" #'consult-recent-file
 "fs" #'save-buffer
 "fa" #'(lambda () (interactive) (affe-find "/home/jds6696/"))
 "fS" #'evil-write-all
 "fz" #'zoxide-find-file
 "fZ" #'zoxide-cd)

;;; help bindings
(jds/leader-def
 "h" '(:ignore t :which-key "help")
 "hk" #'helpful-key
 "hf" #'helpful-callable
 "hv" #'helpful-variable
 "hF" #'describe-face
 "ha" #'consult-apropos
 "ht" #'consult-theme)


;;; window management
(jds/leader-def
 "w" '(:ignore t :which-key "window")
 "wt" #'tear-off-window
 ;; "wT" #'jds/window-go-home
 "wu" #'winner-undo
 "wR" #'winner-redo
 "wd" #'evil-window-delete
 "w=" #'balance-windows
 "wv" #'evil-window-vsplit
 "wV" #'+evil/window-vsplit-and-follow
 "ws" #'evil-window-split
 "wS" #'+evil/window-split-and-follow
 "wh" #'evil-window-left
 "wj" #'evil-window-down
 "wk" #'evil-window-up
 "wl" #'evil-window-right
 "wh" #'evil-window-left
 "wj" #'evil-window-down
 "wk" #'evil-window-up
 "wl" #'evil-window-right
 "wH" #'+evil/window-move-left
 "wJ" #'+evil/window-move-down
 "wK" #'+evil/window-move-up
 "wL" #'+evil/window-move-right)

;;; buffers

(jds/leader-def
 "b" '(:ignore t :which-key "buffer")
 "bb" #'consult-buffer
 "bB" #'consult-buffer-other-frame
 "bd" #'kill-current-buffer
 "bm" #'bookmark-set
 "bM" #'bookmark-delete
 "br" #'revert-buffer
 "bx" #'(lambda () (interactive) (switch-to-buffer "*scratch*")))

;;; org and apps

(general-define-key
 :states '(n v)
 "q" '(:ignore t :which-key "open/org")
 "qc" #'org-capture
 "qO" #'org-capture-goto-target
 "ql" #'org-store-link
 "qt" #'vterm
 "qa" #'jds/org-agenda-show-custom-day
 "qA" #'org-agenda
 "qs" #'consult-org-agenda
 "qS" #'org-search-view
 "qd" #'dired-jump
 "qD" #'jds/deer-downloads
 "qh" #'(lambda () (interactive) (deer "/home/jds6696/")))

;; move macros
(general-define-key
 :states '(n v)
 "Q" #'evil-record-macro)

;;; search
(jds/leader-def
 "/" #'consult-ripgrep
 "s" '(:ignore t :which-key "search")
 "ss" #'consult-line
 "sS" #'(lambda () (interactive) (consult-line-multi 'all-buffers))
 "sm" #'consult-mark
 "si" #'consult-outline
 "sI" #'consult-imenu-multi
 "sy" #'consult-yank-from-kill-ring
 "sb" #'consult-bookmark)


;;; git

(jds/leader-def
 "g" '(:ignore t :which-key "search")
 "gg" #'magit-status)

;;; evil bindings

(general-define-key
 :states '(normal visual)
 "gr" #'quickrun-region
 "gR" #'quickrun-shell
 "gc" #'evilnc-comment-operator
 "g;" #'goto-last-change
 "gi" #'evil-insert-resume
 "gv" #'evil-visual-restore
 "gu"  #'evil-downcase
 "gU"  #'evil-upcate
 ;; avy and hinting
 "gl" #'link-hint-open-link
 "gL" #'link-hint-copy-link)


(general-itomap
 :state 'o
 "g"  #'+evil:whole-buffer-txtobj
 "f"  #'+evil:defun-textobj
 "c"  #'evilnc-inner-comment
 "a"  #'evil-inner-arg
 ;; "d"  #'evil-inner-delim
 )


(general-otomap
 :state 'o
 "g"  #'+evil:whole-buffer-txtobj
 "f"  #'+evil:defun-textobj
 "c"  #'evilnc-outer-comment
 "a"  #'evil-outer-arg
 ;; "d"  #'evil-outer-delim
 )





(provide 'bindings)
;;; bindings.el ends here
