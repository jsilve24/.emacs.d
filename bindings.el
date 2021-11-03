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
 "/" #'consult-ripgrep
 "?" #'affe-grep
 ";" #'pp-eval-expression
 "RET" #'projectile-find-file
 "\`" #'evil-switch-to-windows-last-buffer
 "SPC" #'execute-extended-command)

;;; filesystem bindings
(jds/leader-def
 "f" '(:ignore t :which-key "files")
 "ff" #'find-file
 "fr" #'consult-recent-file
 "fs" #'save-buffer
 "fa" #'(lambda () (interactive) (affe-find "/home/jds6696/"))
 "fS" #'evil-write-all
 "fz" #'zoxide-find-file
 "fZ" #'zoxide-cd
 "fc" #'jds/open-config
 "fC" #'jds/find-file-config
 "fp" #'projectile-switch-project
 "fP" #'jds/find-file-other-project
 "fo" #'projectile-find-other-file)

;;; project bindings


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
 "w" '(:ignore t :which-key "window/frame")
 "wt" #'tear-off-window
 "wT" #'transpose-frame
 "wf" #'make-frame
 ;; "wT" #'jds/window-go-home
 "wu" #'winner-undo
 "wR" #'winner-redo
 "wd" #'evil-window-delete
 "wD" #'kill-buffer-and-window
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
 "wL" #'+evil/window-move-right
 "wm" #'delete-other-windows
 "wb" #'switch-to-minibuffer)

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

;;; text editing
(jds/leader-def
  "a"    '(:ignore t :wk "editing")
  "af"   '(:ignore t :wk "fill/unfill")
  "afp" #'fill-paragraph
  "afr" #'fill-region
  "afP" #'unfill-paragraph
  "afR" #'unfill-region
  "af SPC" #'unfill-toggle)

;;; org and apps

(jds/sub-leader-def
 "," #'org-capture           ;; q "new"
 "<" #'org-capture-goto-target ;; new and follow
 ">" #'org-capture-goto-last-stored
 "c" #'jds/mu4e-compose-goto-to
 "m" #'jds/open-mu4e-new-frame
 "M" #'mu4e
 "l" #'org-store-link
 "i" #'org-insert-link
 "t" #'vterm
 "a" #'jds/org-agenda-show-custom-day
 "A" #'org-agenda
 "s" #'consult-org-agenda
 "S" #'org-search-view
 "k" #'helpful-at-point
 "d" #'dired-jump
 "D" #'jds/deer-downloads
 "h" #'(lambda () (interactive) (deer "/home/jds6696/")))

;;; faster editing in text buffers

;; (defun jds/text-mode-local-keys ()
;;   (interactive)
;;   (evil-local-set-key 'normal "H" #'evil-backward-sentence-begin)
;;   (evil-local-set-key 'normal "J" #'jds/paragraph-forward)
;;   (evil-local-set-key 'normal "K" #'jds/paragraph-backward)
;;   (evil-local-set-key 'normal "L" #'evil-forward-sentence-begin))
;; (add-hook 'text-mode-hook 'jds/text-mode-local-keys)

(general-define-key
 :states 'n
 :keymaps 'override
 "K" #'delete-indentation)

;;; search
(jds/leader-def
 "s" '(:ignore t :which-key "search")
 "ss" #'consult-line
 "sS" #'(lambda () (interactive) (consult-line-multi 'all-buffers))
 "sm" #'consult-mark
 ;; "si" #'consult-outline
 ;; "sI" #'consult-imenu-multi
 "sy" #'consult-yank-from-kill-ring
 "sb" #'consult-bookmark)


;;; git

(jds/leader-def
 "g" '(:ignore t :which-key "search")
 "gg" #'magit-status)

;;; evil bindings

;; declare some motion commands to make jumping in visual mode go
;; to the correct character
(with-eval-after-load 'avy 
  (evil-declare-motion 'jds/avy-goto-delim-end)
  (evil-declare-motion 'jds/avy-goto-delim-start)
  (evil-declare-motion 'jds/avy-goto-punctuation)
  (evil-declare-motion 'jds/avy-goto-quote)
  (evil-declare-motion 'jds/avy-goto-word-0-end))


;; don't touch
;; g-;  g-i   g-n
(general-define-key
 :states '(normal visual motion)
 "gr" #'eval-region
 "gR" #'quickrun-shell
 "gc" #'evilnc-comment-operator
 "g;" #'goto-last-change
 "g/" #'avy-goto-char-timer
 "g," #'jds/evil-snipe-convert-avy-jump
 "gm" #'evil-next-match                ;; select prior search
 "gn" #'jds/evil-search-convert-avy-jump
 "gi" #'evil-insert-resume
 "gv" #'evil-visual-restore
 "gu"  #'evil-downcase
 "gU"  #'evil-upcate
 "go" #'jds/evil-insert-line-below
 "gO" #'jds/evil-insert-line-above
 ;; avy and hinting
 "gl" #'link-hint-open-link
 "gL" #'link-hint-copy-link
 "gw" #'avy-goto-word-0
 "ge" #'jds/avy-goto-word-0-end
 "gd" #'jds/avy-goto-delim-start
 "gD" #'jds/avy-goto-delim-end
 "gs" #'consult-line
 "g'" #'jds/avy-goto-quote
 "g." #'jds/avy-goto-punctuation
 "gj" #'evilem-motion-next-visual-line
 "gk" #'evilem-motion-previous-visual-line
 "gy" #'consult-yank-from-kill-ring
 ;; "gY" #'XXX
 ;; "gq" #'XXX
 ;; "gQ" #'XXX
 "ga" #'ace-window
 ;; "gA" #'XXX
 "gz" #'jds/avy-fix-spelling
 ;; "gZ" #'XXX
 ;; "gt" #'XXX
 ;; "gT" #'XXX
 "gf" #'avy-goto-char
 "gi" #'consult-outline
 "gI" #'consult-imenu-multi)

;; make gh mode specific -- for headings or sections (can popup-imenu or something)
(general-define-key
 :states '(n v)
 :keymaps 'org-mode-map
 "gh" #'jds/org-goto-visible)

(general-define-key
 :states '(normal visual)
 "z=" #'flyspell-correct-wrapper
 "zg" #'jds/save-word
 ;; "[s" #'jds/spell-fix-previous-error
 ;; "]s" #'jds/spell-fix-next-error
 "[s" #'flyspell-correct-previous
 "]s" #'flyspell-correct-next
 "[S" #'evil-prev-flyspell-error
 "]S" #'evil-next-flyspell-error)
(general-define-key
 :states '(insert normal visual)
 "M-z" #'flyspell-auto-correct-previous-word)

(general-define-key
 :states '(normal visual)
 ;; note a bunch taken by evil fold
 "zf" #'find-file-at-point
 "zF" #'find-file-other-frame
 "zd" #'xref-find-definitions
 "zD" #'xref-find-definitions-other-frame
 ;;"zl" taken by evil lion
 ;;"zL" "taken by evil lion"
 )


;;; text objects


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
