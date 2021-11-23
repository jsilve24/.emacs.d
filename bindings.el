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
 "RET" #'consult-projectile
 "\`" #'evil-switch-to-windows-last-buffer
 "SPC" #'consult-buffer
 "C-SPC" #'consult-buffer)

(general-define-key
 :keymaps 'override
 "M-'" #'popper-toggle-latest
 "C-M-'" #'popper-cycle
 "C-M-\"" #'popper-toggle-type)

;;; completion
;;;###autoload
(defun jds/yas-or-company ()
  (interactive)
  (if (yas-expand)
      nil
    (if (company-complete-common)
	nil
      (call-interactively #'company-dabbrev))))
;;;###autoload
(defun jds/tab-dwim ()
  (interactive)
  (cond
   ((and (texmathp) (bound-and-true-p cdlatex-mode))  (cdlatex-tab))
   ((message--in-tocc-p) (completion-at-point))
   (t (jds/yas-or-company))))
(defun jds/completion-keys ()
  (evil-local-set-key 'insert (kbd "<tab>") #'jds/tab-dwim)
  (evil-local-set-key 'insert (kbd "C-l")   #'company-ispell))
(add-hook 'text-mode-hook 'jds/completion-keys)
(add-hook 'prog-mode-hook 'jds/completion-keys)

;; (general-define-key
;;  :keymaps 'overide
;;  :states '(n v i m o e)
;;  "C-u" #'universal-argument)

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
 "fp" #'consult-projectile
 "fP" #'jds/find-file-other-project
 "fo" #'projectile-find-other-file)


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
 "wq" #'kill-buffer-and-window
 "wk" #'evil-window-up
 "wl" #'evil-window-right
 "wH" #'+evil/window-move-left
 "wJ" #'+evil/window-move-down
 "wK" #'+evil/window-move-up
 "wL" #'+evil/window-move-right
 "wb" #'switch-to-minibuffer
 "wm" '(:ignore t :which-key "maximize")
 "wmm" #'delete-other-windows
 "wmv" #'delete-other-windows-vertically)

(if (fboundp 'eh-current-window-to-frame) ;; proxy for "if currently using exwm"
    (jds/leader-def
      "wt" #'eh-current-window-to-frame)
    (jds/leader-def
      "wt" #'tear-off-window))


;;; toggle
(jds/leader-def
  "t" '(:ignore t :wk "toggles")
  "tm" #'hide-mode-line-mode)

;;; buffers

(jds/leader-def
 "b"  '(:ignore t :which-key "buffer/bookmark")
 "bb" #'consult-buffer
 "bB" #'consult-buffer-other-frame
 "bo" #'consult-bookmark
 "bO" #'burly-open-last-bookmark
 "bd" #'kill-current-buffer
 "bw" #'burly-bookmark-windows
 "bf" #'burly-bookmark-frames
 "bm" #'bookmark-set
 "bM" #'bookmark-delete
 "br" #'revert-buffer
 "bi" #'ibuffer-jump
 "bI" #'ibuffer-other-window
 "bx" #'(lambda () (interactive) (switch-to-buffer (get-buffer-create "*scratch*"))))

(general-define-key
 :keymaps 'ibuffer-mode-map
 :states '(mnv)
 "f" #'avy-goto-line)

;;; text editing
(jds/leader-def
  "a"      '(:ignore t :wk "editing")
  "as"     #'synosaurus-choose-and-replace
  "af"     '(:ignore t :wk "fill/unfill")
  "afp"    #'fill-paragraph
  "afr"    #'fill-region
  "afP"    #'unfill-paragraph
  "afR"    #'unfill-region
  "af SPC" #'unfill-toggle
  "al"     #'evil-lion-left
  "aL"     #'evil-lion-right)


;;; repls and such
(jds/leader-def
  "r"    '(:ignore t :wk "REPL")
  "rr"   #'R ;; or run-ess-r 
  "rp"   #'run-python
  "rl"   #'run-lisp)

;;; org and apps

(jds/sub-leader-def
 "," #'org-capture           ;; q "new"
 "<" #'org-capture-goto-target ;; new and follow
 ">" #'org-capture-goto-last-stored
 "a" #'(lambda () (interactive) (affe-find "/home/jds6696/"))
 "z" #'zoxide-find-file
 "Z" #'zoxide-cd
 "c" #'jds/mu4e-compose-goto-to
 ;; "M" #'jds/open-mu4e-new-frame
 ;; "m" #'mu4e
 "m" #'(lambda () (interactive)
	 (mu4e-headers-search-bookmark
	  (mu4e-get-bookmark-query ?t)))
 "M" #'(lambda () (interactive)
	 ;; (if (frame-parameter (selected-frame) 'exwm-active) ... ...)
	 (select-frame (make-frame))
	 (mu4e-headers-search-bookmark (mu4e-get-bookmark-query ?t)))
 "l" #'org-store-link
 "i" #'org-insert-link
 ;; "t" #'vterm
 "t" #'multi-vterm-next
 "f" #'jds/org-agenda-show-custom-day
 "F" #'jds/open-custom-day-agenda-new-frame
 "p" #'org-agenda
 "s" #'consult-org-agenda
 "S" #'org-search-view
 "k" #'helpful-at-point
 "d" #'dired-jump
 "D" #'jds/deer-downloads
 "h" #'(lambda () (interactive) (deer "/home/jds6696/")))

;;; faster editing in text buffers


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
 "sy" #'consult-yank-from-kill-ring)


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
 :keymaps 'override
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
 "gU"  #'evil-upcase
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
 "<M-tab>" #'flyspell-correct-wrapper
 "zg" #'jds/save-word
 "zG" #'jds/flyspell-correct-word-then-abbrev
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
 "zD" #'xref-find-definitions-other-frame)

(general-define-key
 :keymaps 'org-mode-map
 :states '(n v)
 "zi" #'org-toggle-inline-images
 "zI" #'org-toggle-latex-fragment)

(jds/localleader-def
  :keymaps '(org-mode-map org-capture-mode-map)
  "*" #'org-toggle-heading
  "-" #'org-toggle-item)

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
