;;; bindings.el -- only for general purpose high-level bindings -*- lexical-binding: t; -*-


;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;;; universal argument and other short stuff
(jds/leader-def 
  "/" #'consult-ripgrep
  "?" #'affe-grep
  ";" #'pp-eval-expression
  "." #'consult-projectile
  "\`" #'evil-switch-to-windows-last-buffer
  "-"  #'vertico-repeat-last
  "SPC" #'consult-buffer
  "C-SPC" #'consult-buffer)


(general-define-key
 :keymaps 'override
 "C-'" #'popper-toggle-latest
 "C-M-'" #'popper-cycle
 "C-M-\"" #'popper-toggle-type
 "C-;" #'embark-act
 "C-:" #'embark-dwim
 "C-<" #'lispyville-barf
 "C->" #'lispyville-slurp)

;;;###autoload
(defun jds~kill-whole-line ()
  "Kill line, with prefix-arg kill entire line forwards and backwards to start of line."
  (interactive)
  ;; (beginning-of-line)
  ;; (kill-line)
  (evil-delete-back-to-indentation))

(general-imap
  "C-k" #'jds~kill-whole-line
  "C-a" #'move-beginning-of-line
  "C-e" #'move-end-of-line
  "M-w" #'kill-word)
;;  Note Defaults
;; C-w kill word back
;; "C-M-e" move to end of defun
;; "C-M-a" move to start of defun
(general-nmap
  "C-e" #'move-end-of-line)

;; completion
;;;###autoload
(defun jds/yas-or-capf-or-indent ()
  (interactive)
  (if (yas-expand)
      nil
    (if (completion-at-point)
	nil
      (indent-for-tab-command))))


;; ultimately I found this annoying and it screwed with balanced parentheses in lisp code
;; (global-set-key [remap dabbrev-expand] 'hippie-expand)

;;;###autoload
(defun jds~blank-line-p ()
  "Is point currently on blank line."
  (looking-at-p "^[[:space:]]*$"))

;;;###autoload
(defun jds/tab-dwim ()
  (interactive)
  ;; don't indent in texmathp
  (cond
   ((and (texmathp) (or (bound-and-true-p cdlatex-mode) ord-cdlatex-mode))
    (cdlatex-tab))
   ((and (string= (string (char-before)) " ")
	 (string= major-mode "org-mode"))
    (org-cycle))
   (t (jds/yas-or-capf-or-indent))))


(defun jds/completion-keys ()
  (evil-local-set-key 'insert (kbd "<tab>") #'jds/tab-dwim)
  ;; (evil-local-set-key 'insert (kbd "C-l")   #'company-ispell)
  )
(add-hook 'text-mode-hook 'jds/completion-keys)
(add-hook 'prog-mode-hook 'jds/completion-keys)


;;; filesystem bindings
(jds/leader-def
  "f" '(:ignore t :which-key "files")
  "ff" #'find-file
  "fr" #'consult-recent-file
  "fs" #'save-buffer
  "fa" #'jds/affe-find-files-home
  "fS" #'evil-write-all
  "fz" #'zoxide-find-file
  "fZ" #'zoxide-cd
  "fc" #'jds/open-config
  "fC" #'jds/find-file-config
  "fk" #'projectile-kill-buffers
  "fp" #'consult-projectile
  "fP" #'jds/find-file-other-project
  "fo" #'projectile-find-other-file
  "fi" #'projectile-find-tag)


;;; help bindings
(jds/leader-def
  "h" '(:ignore t :which-key "help")
  "hk" #'helpful-key
  "hK" #'describe-keymap
  "hf" #'helpful-callable
  "hv" #'helpful-variable
  "hF" #'describe-face
  "ha" #'consult-apropos
  "ht" #'consult-theme
  "hl" #'view-lossage
  "hi" #'info-display-manual
  "hm" #'describe-mode)


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

;;; toggle
(jds/leader-def
  "t" '(:ignore t :wk "toggles")
  "tm" #'hide-mode-line-mode)

;;; other
(jds/leader-def
  "S" #'jds/screenshot-dragon-temp-file
  "u" #'undo-tree-visualize)

;;; buffers

;;;###autoload
(defun jds/switch-to-splash ()
  "Switch to splash buffer if it exists, otherwise call kisses-redraw."
  (interactive)
  (let ((buf (get-buffer "*splash*")))
    (if buf
	(switch-to-buffer buf)
      (kisses-redraw))))

;;;###autoload
(defun jds/switch-to-agenda-file (fn)
    "Display file listed org-directory."
  (interactive)
  (find-file
   (expand-file-name fn org-directory)))

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
  "bx" '((lambda () (interactive) (switch-to-buffer (get-buffer-create "*scratch*"))) :which-key "scratch")
  "bz" '((lambda () (interactive) (switch-to-buffer (messages-buffer))) :which-key "messages")
  "bs" #'jds/switch-to-splash
  "bt" '((lambda () (interactive) (jds/switch-to-agenda-file "tasks.org")) :which-key "tasks")
  "bp" '((lambda () (interactive) (jds/switch-to-agenda-file "meetings_psu.org")) :which-key "meetings-psu")
  "bp" '((lambda () (interactive) (jds/switch-to-agenda-file "notes.org")) :which-key "notes"))


(general-define-key
 :keymaps 'ibuffer-mode-map
 :states '(mnv)
 "f" #'avy-goto-line)

;;; text editing
(jds/leader-def
  "a"      '(:ignore t :wk "editing")
  "at"     #'synosaurus-choose-and-replace
  "aT"     #'dictionary-search
  "aq"     '(:ignore t :wk "fill/unfill")
  "aqp"    #'fill-paragraph
  "aqr"    #'fill-region
  "aqP"    #'unfill-paragraph
  "aqR"    #'unfill-region
  "af SPC" #'unfill-toggle
  "ac" #'completion-at-point
  ;; "at" #'complete-tag
  "ad" #'cape-dabbrev
  "af" #'cape-file
  "ah" #'cape-history
  "ak" #'cape-keyword
  "as" #'cape-symbol
  "ab" #'cape-abbrev
  "al" #'cape-line
  "aw" #'cape-dict
  "a\\" #'cape-tex)

;;; repls and such
(jds/leader-def
  "r"    '(:ignore t :wk "REPL")
  "rr"   #'R ;; or run-ess-r 
  "rp"   #'run-python
  "rl"   #'run-lisp)

(jds/leader-def
  "j" '(:ignore t :which-key "jump")
  "j f"   #'dired-hist-go-forward
  "j b"   #'dired-hist-go-back
  "j RET" #'dired-registers-goto-completing-read
  "j m"   #'dired-registers-store
  "j j"   #'dired-registers-goto
  "j d"   '((lambda () (interactive) (dired-registers-goto ?d)) :wk "downloads")
  "j o"   '((lambda () (interactive) (dired-registers-goto ?o)) :wk "org")
  "j h"   '((lambda () (interactive) (dired-registers-goto ?h)) :wk "home")
  "j c"   '((lambda () (interactive) (dired-registers-goto ?c)) :wk "config"))

;;; org and apps

;;;###autoload
(defun jds/help-thing-at-point ()
    "Look up documentation for thing at point."
    (interactive)
    (cond
     ((or (string= major-mode "ess-r-mode")
	  (string= major-mode "inferior-ess-r-mode"))
      (call-interactively #'ess-display-help-on-object))
     (t (helpful-at-point))))

;; macro to start slack if not already
(defmacro jds~start-slack-function (fun)
      "If slack is not already started, start it then run fun."
          `(lambda () (interactive)
	            (if (fboundp 'slack-select-rooms)
		                 (,fun)
				          (slack-start)
					           (,fun))))

(jds/sub-leader-def
  "," #'org-capture   ;; q "new"
  "C-," #'org-capture ;; q "new"
  ;; "<" #'org-capture-goto-target ;; new and follow
  ">" #'org-refile-goto-last-stored
  "<" #'org-capture-goto-last-stored
  "a" #'jds/affe-find-files-home
  "z" #'zoxide-find-file
  "Z" #'zoxide-cd
  "c" #'jds/mu4e-compose-goto-to
  ;; "M" #'jds/open-mu4e-new-frame
  ;; "m" #'mu4e
  "g" #'magit-status
  "m" #'(lambda () (interactive)
	  (mu4e-headers-search-bookmark
	   (mu4e-get-bookmark-query ?t)))
  "M" #'(lambda (&optional arg) (interactive "P")
	  (jds~new-frame-or-new-window arg)
	  (mu4e-headers-search-bookmark (mu4e-get-bookmark-query ?t)))
  "l" #'org-store-link
  "i" #'org-insert-link
  ;; "t" #'vterm
  "t" #'jds/multi-vterm-same-window
  "T" #'jds/multi-vterm-new-window-or-frame
  "f" #'jds/org-agenda-show-custom-day
  "F" #'jds/open-custom-day-agenda-new-frame
  "p" #'org-agenda
  "s" #'consult-org-agenda
  "S" #'org-search-view
  "y" (jds~start-slack-function slack-select-rooms)
  "Y" (jds~start-slack-function slack-select-unread-rooms)
  "k" #'jds/help-thing-at-point
  "d" #'jds/dired-jump-and-kill-buffer
  "D" #'dired-jump-other-window
  ;; "d" #'jds/dired-jump-reuse-dired
  ;; "d" #'dired-sidebar-toggle-sidebar
  ;; "D" #'jds/dired-jump-and-kill-buffer
  )

;;; faster editing in text buffers


(general-define-key
 :states 'n
 :keymaps 'override
 "K" #'delete-indentation)


;;;###autoload
(defun jds/consult-ripgrep-config ()
  (interactive)
  (consult-ripgrep "~/.emacs.d"))

;;; search
(jds/leader-def
  "s" '(:ignore t :which-key "search")
  "ss" #'consult-line
  "sS" #'(lambda () (interactive) (consult-line-multi 'all-buffers))
  "sc" #'jds/consult-ripgrep-config
  "sm" #'consult-mark
  ;; "si" #'consult-outline
  ;; "sI" #'consult-imenu-multi
  "sy" #'consult-yank-from-kill-ring)

;;; git

(jds/leader-def
  "g" '(:ignore t :which-key "git")
  "gg" #'magit-status
  "gt" #'git-timemachine
  "gb" #'magit-blame)

;;; apps

(jds/leader-def
  "o" '(:ignore t :which-key "other/apps")
  "os" #'jds/hydra-spotify-wrapper)

;;; evil bindings

;; declare some motion commands to make jumping in visual mode go
;; to the correct character
(with-eval-after-load 'avy 
  (evil-declare-motion 'jds/avy-goto-delim-end)
  (evil-declare-motion 'jds/avy-goto-delim-start)
  (evil-declare-motion 'jds/avy-goto-punctuation)
  (evil-declare-motion 'jds/avy-goto-quote)
  (evil-declare-motion 'jds/avy-goto-word-0-end))



;;;###autoload
(defun jds/smart-consult-outline-imenu ()
    "Use consult-outline in org-buffers but consult-imenu otherwise."
  (interactive)
  (if (string= major-mode "org-mode")
      (consult-outline)
    (consult-imenu)))

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
 "gm" #'evil-next-match	;; select prior search
 "gn" #'jds/evil-search-convert-avy-jump
 ;; "gi" #'evil-insert-resume
 "gv" #'evil-visual-restore
 "gu"  #'evil-downcase
 "gU"  #'evil-upcase
 "gt" #'jds/title-case-region-or-line
 "go" #'jds/evil-insert-line-below
 "gO" #'jds/evil-insert-line-above
 ;; avy and hinting
 "gl" (lambda (&optional arg) (interactive "P") (if arg (link-hint-copy-link) (link-hint-open-link)))
 ;; "gL" #'link-hint-copy-link
 "gL" #'link-hint-aw-select
 "gw" #'avy-goto-word-0
 "ge" #'jds/avy-goto-word-0-end
 "gd" #'jds/avy-goto-delim-start
 "gD" #'jds/avy-goto-delim-end
 "gs" #'consult-line
 "gS" #'(lambda () (interactive) (consult-line-multi 'all-buffers))
 "g'" #'jds/avy-goto-quote
 "g." #'jds/avy-goto-punctuation
 "gj" #'evilem-motion-next-visual-line
 "gk" #'evilem-motion-previous-visual-line
 "gy" #'consult-yank-from-kill-ring
 ;; "gY" #'XXX
 ;; "gq" #'XXX
 ;; "gQ" #'XXX
 "ga" #'ace-window
 "gA" #'jds/ace-window-save-excursion
 "gz" #'jds/avy-fix-spelling
 ;; "gZ" #'XXX
 ;; "gt" #'XXX
 ;; "gT" #'XXX
 "gf" #'avy-goto-char
 "gi" #'jds/smart-consult-outline-imenu
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
 :states '(insert)
 "<M-tab>" #'flyspell-auto-correct-previous-word)

(general-define-key
 :states '(normal visual)
 ;; note a bunch taken by evil fold
 "zf"  #'find-file-at-point
 "zF"  #'find-file-other-frame
 "zd"  #'xref-find-definitions
 "zD"  #'xref-find-definitions-other-frame
 "zs"  #'xref-find-references
 "zl"  #'evil-lion-left
 "zL"  #'evil-lion-right)

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



;;; key-chord for evil
(use-package key-chord
  :config 
  (key-chord-mode 1)
  (setq key-chord-one-keys-delay 0.010
	key-chord-two-keys-delay 0.06)
  (key-chord-define-global "jk" 'evil-force-normal-state)
  (key-chord-define-global "wq" 'save-buffer)
  (key-chord-define-global "WQ" 'evil-write-all))

(defun jds~org-agenda-local-bindings ()
  (key-chord-define-local "wq" #'org-save-all-org-buffers))
(add-hook 'org-agenda-mode-hook 'jds~org-agenda-local-bindings)

(general-vmap
  :keymap 'global
  "m" #'er/expand-region
  "M" #'er/contract-region)
