;;; bindings.el -- only for general purpose high-level bindings -*- lexical-binding: t; -*-


;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;;; universal argument and other short stuff
(jds/leader-def 
  "/" #'consult-ripgrep
  "?" #'affe-grep
  ";" #'pp-eval-expression
  "RET" #'consult-projectile
  "\`" #'evil-switch-to-windows-last-buffer
  "-"  #'vertico-repeat
  "SPC" #'consult-buffer
  "C-SPC" #'consult-buffer)

(general-define-key
 :keymaps 'override
 "C-'" #'popper-toggle-latest
 "C-M-'" #'popper-cycle
 "C-M-\"" #'popper-toggle-type
 "C-;" #'embark-act
 "C-:" #'embark-dwim)

;;;###autoload
(defun jds~kill-whole-line ()
  "Kill line, with prefix-arg kill entire line forwards and backwards to start of line."
  (interactive)
  (beginning-of-line)
  (kill-line))

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
(defun jds~blank-line-p ()
    "Is point currently on blank line."
  (looking-at-p "^[[:space:]]*$"))

;;;###autoload
(defun jds/tab-dwim ()
  (interactive)
  (cond
   ((jds~blank-line-p) (progn (indent-relative)
			      (if (string= major-mode "org=mode")
				  (org-beginning-of-line))))
   ((and (texmathp) (or (bound-and-true-p cdlatex-mode)
			org-cdlatex-mode))
    (cdlatex-tab))
   ((and (string= (string (char-before)) " ")
	 (string= major-mode "org-mode"))
    (org-cycle))
   ((message--in-tocc-p)
    (completion-at-point))
   (t
    (jds/yas-or-company))))

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
  "as"     #'synosaurus-choose-and-replace
  "af"     '(:ignore t :wk "fill/unfill")
  "afp"    #'fill-paragraph
  "afr"    #'fill-region
  "afP"    #'unfill-paragraph
  "afR"    #'unfill-region
  "af SPC" #'unfill-toggle)


;;; repls and such
(jds/leader-def
  "r"    '(:ignore t :wk "REPL")
  "rr"   #'R ;; or run-ess-r 
  "rp"   #'run-python
  "rl"   #'run-lisp)

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


(jds/sub-leader-def
  "," #'org-capture		;; q "new"
  "C-," #'org-capture		;; q "new"
  ;; "<" #'org-capture-goto-target ;; new and follow
  ">" #'org-refile-goto-last-stored
  "<" #'org-capture-goto-last-stored
  "a" #'jds/affe-find-files-home
  "z" #'zoxide-find-file
  "Z" #'zoxide-cd
  "c" #'jds/mu4e-compose-goto-to
  ;; "M" #'jds/open-mu4e-new-frame
  ;; "m" #'mu4e
  "m" #'(lambda () (interactive)
	  (mu4e-headers-search-bookmark
	   (mu4e-get-bookmark-query ?t)))
  "M" #'(lambda () (interactive)
	  (jds~new-frame-or-new-window)
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
  "y" #'slack-select-rooms
  "Y" #'slack-select-unread-rooms
  "k" #'jds/help-thing-at-point
  ;; "d" #'dired-jump
  "d" #'jds/dired-jump-reuse-dired
  ;; "d" #'dired-sidebar-toggle-sidebar
  "D" #'jds/dired-jump-and-kill-buffer
  "j" #'(:ignore t :wk "dired jump to")
  "jd" #'(lambda () (interactive) (dired "~/Downloads/"))
  "jh" #'(lambda () (interactive) (dired "~"))
  "jo" '((lambda () (interactive) (dired "~/Dropbox/org")) :wk "org")
  "jc" '((lambda () (interactive) (dired "~/.emacs.d")) :wk "config")
  "jg" '((lambda () (interactive) (dired "~/Dropbox/Faculty/Grants")) :wk "grants")
  "ja" '((lambda () (interactive) (dired "~/Dropbox/Faculty/advising")) :wk "advising")
  "jp" '((lambda () (interactive) (dired "~/Dropbox/Faculty/Presentations")) :wk "presentations")
  "jt" '((lambda () (interactive) (dired "~/Dropbox/Faculty/Teaching")) :wk "teaching")
  "ji" '((lambda () (interactive) (dired "~/Dropbox/Faculty/Teaching")) :wk "teaching")
  )

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
 "sc" '((lambda () (interactive) (consult-ripgrep "~/.emacs.d")) :wk "ripgrep-config")
 "sm" #'consult-mark
 ;; "si" #'consult-outline
 ;; "sI" #'consult-imenu-multi
 "sy" #'consult-yank-from-kill-ring)


;;; git

(jds/leader-def
  "g" '(:ignore t :which-key "git")
  "gg" #'magit-status
  "gt" #'git-timemachine)

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
 "gt" #'jds/title-case-region-or-line
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
 ;; "gA" #'XXX
 "gz" #'jds/avy-fix-spelling
 ;; "gZ" #'XXX
 ;; "gt" #'XXX
 ;; "gT" #'XXX
 "gf" #'avy-goto-char
 "gi" #'consult-outline
 "gI" #'consult-imenu-multi)

(general-define-key
 :states '(n v)
 "U" #'evil-invert-char)

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
	key-chord-two-keys-delay 0.04)
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
