;;; bindings.el -- only for general purpose high-level bindings -*- lexical-binding: t; -*-



;;; universal argument and other short stuff
(jds/leader-def 
  "/" #'consult-ripgrep
  "?" #'affe-grep
  ";" #'pp-eval-expression
  ;; "." #'consult-project-extra-find
  "." #'consult-projectile
  ">" #'consult-projectile-switch-project
  "\`" #'evil-switch-to-windows-last-buffer
  "-"  #'vertico-repeat
  "SPC" #'consult-buffer
  "C-SPC" #'consult-buffer)


(general-define-key
 :keymaps 'override
 "C-'" #'popper-toggle
 "C-\"" #'popper-cycle
 "C-M-'" #'popper-toggle-type
 "C-;" #'embark-act
 "C-:" #'embark-dwim
 "C-<" #'lispyville-barf
 "C->" #'lispyville-slurp
 "C-<tab>" #'completion-at-point)

(general-define-key
 :keymaps '(insert normal override)
 "C-<backspace>" #'jds/hungry-delete-or-kill-sexp
 "C-n" #'grugru
 "C-t" #'evil-numbers/inc-at-pt-incremental
 "C-S-t" #'evil-numbers/dec-at-pt-incremental)

(general-define-key
 :keymaps 'normal
 "C-y" #'evil-scroll-line-down
 "C-S-y" #'evil-scroll-line-up
 "C-d" #'evil-scroll-down
 "C-S-d" #'evil-scroll-up)

;; better scroll other window  bindings
(general-define-key
 :keymaps '(normal insert emacs visual motion)
 "C-S-n" (general-simulate-key "C-M-v")
 "C-S-p" (general-simulate-key "M-<prior>"))


;;;###autoload
(defun jds~kill-whole-line ()
  "Kill line, with prefix-arg kill entire line forwards and backwards to start of line."
  (interactive)
  (evil-delete-back-to-indentation))

;;;###autoload
(defun jds/evil-paste-from-clipboard ()
  (interactive)
  (evil-paste-from-register ?\"))

(general-imap
  "C-k" #'jds~kill-whole-line
  "C-a" #'move-beginning-of-line
  "C-e" #'move-end-of-line
  "C-p" #'jds/evil-paste-from-clipboard
  "M-w" #'kill-word)
;;  Note Defaults
;; C-w kill word back
;; "C-M-e" move to end of defun
;; "C-M-a" move to start of defun

(general-nmap
  "C-e" #'move-end-of-line)


;; ultimately I found this annoying and it screwed with balanced parentheses in lisp code
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;;;###autoload
(defun jds~blank-line-p ()
  "Is point currently on blank line."
  (looking-at-p "^[[:space:]]*$"))

;;;###autoload
(defun jds/jump-delim ()
    "Jump past delimiter"
  (interactive)
(if (looking-at (rx (or
		     (literal ")")
		     (literal "]")
		     (literal "}"))))
    (progn (forward-char) t)
  nil))
					
;;;###autoload
(defun jds/tab-dwim ()
  (interactive)
  ;; don't indent in texmathp
  (cond
   ((and (texmathp) (or (bound-and-true-p cdlatex-mode) org-cdlatex-mode))
    (cdlatex-tab))
   ((org-table-p) (org-table-next-field))
   ((and
     (or 
      (org-at-drawer-p)
      (org-at-heading-p))     
     (or (string= major-mode "org-mode")
	 (string= major-mode "org-msg-edit-mode")))
    (org-cycle))
   ((yas-expand) nil)
   ((yas-active-snippets) (yas-next-field))
   ((and (looking-back "[a-zA-Z0-9][a-zA-Z0-9]" 2)
	 (completion-at-point))  nil)
   ((jds/jump-delim) nil)
   ((and (string= major-mode "ess-r-mode") (looking-back "[^\s]"))
    (ess-indent-command))
   ((indent-for-tab-command nil))))


(defun jds/completion-keys ()
  (evil-local-set-key 'insert (kbd "<tab>") #'jds/tab-dwim)
  (evil-local-set-key 'normal (kbd "<tab>") #'jds/tab-dwim)
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
  ;; "fa" #'jds/affe-find-files-home
  "fS" #'evil-write-all
  "fz" #'zoxide-find-file
  "fc" #'jds/open-config
  "fC" #'jds/find-file-config
  ;; "fk" #'project-kill-buffers
  "fm" #'projectile-compile-project ;; mneumonic: "file make"
  "fk" #'projectile-kill-buffers
  "fp" #'consult-projectile
  "fP" #'jds/find-file-other-project
  "ft" #'projectile-run-vterm
  ;; "fp" #'consult-project-extra-find
  ;; "fP" #'consult-project-extra-find-other-window
  "fo" #'ruled-switch-buffer
  "fO" #'ruled-switch-buffer-other-window)

;;; help bindings
(jds/leader-def
  "h" '(:ignore t :which-key "help")
  "hk" #'helpful-key
  "hK" #'describe-keymap
  "hf" #'helpful-callable
  "hv" #'helpful-variable
  "hF" #'describe-face
  ;; "ha" #'consult-apropos
  "ha" #'helpful-symbol
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
  ;; "tm" #'hide-mode-line-mode
  ;; "to" #'olivetti-mode
  )

;;; other
(jds/leader-def
  "S" #'jds/screenshot-dragon-temp-file
  "u" #'undo-tree-visualize)

;;; buffers


(setq jds~scratch-keymap (make-sparse-keymap))
(defmacro jds~execute-in-scratch (body)
    "Switch to scratch buffer and execute body."
    `(lambda () (interactive) 
	(progn (switch-to-buffer (get-buffer-create "*scratch*"))
	       ,body)))

(general-define-key
 :keymaps 'jds~scratch-keymap
 "x" (jds~execute-in-scratch (fundamental-mode))
 "o" (jds~execute-in-scratch (org-mode))
 "e" (jds~execute-in-scratch (emacs-lisp-mode))
 "t" (jds~execute-in-scratch (LaTeX-mode))
 "r" (jds~execute-in-scratch (ess-r-mode)))

(jds/leader-def
  "b"  '(:ignore t :which-key "buffer/bookmark")
  "bb" #'consult-buffer
  "bB" #'consult-buffer-other-frame
  "bo" #'consult-bookmark
  "bd" #'kill-current-buffer
  "bm" #'bookmark-set
  "bM" #'bookmark-delete
  "br" #'revert-buffer
  "bi" #'ibuffer-jump
  "bI" #'ibuffer-other-window
  ;; "bx" '((lambda () (interactive) (switch-to-buffer (get-buffer-create "*scratch*"))) :which-key "scratch")
  "bx" jds~scratch-keymap
  "bz" '((lambda () (interactive) (switch-to-buffer (messages-buffer))) :which-key "messages")
  "bs" #'(lambda () (interactive) (switch-to-buffer (get-buffer-create "*scratch*"))))

(general-define-key
 :keymaps 'ibuffer-mode-map
 :states '(mnv)
 "f" #'avy-goto-line)

;;; text editing
(jds/leader-def
  "a"      '(:ignore t :wk "editing")
  ;; "at"     #'synosaurus-choose-and-replace
  "at"     #'powerthesaurus-lookup-dwim
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
  "as" #'cape-elisp-symbol
  ;; "ab" #'cape-abbrev
  "al" #'cape-line
  "aw" #'cape-dict
  "a\\" #'cape-tex
  "ab" #'delete-blank-lines)

;;; repls and such
(jds/leader-def
  "r"    '(:ignore t :wk "REPL")
  "rr"   #'R ;; or run-ess-r 
  "rp"   #'run-python
  "rl"   #'run-lisp)

(defmacro jds~dired-find-file (file)
    "Macro to create interactive lambda opening dired at path. Used for bindings."
    `(lambda () (interactive) (find-file ,file)))

;;;###autoload
(defun jds/switch-to-agenda-file (fn)
    "Display file listed org-directory."
  (interactive)
  (find-file
   (expand-file-name fn org-directory)))


(jds/leader-def
  "j" '(:ignore t :which-key "jump")
  "j n"   #'dired-hist-go-forward
  "j b"   #'dired-hist-go-back
  "j d"   `(,(jds~dired-find-file "~/Downloads/") :which-key "Downloads")
  "j o"   `(,(jds~dired-find-file "~/Dropbox/org/") :which-key "Org")
  "j h"   `(,(jds~dired-find-file "~/") :which-key "Home")
  "j c"   `(,(jds~dired-find-file "~/.emacs.d/") :which-key "Emacs")
  "j m"   `(,(jds~dired-find-file "/run/media/jds6696/") :which-key "Media")
  "j r"   `(,(jds~dired-find-file "~/Dropbox/org/roam/references/references.bib") :which-key "references")
  "j a" (lambda () (interactive) (find-file "~/Dropbox/Buisness/anarres/cash_flow/2025_anarres_cash_flow.xlsx"))
  "j f" (lambda () (interactive) (find-file "~/Dropbox/Buisness/homewood_farm/cash_flow_records/homewood_cashflow_2025.xlsx"))
  "j t" '((lambda () (interactive) (jds/switch-to-agenda-file "tasks.org")) :which-key "tasks"))

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

(defmacro jds~roam-exclude-dwim (body)
    "Create new lambda function that wraps functions like org-store-link to smarly add ROAM_EXCLUDE tag."
    `(lambda (&optional arg)
       (interactive "P")
       (let* ((inorg (string= major-mode "org-mode"))
	      (nodep (if inorg (org-roam-db-node-p) nil))
	      (inroam (string-prefix-p (expand-file-name org-roam-directory) (buffer-file-name)))
	      (include (if inorg  (not (member "ROAM_EXCLUDE" (org-get-tags))) nil)))
	 ,body
	 (unless arg
	   (if (and inorg
		    inroam
		    include
		    (not nodep))
	       (org-roam-tag-add '("ROAM_EXCLUDE")))))))



;;;###autoload
(setq jds~task-files (mapcar (lambda (relfile) (expand-file-name relfile org-directory))
			     (list "tasks.org" "resources.org" "mail.org" "inbox.org" "inbox_mobile.org" "tasks-homewood.org")))
(defun jds/consult-org-agenda-or-ripgrep-all-headlines (&optional arg)
  "Run consult-org-agenda or with prefix, run jds/consult-org-agenda-or-ripgrep-all-headlines."
  (interactive "P")
  (let ((prefix (prefix-numeric-value current-prefix-arg)))
    (cond
     ((eq prefix 4) (consult-org-agenda))
     ((eq prefix 16) (jds/consult-ripgrep-all-org-headlines))
     (t (consult-org-heading nil jds~task-files)))))

(jds/sub-leader-def
  "," #'org-capture   ;; q "new"
  "C-," #'org-capture ;; q "new"
  ;; "<" #'org-capture-goto-target ;; new and follow
  ">" #'org-refile-goto-last-stored
  "<" #'org-capture-goto-last-stored
  "a" #'jds/affe-find-files-home
  "A" #'consult-locate
  "z" #'zoxide-find-file
  "Z" #'zoxide-cd
  "g" #'magit-status
  "m" #'jds/mu4e-goto-todays-headers
  "M" #'(lambda (&optional arg) (interactive "P")
	  (jds~new-frame-or-new-window arg)
	  (jds/mu4e-goto-todays-headers))
  ;; "n" #'org-roam-node-find
  "n" (lambda () (interactive) (let ((consult-preview-key nil))
				 (call-interactively #'org-roam-node-find)))
  ";" #'org-roam-capture
  "l" (jds~roam-exclude-dwim (org-store-link nil t))
  ;; "L" #'org-super-links-store-link
  "i" #'org-insert-link
  ;; "I" #'org-super-links-insert-link
  ;; "o" #'jds/org-super-links-link-or-delete
  ;; "O" #'org-super-links-quick-insert-drawer-link
  "t" #'jds/multi-vterm-same-window
  "T" #'jds/multi-vterm-new-window-or-frame
  "f" #'jds/org-agenda-show-custom-day
  "F" (lambda () (interactive)
	(split-window-right)
	(other-window 1)
	(org-agenda nil "d"))
  "p" #'org-agenda
  "s" #'jds/consult-org-agenda-or-ripgrep-all-headlines
  ;; "s" #'consult-org-agenda
  ;; "S" #'org-search-view
  ;; "s" #'jds/consult-ripgrep-all-org-headlines
  "S" #'jds/consult-ripgrep-all-org
  "y" (jds~start-slack-function slack-select-rooms)
  "Y" (jds~start-slack-function slack-select-unread-rooms)
  "k" #'jds/help-thing-at-point
  "d" #'jds/dired-jump-and-kill-buffer
  "D" #'dired-jump-other-window
  "c" #'jds/mu4e-compose-goto-to
  "e" #'consult-mu
  "E" (lambda () (interactive) (consult-mu "flag:attach "))
  "C" #'consult-mu-contacts) 

;;; faster editing in text buffers ------


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
  "gs" #'magit-list-repositories
  "gg" #'magit-status
  "gt" #'git-timemachine
  "gb" #'magit-blame
  "gB" #'magit-blame
  "gf" #'magit-fetch
  "gF" #'magit-pull
  "gp" #'magit-push
  "g@" #'forge-dispatch
  "g%" #'magit-worktree
  "gl" #'magit-log
  "gr" #'browse-at-remote)

;;; apps

(jds/leader-def
  "o" '(:ignore t :which-key "other/apps")
  ;; "os" #'jds/hydra-spotify-wrapper
  "oc" #'calendar
  "or" #'jds/citar-open-prioritize-global-bib
  "op" #'proced
  "od" #'hydra-edebug/body
  "ob" #'ebib
  "oz" #'(lambda () (interactive) (jds~launch-zoom-by-conference-number 3697254414)))

(with-eval-after-load 'engine-mode
  (jds/leader-def
      "os" engine-mode-prefixed-map))

;;; notes and org/org-roam stuff
(jds/leader-def
  "l" '(:ignore t :which-key "notes")
  "ls" #'jds/consult-org-roam-and-agenda-search-headlines
  "ln" #'org-roam-node-find
  "lS" #'consult-org-roam-search
  "lf" #'consult-org-roam-file-find
  "ll" #'consult-org-roam-backlinks)


;; eglot/lsp bindings
(jds/leader-def
  "e" '(:ignore t :which-key "LSP")
  "er" #'eglot-rename
  "ef" #'eglot-format
  "ea" #'eglot-code-actions
  "ee" #'consult-eglot-symbols
  "eh" #'eldoc
  "em" #'consult-flymake)

;; AI/gptel bindings
(jds/leader-def
  "d" '(:ignore t :which-key "AI")
  "dd" #'gptel
  "ds" #'gptel-send
  "da" #'gptel-add
  "dr" #'gptel-rewrite)

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
 "gR" #'(lambda () (interactive) (let ((edebug-all-forms t))
				   (call-interactively #'eval-region)))
 "gc" #'evilnc-comment-operator
 "gC" #'evilnc-copy-and-comment-operator
 "g;" #'goto-last-change
 "g/" #'avy-goto-char-timer
 "g," #'jds/evil-snipe-convert-avy-jump
 "gm" #'evil-next-match	;; select prior search
 ;; "gM" held for things like jds/avy-latex-math
 "gn" #'jds/evil-search-convert-avy-jump
 ;; "gi" #'evil-insert-resume
 "gv" #'evil-visual-restore
 "gp" #'+evil/reselect-paste
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
 ;; "ga" #'ace-window
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
 ;; "zg" #'jds/save-word
 "zg" #'jds/flyspell-correct-word-then-abbrev
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
 "zF"  #'find-file-other-window
 "zd"  #'xref-find-definitions
 "zD"  #'xref-find-definitions-other-window
 "zs"  #'xref-find-references
 "zl"  #'evil-lion-left
 "zL"  #'evil-lion-right
 "zn"  #'flymake-goto-next-error
 "zN"  #'flymake-goto-prev-error)

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
 ;; "f"  #'+evil:defun-textobj
 "c"  #'evilnc-inner-comment
 "a"  #'evil-inner-arg
 ;; "d"  #'evil-inner-delim
 )


(general-otomap
 :state 'o
 "g"  #'+evil:whole-buffer-txtobj
 ;; "f"  #'+evil:defun-textobj
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
  ;; (key-chord-define-global "jk" 'evil-force-normal-state)
  (key-chord-define-global "wq" 'save-buffer)
  (key-chord-define-global "WQ" 'evil-write-all))

(defun jds~org-agenda-local-bindings ()
  (key-chord-define-local "wq" #'org-save-all-org-buffers))
(add-hook 'org-agenda-mode-hook 'jds~org-agenda-local-bindings)

;; Idea for this binding came from regardtoo evil configuration
(general-vmap
  :keymap 'global
  "v" #'er/expand-region
  "V" #'er/contract-region)

