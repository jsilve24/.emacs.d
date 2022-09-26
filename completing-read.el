;;; completing-read.el --- vertico and co -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jds6696@psu.edu>
;; Created: October 21, 2021
;; Modified: October 21, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/completing-read
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;;; vertico orderless and savehist

(straight-use-package '(vertico :files (:defaults "extensions/*")
                                :includes (vertico-buffer
                                           vertico-directory
                                           vertico-flat
                                           vertico-indexed
                                           vertico-mouse
                                           vertico-quick
                                           vertico-repeat
                                           vertico-reverse)))
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-resize nil
	vertico-count 17
	vertico-cycle t
	completion-in-region-function
	(lambda (&rest args)
	  (apply (if vertico-mode
		     #'consult-completion-in-region
		   #'completion--in-region)
		 args)))
  :config
  ;; setup to allow resuming last vertico session (with vertico-repeat)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  ;; setup vertico quick
  (define-key vertico-map "\M-q" #'vertico-quick-exit)
  (define-key vertico-map "\M-Q" #'vertico-quick-insert)

  ;; setup vertico multiform
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
	'((consult-imenu buffer)
	  (consult-imenu-multi buffer)
	  (consult-outline buffer)
	  (consult-ripgrep buffer)
	  (consult-org-agenda buffer)
	  (jds/smart-consult-outline-imenu buffer)
	  (jds/consult-org-roam-and-agenda buffer)
	  (jds/consult-ripgrep-config buffer)))
  ;; give vertico-map precedence over evil maps
  (evil-make-intercept-map vertico-map)
  (general-define-key
   :states '(e m i)
   :keymaps 'vertico-map
   "ESC" #'minibuffer-keyboard-quit)
  ;; setup keys to temporarily toggle between display modes
  (define-key vertico-map "\M-V" #'vertico-multiform-vertical)
  (define-key vertico-map "\M-G" #'vertico-multiform-grid)
  (define-key vertico-map "\M-F" #'vertico-multiform-flat)
  (define-key vertico-map "\M-R" #'vertico-multiform-reverse)
  (define-key vertico-map "\M-U" #'vertico-multiform-unobtrusive)

  (define-key vertico-map "\C-f" #'vertico-scroll-up)
  (define-key vertico-map "\C-b" #'vertico-scroll-down))
;; Configure directory extension.
;; NOTE: The file `vertico-directory.el' must be installed manually.
(use-package vertico-directory
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  ;; tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  ;; (setq completion-styles '(basic partial-completion orderless)
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  ;; allow orderless completion in company
  (setq orderless-component-separator "[ &]"))

;; The matching portions of candidates aren’t highlighted. That’s because company-capf is hard-coded
;; to look for the completions-common-part face, and it only use one face, company-echo-common to
;; highlight candidates.
;; (with-eval-after-load 'company
;;   (defun just-one-face (fn &rest args)
;;     (let ((orderless-match-faces [completions-common-part]))
;;       (apply fn args)))
;;   (advice-add 'company-capf--candidates :around #'just-one-face))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  ;; (defun crm-indicator (args)
  ;;   (cons (concat "[CRM] " (car args)) (cdr args)))
  ;; (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


;;; marginalia

(straight-use-package 'marginalia)
;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)
  :config
  ;; from doom
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;;; embark
;; Install Embark and add two keybindings for embark-dwim and embark-act. I am using M-. and
;; C-.. These commands allow you to act on the object at point or in the minibuffer.
;; investigate embark-keymap-alist for finding correct embark-keymaps

(use-package embark
  :straight (embark :type git :host github :repo "oantolin/embark"
		    :files ("embark.el" "embark.texi" "embark.info" "embark-org.el"))
  :ensure t
  :after which-key
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (require 'embark-org)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none))))

  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
	  (which-key--hide-popup-ignore-command)
	(which-key--show-keymap
	 (if (eq (plist-get (car targets) :type) 'embark-become)
	     "Become"
	   (format "Act on %s '%s'%s"
		   (plist-get (car targets) :type)
		   (embark--truncate-target (plist-get (car targets) :target))
		   (if (cdr targets) "…" "")))
	 (if prefix
	     (pcase (lookup-key keymap prefix 'accept-default)
	       ((and (pred keymapp) km) km)
	       (_ (key-binding prefix 'accept-default)))
	   keymap)
	 nil nil t (lambda (binding)
		     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
	'(embark-which-key-indicator
	  embark-highlight-indicator
	  embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (when-let ((win (get-buffer-window which-key--buffer
				       'visible)))
      (quit-window 'kill-buffer win)
      (let ((embark-indicators (delq #'embark-which-key-indicator embark-indicators)))
	(apply fn args))))

  (advice-add #'embark-completing-read-prompter
	      :around #'embark-hide-which-key-indicator)


  ;; this is awesome -- taken from here: https://karthinks.com/software/fifteen-ways-to-use-embark/
  (eval-when-compile
    (defmacro jds/embark-ace-action (fn)
      `(defun ,(intern (concat "jds/embark-ace-" (symbol-name fn))) ()
	 (interactive)
	 (with-demoted-errors "%s"
	   (require 'ace-window)
	   (let ((aw-dispatch-always t)
		 (aw-dispatch-alist '((?F aw-split-window-fair "Split Fair Window")
				      (?s aw-split-window-vert "Split Vert Window")
				      (?v aw-split-window-horz "Split Horz Window")
				      (?o delete-other-windows "Delete Other Windows"))))
	     (aw-switch-to-window (aw-select nil))
	     (call-interactively (symbol-function ',fn)))))))

  (define-key embark-file-map (kbd "o") (jds/embark-ace-action find-file))
  (define-key embark-buffer-map (kbd "o") (jds/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (jds/embark-ace-action bookmark-jump))
  (define-key embark-org-link-map (kbd "o") (jds/embark-ace-action org-open-at-point))
  (define-key embark-url-map (kbd "o") (jds/embark-ace-action browse-url))



  (eval-when-compile
    (defmacro jds/embark-split-action (fn split-type)
      `(defun ,(intern (concat "jds/embark-"
			       (symbol-name fn)
			       "-"
			       (symbol-name split-type))) ()
	 (interactive)
	 (funcall #',split-type)
	 (call-interactively #',fn))))

  (define-key embark-file-map (kbd "s") (jds/embark-split-action find-file +evil/window-split-and-follow))
  (define-key embark-buffer-map (kbd "s") (jds/embark-split-action switch-to-buffer +evil/window-split-and-follow))
  (define-key embark-bookmark-map (kbd "s") (jds/embark-split-action bookmark-jump +evil/window-split-and-follow))
  (define-key embark-org-link-map (kbd "s") (jds/embark-split-action org-open-at-point +evil/window-split-and-follow))
  (define-key embark-url-map (kbd "s") (jds/embark-split-action browse-url +evil/window-split-and-follow))



  (define-key embark-file-map (kbd "v") (jds/embark-split-action find-file +evil/window-vsplit-and-follow))
  (define-key embark-buffer-map (kbd "v") (jds/embark-split-action switch-to-buffer +evil/window-vsplit-and-follow))
  (define-key embark-bookmark-map (kbd "v") (jds/embark-split-action bookmark-jump +evil/window-vsplit-and-follow))
  (define-key embark-org-link-map (kbd "v") (jds/embark-split-action org-open-at-point +evil/window-vsplit-and-follow))
  (define-key embark-url-map (kbd "v") (jds/embark-split-action browse-url +evil/window-vsplit-and-follow))



  ;; create new embark keymap for org-headings
  (add-to-list 'marginalia-prompt-categories '("Go to heading:" . consult-org-heading))
  (embark-define-keymap embark-consult-org-heading-map
    "Keymap to use with embark and consult-org-heading")
  (add-to-list 'embark-keymap-alist '(consult-org-heading . embark-consult-org-heading-map))
  (define-key embark-consult-org-heading-map (kbd "o") (jds/embark-ace-action consult-org-agenda))
  (define-key embark-consult-org-heading-map (kbd "v") (jds/embark-split-action consult-org-agenda split-window-right))
  (define-key embark-consult-org-heading-map (kbd "s") (jds/embark-split-action consult-org-agenda split-window-below))


  ;; open file as sudo
  (defun sudo-find-file (file)
    "Open FILE as root."
    (interactive "FOpen file as root: ")
    (when (file-writable-p file)
      (user-error "File is user writeable, aborting sudo"))
    (find-file (if (file-remote-p file)
		   (concat "/" (file-remote-p file 'method) ":"
			   (file-remote-p file 'user) "@" (file-remote-p file 'host)
			   "|sudo:root@"
			   (file-remote-p file 'host) ":" (file-remote-p file 'localname))
		 (concat "/sudo:root@localhost:" file))))

  (define-key embark-file-map (kbd "S") 'sudo-find-file))


;;; consult
;; Install Consult if you want additional featureful completion commands, e.g, the buffer switcher
;; consult-buffer with preview or the line-based search consult-line.

(use-package consult
  :defer t
  :bind
  (([remap apropos]                      . #'consult-apropos)
   ([remap bookmark-jump]                . #'consult-bookmark)
   ([remap evil-show-marks]              . #'consult-mark)
   ;; ([remap evil-show-jumps]              . #'+vertico/jump-list)
   ([remap goto-line]                    . #'consult-goto-line)
   ([remap imenu]                        . #'consult-imenu)
   ([remap locate]                       . #'consult-locate)
   ([remap load-theme]                   . #'consult-theme)
   ([remap man]                          . #'consult-man)
   ([remap recentf-open-files]           . #'consult-recent-file)
   ([remap switch-to-buffer]             . #'consult-buffer)
   ([remap switch-to-buffer-other-window]. #'consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . #'consult-buffer-other-frame)
   ([remap yank-pop]                     . #'consult-yank-pop)
   ;; ([remap persp-switch-to-buffer]       . #'+vertico/switch-workspace-buffer)
   )

  :init
  ;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (advice-add #'multi-occur :override #'consult-multi-occur)

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
	register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 1.5 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; consult-buffer filter buffer list
  ;; (add-to-list 'consult-buffer-filter "\\*straight-process\\*")
  (add-to-list 'consult-buffer-filter "\\*helpful*")
  (add-to-list 'consult-buffer-filter "\\*Apropos\\*")
  (add-to-list 'consult-buffer-filter "\\*zoxide\\*")
  (add-to-list 'consult-buffer-filter "\\*trace*")
  (add-to-list 'consult-buffer-filter "\\*sent draft\\*")
  ;; (add-to-list 'consult-buffer-filter "\\*splash\\*")

  ;; combine sources for consult-buffer
  (setq consult-buffer-sources '(consult--source-buffer
				 consult--source-bookmark
				 consult-projectile--source-projectile-file
				 consult-projectile--source-projectile-project
				 consult--source-recent-file))

  ;; add annotation to consult-org-agenda
  ;; (consult-customize
  ;;  consult-org-agenda
  ;;  :annotate (lambda (&optional foo)
  ;; 	       (let ((end (save-excursion (outline-next-heading) (point))))
  ;; 		 (if (or (re-search-forward org-ts-regexp end t)
  ;; 			 (re-search-forward org-ts-regexp-both end t))
  ;; 		     (match-string 0))) ))




  ;; dont' preview exwm buffers
  ;; see: https://github.com/minad/consult/wiki#do-not-preview-exwm-windows-or-tramp-buffers
(defun consult-buffer-state-no-x ()
  "Buffer state function that doesn't preview X buffers."
  (let ((orig-state (consult--buffer-state))
        (filter (lambda (action cand)
                  (if (or (eq action 'return)
			  (if cand
                              (let ((buffer (get-buffer cand)))
				(and buffer
                                     (not (eq 'exwm-mode (buffer-local-value 'major-mode buffer)))))))
                      cand
                    nil))))
    (lambda (action cand)
      (funcall orig-state action (funcall filter action cand)))))

(setq consult--source-buffer
      (plist-put consult--source-buffer :state #'consult-buffer-state-no-x))
  
    ;; group exwm buffers together
  (defun exwm-all-buffers ()
    (seq-filter
     (lambda (buffer)
       (eq 'exwm-mode (buffer-local-value 'major-mode buffer)))
     (buffer-list)))
  (defvar exwm-buffer-source
    `(:name "EXWM"
	    :hidden t
	    :narrow ?x
	    :category buffer
	    :state ,#'consult--buffer-state
	    :items ,(lambda () (mapcar #'buffer-name (exwm-all-buffers)))))
  (add-to-list 'consult-buffer-sources 'exwm-buffer-source 'append)


  (setq consult-narrow-key "\\"
	consult-line-numbers-widen t
	consult-async-min-input 2
	consult-async-refresh-delay 0.15
	consult-async-input-throttle 0.2
	consult-async-input-debounce 0.1)

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
;;;; 1. project.el (project-roots)
  ;; (setq consult-project-root-function
  ;; 	(lambda ()
  ;; 	  (when-let (project (project-current))
  ;; 	    (car (project-roots project)))))
;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))

  ;; customize ripgrep
  (setq consult-ripgrep-args "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number --hidden .")
  ;; (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number --hidden .")
  )

;;; set outline regex for consult-outline and more 
;; (setq outline-regexp "[*\f]+")


;;; embark-consult
;; Install Embark-Consult and Wgrep for export from consult-line to occur-mode buffers and from
;; consult-grep to editable grep-mode buffers.
;;
;; If you use the grepping
;; commands from the Consult package, consult-grep, consult-git-grep or consult-ripgrep, then you’ll
;; probably want to install and load the embark-consult package, which adds support for exporting a
;; list of grep results to an honest grep-mode buffer, on which you can even use wgrep if you wish.

(straight-use-package 'embark-consult)
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(straight-use-package 'helpful)
(use-package helpful
  ;; a better *help* buffer
  :commands helpful--read-symbol
  :init
  ;; Make `apropos' et co search more extensively. They're more useful this way.
  (setq apropos-do-all t)

  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol))

;;; wgrep

(straight-use-package 'wgrep)
(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))


;;; fluff
(straight-use-package 'all-the-icons-completion)
(use-package all-the-icons-completion
  :after marginalia
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(provide 'completing-read)
;;; completing-read.el ends here
