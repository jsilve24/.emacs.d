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
                 args))))

;; Configure directory extension.
;; NOTE: The file `vertico-directory.el' must be installed manually.
(use-package vertico-directory
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(straight-use-package 'orderless)
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(basic partial-completion orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(straight-use-package 'savehist)
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

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


(straight-use-package 'embark)
(use-package embark
  :ensure t
  :after which-key

  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ("C-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
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
              :around #'embark-hide-which-key-indicator))


;;; consult
;; Install Consult if you want additional featureful completion commands, e.g, the buffer switcher
;; consult-buffer with preview or the line-based search consult-line.

(straight-use-package 'consult)
(use-package consult
  :defer t
  :bind (
  ([remap apropos]                      . #'consult-apropos)
  ([remap bookmark-jump]                . #'consult-bookmark)
   ([remap evil-show-marks]              . #'consult-mark)
   ([remap evil-show-jumps]              . #'+vertico/jump-list)
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
   ([remap persp-switch-to-buffer]       . #'+vertico/switch-workspace-buffer))
  :init
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (advice-add #'multi-occur :override #'consult-multi-occur))


;;; embark-consult
;; Install Embark-Consult and Wgrep for export from consult-line to occur-mode buffers and from
;; consult-grep to editable grep-mode buffers.
;;
;; If you use the grepping
;; commands from the Consult package, consult-grep, consult-git-grep or consult-ripgrep, then you’ll
;; probably want to install and load the embark-consult package, which adds support for exporting a
;; list of grep results to an honest grep-mode buffer, on which you can even use wgrep if you wish.

;; (straight-use-package 'embark)
;; ;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :ensure t
;;   :after (embark consult)
;;   :demand t ; only necessary if you have the hook below
;;   ;; if you want to have consult previews as you move around an
;;   ;; auto-updating embark collect buffer
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))


;; (straight-use-package 'helpful)
;; (use-package helpful
;;   :commands (helpful-callable helpful-variable helpful-command helpful-key)
;;   :custom
;;   ;; (counsel-describe-function-function #'helpful-callable)
;;   ;; (counsel-describe-variable-function #'helpful-variable)
;;   :bind
;;   ;; ([remap describe-function] . counsel-describe-function)
;;   ([remap describe-command] . helpful-command)
;;   ;; ([remap describe-variable] . counsel-describe-variable)
;;   ([remap describe-key] . helpful-key))



(provide 'completing-read)
;;; completing-read.el ends here
