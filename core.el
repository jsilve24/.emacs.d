;;; core.el --- Core Config Stuff -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jds6696@psu.edu>
;; Created: October 20, 2021
;; Modified: October 20, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/core
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Core Config Stuff
;;
;;; Code:


;;; Start-up Performance

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Nice visual showing startup-time
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; The GC introduces annoying pauses and stuttering into our Emacs experience,
;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
;; when it's idle. However, if the idle delay is too long, we run the risk of
;; runaway memory usage in busy sessions. If it's too low, then we may as well
;; not be using gcmh at all.
(setq gcmh-idle-delay 'auto  ; default is 15s
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; PGTK builds only: this timeout adds latency to frame operations, like
;; `make-frame-invisible', which are frequently called without a guard because
;; it's inexpensive in non-PGTK builds. Lowering the timeout from the default
;; 0.1 should make childframes and packages that manipulate them (like `lsp-ui',
;; `company-box', and `posframe') feel much snappier. See emacs-lsp/lsp-ui#613.
(setq pgtk-wait-for-event-timeout 0.001)

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Shave seconds off startup time by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
      ;; pull in a ton of packages. `doom/open-scratch-buffer' provides a better
      ;; scratch buffer anyway.
      initial-major-mode 'fundamental-mode
      initial-buffer-choice (lambda () (progn
					 (switch-to-buffer (get-buffer-create "*splash*"))
					 (splash--setup)))
      initial-scratch-message nil)


;;; Package System Setup

;; Install Straight -- per straight documentation
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Install use-package through straight 
;; Now use-package will use straight.el to automatically install
;; missing packages if you provide :straight t:
(straight-use-package 'use-package)

; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

;; default to install use-package recipes
;; (Note that the variable use-package-always-ensure is
;; associated with package.el, and you should not use it with straight.el.)
(setq straight-use-package-by-default t)


;;; Keybinding Utilities
(straight-use-package 'general)
(use-package general
  :ensure t
  :config
  ;; allow for shorter bindings -- e.g., just using things like nmap alone without general-* prefix
  (general-evil-setup t)

  ;; To automatically prevent Key sequence starts with a non-prefix key errors without the need to
  ;; explicitly unbind non-prefix keys, you can add (general-auto-unbind-keys) to your configuration
  ;; file. This will advise define-key to unbind any bound subsequence of the KEY. Currently, this
  ;; will only have an effect for general.el key definers. The advice can later be removed with
  ;; (general-auto-unbind-keys t).
  (general-auto-unbind-keys)


  (general-create-definer jds/leader-def
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer jds/sub-leader-def
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix ","
    :global-prefix "C-,")
  (general-create-definer jds/localleader-def
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "\\"
    :global-prefix "C-\\")
  (general-create-definer jds/sub-localleader-def
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "<backspace>")

  ;; move macros
  ;; (general-define-key
  ;;  :states '(n v)
  ;;  "Q" #'evil-record-macro)
  )






;;; Visual Niceties

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(;;org-mode-hook
                term-mode-hook
                pdf-view-mode-hook
		vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                mu4e-main-mode-hook
		mu4e-view-mode-hook
                mu4e-main-index-update-hook
		org-capture-before-finalize-hook
		mu4e-headers-mode-hook
                org-agenda-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;; Fringes
;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; GUIs are inconsistent across systems and themes (and will rarely match our
;; active Emacs theme). They impose inconsistent shortcut key paradigms too.
;; It's best to avoid them altogether and have Emacs handle the prompting.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(setq x-gtk-use-system-tooltips nil)

 ;; Favor vertical splits over horizontal ones. Monitors are trending toward
 ;; wide, rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)

;;; Minibuffer

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Typing yes/no is obnoxious when y/n will do
(fset #'yes-or-no-p #'y-or-n-p)

;; from here: http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
;; this is a life saver
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;;; Random
;; (defun flash-mode-line ()
;;   (invert-face 'mode-line)
;;   (run-with-timer 0.1 nil #'invert-face 'mode-line))
;; (setq visible-bell nil
;;       ring-bell-function
;;          (lambda ()
;; 	   (unless (memq this-command
;; 			 '(isearch-abort
;; 			   abort-recursive-edit
;; 			   exit-minibuffer
;; 			   keyboard-quit))
;; 	     (flash-mode-line))))

;; sentence setup
(setq sentence-end-double-space nil)

;;; TODO not honestly sure what this does want it in mu4e headers but not sure about elsewhere
;; (setq-default truncate-lines t)

;; turn on visual line mode
(global-visual-line-mode 1)

;; turn on highlight matching brackets when cursor is on one
(show-paren-mode 1)

;; winner mode
(winner-mode 1)

;; track recent files
(use-package recentf
  :after evil
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 100
        recentf-max-saved-items 400
        recentf-auto-cleanup 'never)
  (run-at-time nil (* 5 60) 'recentf-save-list)

  ;; exclude some files from tracking
  ;; if you add something here you then need to run recentf-cleanup to take effect
  (add-to-list 'recentf-exclude "~/\\.mail/*")
  (add-to-list 'recentf-exclude "^/tmp/*")
  (add-to-list 'recentf-exclude "\\.synctex\\.gz$")
  (add-to-list 'recentf-exclude "recentf-save\\.el")
  (recentf-cleanup))


;; Keep folders Clean
(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))


;; Emacs stores `authinfo' in $home and in plain-text - lets not do that.
(setq auth-sources '(default
		    "secrets:default"
		    "secrets:login"
		    "~/.authinfo.gpg"))


;; Can't get by without which-key
(straight-use-package 'which-key)
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))


;;; pkg info
(use-package pkg-info
  :straight t
  :defer t)

;;; hydras

(use-package hydra
  :commands hydra-resize/body
  :config
  ;; window resize hydra
  (defhydra hydra-resize ()
    ("+" text-scale-increase "zoom-in")
    ("-" text-scale-decrease "zoom-out")
    ("h" (lambda () (interactive) (shrink-window-horizontally 2)) "-narrower-")
    ("l" (lambda () (interactive) (enlarge-window-horizontally 2)) "-wider")
    ("j" (lambda () (interactive) (shrink-window 2)) "|shorter|")
    ("k" (lambda () (interactive) (enlarge-window 2)) "|longer|")
    ("=" balance-windows "balance")
    ("q" nil "quit")))

(provide 'core)
;;; core.el ends here
