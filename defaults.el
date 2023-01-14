;;; defaults.el --- tweak emacs defaults -*- lexical-binding: t -*-


;;; Performance and GC settings-------------------------------------------------

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
      initial-scratch-message nil)

;;; Visual Niceties ------------------------------------------------------------

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Disable line numbers for some modes
(dolist (mode '(;;org-mode-hook
                term-mode-hook
		slack-message-buffer-mode-hook
		slack-file-info-buffer-mode-hook
		slack-file-list-buffer-mode-hook
                pdf-view-mode-hook
		doc-view-mode-hook
		vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                mu4e-main-mode-hook
		mu4e-view-mode-hook
                mu4e-main-index-update-hook
		org-capture-before-finalize-hook
		mu4e-headers-mode-hook
                org-agenda-mode-hook
		dired-sidebar-mode
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; turn off bell
(setq ring-bell-function 'ignore)

;; turn on visual line mode
(global-visual-line-mode 1)

;; turn on highlight matching brackets when cursor is on one
(show-paren-mode 1)

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

;; Without this, Emacs will try to resize itself to a specific column size, but like Tony, I’m on a
;; tiling wm, and I change font sizes all the time, so this is no good.
(setq frame-inhibit-implied-resize t)

;; I think this may require emacs 29
(setq pixel-scroll-precision-mode t)


;;; Minibuffer -----------------------------------------------------------------

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


;;; Assorted Defaults ----------------------------------------------------------

;; If non-nil, displaying help for an autoloaded function whose
;; 'autoload' form provides no documentation string will try to load the
;; file it's from.  This will give more extensive help for such
;; functions.
(setq help-enable-symbol-autoload t)

;; New user option 'describe-bindings-outline'.
;; It enables outlines in the output buffer of 'describe-bindings' that
;; can provide a better overview in a long list of available bindings.
(setq describe-bindings-outline t)

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

;; winner mode
(winner-mode 1)

;; save place between sessions
(save-place-mode 1)

;; sentence setup
(setq sentence-end-double-space nil)

;; don't backup by moving file 
;; https://idiomdrottning.org/bad-emacs-defaults
(setq backup-by-copying t)

;; don't litter with backups all over the place
(make-directory "~/.emacs_backups/" t)
(make-directory "~/.emacs_autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs_autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs_backups/")))

;; require final newline in files
(setq require-final-newline t)

;;; recentf and savehist -------------------------------------------------------

;; track recent files
(use-package recentf
  :init
  (setq recentf-max-menu-items 300
        recentf-max-saved-items 500
        recentf-auto-cleanup 'mode)
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list)

  ;; exclude some files from tracking
  ;; if you add something here you then need to run recentf-cleanup to take effect
  (add-to-list 'recentf-exclude "~/\\.mail/*")
  (add-to-list 'recentf-exclude "*\\.fdb_latexmk")
  (add-to-list 'recentf-exclude "*\\.fls")
  (add-to-list 'recentf-exclude "^/tmp/*")
  (add-to-list 'recentf-exclude "\\.synctex\\.gz$")
  (add-to-list 'recentf-exclude "recentf-save\\.el")
  (recentf-cleanup))


;; Keep folders Clean
(use-package no-littering
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode)
  :config
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t))
