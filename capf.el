;;; capf.el --- capf config -*- lexical-binding: t -*-

(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-history
			      corfu-quick
			      corfu-popupinfo))

  ;; :bind
  ;; (:map corfu-map
  ;; 	("TAB" . corfu-next)
  ;; 	([tab] . corfu-next)
  ;; 	("S-TAB" . corfu-previous)
  ;; 	([backtab] . corfu-previous))
  ;; Optional customizations
  :custom
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)  ;; Enable auto completion
  (corfu-auto-delay .5)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current t) ;; Enable current candidate preview
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode)
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
	  completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)

  ;; sort candidates by use
  (corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  ;; setup corfu popup (used to be corfu-doc)
  (setq corfu-popupinfo-auto t
	corfu-popupinfo-delay 0.75)
  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
  (define-key corfu-map (kbd "M-d") #'corfu-popupinfo-toggle)
  (define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down) ;; corfu-next
  (define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up) ;; corfu-previous
  


  ;; setup corfu quick
  (define-key corfu-map "\M-q" #'corfu-quick-complete)
  (define-key corfu-map "\M-Q" #'corfu-quick-insert)

  ;; Advise packages that use posframe for a multi-head setup
  ;; Fixes this issue: https://github.com/minad/corfu/discussions/408
  ;; From this config: 
  ;; https://github.com/Momo-Softworks/Momomacs/blob/e1cdcb6e022be0a59594129ef1e61ea8de51d000/modules/system/exwm.el#L69
  (defun get-focused-monitor-geometry ()
    "Get the geometry of the monitor displaying the selected frame in EXWM."
    (let* ((monitor-attrs (frame-monitor-attributes))
           (workarea (assoc 'workarea monitor-attrs))
           (geometry (cdr workarea)))
      (list (nth 0 geometry) ; X
            (nth 1 geometry) ; Y
            (nth 2 geometry) ; Width
            (nth 3 geometry) ; Height
            )))
  
  (defun advise-corfu-make-frame-with-monitor-awareness (orig-fun frame x y width height buffer)
    "Advise `corfu--make-frame` to be monitor-aware, adjusting X and Y according to the focused monitor."

    ;; Get the geometry of the currently focused monitor
    (let* ((monitor-geometry (get-focused-monitor-geometry))
           (monitor-x (nth 0 monitor-geometry))
           (monitor-y (nth 1 monitor-geometry))
           ;; You may want to adjust the logic below if you have specific preferences
           ;; on where on the monitor the posframe should appear.
           ;; Currently, it places the posframe at its intended X and Y, but ensures
           ;; it's within the bounds of the focused monitor.
           (new-x (+ monitor-x x))
           (new-y (+ monitor-y y)))

      ;; Call the original function with potentially adjusted coordinates
      (funcall orig-fun frame new-x new-y width height buffer)))
  (advice-add 'corfu--make-frame :around #'advise-corfu-make-frame-with-monitor-awareness))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent nil))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :config
  ;; setup cape based on major mode
  (defun jds~setup-prog-mode-capf ()
    (make-variable-buffer-local 'completion-at-point-functions)
    (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    (add-to-list 'completion-at-point-functions #'cape-keyword))
  (add-hook 'emacs-lisp-mode-hook 'jds~setup-prog-mode-capf))

;;; keep comint mode out of completion -----------------------------------------

;; should use completions using M-j and M-k now
;; (general-define-key
;;  :keymaps 'comint-mode-map
;;  :states '(n i)
;;  "<up>" nil
;;  "<down>" nil
;;  "C-k" #'comint-previous-input
;;  "C-j" #'comint-next-input)

