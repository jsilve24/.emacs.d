;;; capf.el --- capf config -*- lexical-binding: t -*-

(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-history
			      corfu-quick))
  ;; :bind
  ;; (:map corfu-map
  ;; 	("TAB" . corfu-next)
  ;; 	([tab] . corfu-next)
  ;; 	("S-TAB" . corfu-previous)
  ;; 	([backtab] . corfu-previous))
  ;; Optional customizations
  :custom
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil)  ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil) ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
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

  ;; setup corfu quick
  (define-key corfu-map "\M-q" #'corfu-quick-complete)
  (define-key corfu-map "\M-Q" #'corfu-quick-insert))

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

(use-package corfu-doc
  :config
  (setq corfu-doc-auto t
	corfu-doc-delay 0.75)
  (add-hook 'corfu-mode-hook #'corfu-doc-mode)
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)) ;; corfu-previous

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
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

;;; tabnine and AI -------------------------------------------------------------

;; (use-package company-tabnine
;;   :config
;;   (defun jds~setup-capf-tabnine ()
;;     (make-local-variable 'completion-at-point-functions)
;;     (add-to-list 'completion-at-point-functions
;; 		 (cape-company-to-capf #'company-tabnine)))

;;   ;; installation details
;;   ;; Installing at /home/jds6696/.emacs.d/var/company/tabnine-binaries/4.4.143/x86_64-unknown-linux-gnu/TabNine. Downloading https://update.tabnine.com/bundles/4.4.143/x86_64-unknown-linux-gnu/TabNine.zip ...
;;   ;; Wrote /home/jds6696/.emacs.d/var/company/tabnine-binaries/4.4.143/x86_64-unknown-linux-gnu.zip
;;   ;; Archive:  /home/jds6696/.emacs.d/var/company/tabnine-binaries/4.4.143/x86_64-unknown-linux-gnu.zip
;;   ;;   inflating: /home/jds6696/.emacs.d/var/company/tabnine-binaries/4.4.143/x86_64-unknown-linux-gnu/TabNine
;;   ;;   inflating: /home/jds6696/.emacs.d/var/company/tabnine-binaries/4.4.143/x86_64-unknown-linux-gnu/TabNine-deep-cloud
;;   ;;   inflating: /home/jds6696/.emacs.d/var/company/tabnine-binaries/4.4.143/x86_64-unknown-linux-gnu/TabNine-deep-local
;;   ;;   inflating: /home/jds6696/.emacs.d/var/company/tabnine-binaries/4.4.143/x86_64-unknown-linux-gnu/WD-TabNine
;;   ;; TabNine installation complete.
;;   (company-tabnine-install-binary))


