;;; lsp.el ---  Language Server Protocol setup -*- lexical-binding: t; -*-


(use-package eglot
  :hook ((ess-r-mode . eglot-ensure)
	 ;; (latex-mode . eglot-ensure)
	 ;; (LaTeX-mode . eglot-ensure)
	 )
  :config
  (setq eldoc-echo-area-use-multiline-p 1)
  (setq eglot-ignored-server-capabilites nil)
  ;; (defun jds~latex-eglot-hook ()
  ;;   "Hook run on eglot start in latex-mode"
  ;;   (setq-local eglot-stay-out-of '("imenu")))
  ;; add hook at front (before eglot) and make it local
  ;; (add-hook 'LaTeX-mode-hook 'jds~latex-eglot-hook -100)
  ;; (add-hook 'latex-mode-hook 'jds~latex-eglot-hook -100)

  ;; use texlab instead of digestif
  ;; (add-to-list 'eglot-server-programs '((tex-mode context-mode texinfo-mode bibtex-mode) . ("texlab")))
  )

(use-package consult-eglot)


;;; lsp-mode rather than eglot 
(use-package lsp-mode
  :disabled t
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!

  :init
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

  ;; Optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))

  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  (ess-r-mode . lsp)
  :commands lsp)
