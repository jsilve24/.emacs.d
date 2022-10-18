;;; lsp.el ---  Language Server Protocol setup -*- lexical-binding: t; -*-


(use-package eglot
  :disabled t
  :hook ((latex-mode . eglot-ensure)
	 (LaTeX-mode . eglot-ensure))
  :config
  (setq eldoc-echo-area-use-multiline-p 1)
  (setq eglot-ignored-server-capabilites nil)
  (defun jds~latex-eglot-hook ()
    "Hook run on eglot start in latex-mode"
    (setq-local eglot-stay-out-of '("imenu")))
  ;; add hook at front (before eglot) and make it local
  (add-hook 'LaTeX-mode-hook 'jds~latex-eglot-hook -100)
  (add-hook 'latex-mode-hook 'jds~latex-eglot-hook -100)

  ;; use texlab instead of digestif
  ;; (add-to-list 'eglot-server-programs '((tex-mode context-mode texinfo-mode bibtex-mode) . ("texlab")))
  )


(use-package consult-eglot)
