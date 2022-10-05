;;; lsp.el ---  Language Server Protocol setup -*- lexical-binding: t; -*-


(use-package eglot
  :hook ((latex-mode . eglot-ensure)
	 (LaTeX-mode . eglot-ensure))
  :config
  (setq eldoc-echo-area-use-multiline-p 1)
  (setq eglot-ignored-server-capabilites nil))


(use-package consult-eglot)
