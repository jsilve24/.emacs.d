;;; lsp.el ---  Language Server Protocol setup -*- lexical-binding: t; -*-


(use-package eglot
  :hook (latex-mode . eglot-ensure))


(use-package consult-eglot)
