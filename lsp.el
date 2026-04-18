;;; lsp.el ---  Language Server Protocol setup -*- lexical-binding: t; -*-

;; Eglot is built-in to Emacs 30+. Keybindings are in bindings.el under SPC e.
;; Prerequisites:
;;   Python: install pyright (npm i -g pyright) or pylsp (pip install python-lsp-server)
;;   R / Rcpp: install.packages("languageserver") in R, plus clangd for mixed R/C++ projects
;;   LaTeX: install texlab (optional, uncomment below)

(use-package eglot
  :straight (:type built-in)
  ;; Hook both python-mode and python-ts-mode since treesit-auto may remap;
  ;; only one fires per buffer.
  :hook ((python-mode . eglot-ensure)
	 (python-ts-mode . eglot-ensure)
	 (LaTeX-mode . eglot-ensure))
  :config
  (setq eldoc-echo-area-use-multiline-p 1)

  ;; Python: pyright is auto-detected by eglot, no explicit entry needed.
  ;; If you prefer pylsp, uncomment:
  ;; (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pylsp")))

  ;; LaTeX: texlab (uncomment to enable; install texlab first)
  (add-to-list 'eglot-server-programs
               '((tex-mode context-mode texinfo-mode bibtex-mode LaTeX-mode latex-mode)
                 . ("texlab"))))

;; `consult-eglot-symbols` already queries all running Eglot servers for the
;; current project, so it remains the right command for project-wide symbol
;; lookup under SPC e e.
(use-package consult-eglot
  :after eglot
  :commands consult-eglot-symbols)

;; use-package eglot-rcpp
(let ((eglot-rcpp-dir (expand-file-name "eglot-rcpp" user-emacs-directory)))
  (when (file-directory-p eglot-rcpp-dir)
    (add-to-list 'load-path eglot-rcpp-dir)))
(unless (require 'eglot-rcpp nil t)
  (message "Rcpp Eglot support skipped: `eglot-rcpp' is not available"))
