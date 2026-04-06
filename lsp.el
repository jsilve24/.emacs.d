;;; lsp.el ---  Language Server Protocol setup -*- lexical-binding: t; -*-

;; Eglot is built-in to Emacs 30+. Keybindings are in bindings.el under SPC e.
;; Prerequisites:
;;   Python: install pyright (npm i -g pyright) or pylsp (pip install python-lsp-server)
;;   R: install.packages("languageserver") in R
;;   LaTeX: install texlab (optional, uncomment below)
(use-package eglot
  :straight (:type built-in)
  ;; Hook both python-mode and python-ts-mode since treesit-auto may remap;
  ;; only one fires per buffer.
  :hook ((python-mode . eglot-ensure)
	 (python-ts-mode . eglot-ensure)
	 (ess-r-mode . eglot-ensure))
  :config
  (setq eldoc-echo-area-use-multiline-p 1)

  ;; R language server (requires R package: install.packages("languageserver"))
  (add-to-list 'eglot-server-programs
	       '(ess-r-mode . ("R" "--slave" "-e" "languageserver::run()")))

  ;; Python: pyright is auto-detected by eglot, no explicit entry needed.
  ;; If you prefer pylsp, uncomment:
  ;; (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pylsp")))

  ;; LaTeX: texlab (uncomment to enable; install texlab first)
  ;; (add-to-list 'eglot-server-programs
  ;;              '((tex-mode context-mode texinfo-mode bibtex-mode LaTeX-mode latex-mode)
  ;;                . ("texlab")))
  )

;; Provides consult-eglot-symbols, bound to SPC e e in bindings.el
(use-package consult-eglot
  :after eglot
  :commands consult-eglot-symbols)
