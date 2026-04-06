;;; treesitter.el --- tree-sitter configuration -*- lexical-binding: t -*-

;;;; Built-in treesit via treesit-auto
;; Emacs 30+ has built-in treesit support. treesit-auto handles:
;;   1. Auto-installing grammar .so files (prompts on first use)
;;   2. Remapping major modes to -ts-mode variants (python-mode -> python-ts-mode, etc.)
;; Requires a C compiler (gcc/clang) for grammar compilation.
(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;;; External tree-sitter for modes without a built-in -ts-mode (e.g., R/ESS)
;; Dual-backend setup: built-in treesit handles -ts-mode buffers (Python, YAML, etc.);
;; external tree-sitter (elisp-tree-sitter) is still needed for ess-r-mode since
;; there is no mature r-ts-mode. evil-textobj-tree-sitter auto-dispatches between
;; the two backends based on whether the major mode name ends in "-ts-mode".
;; NOTE: Once a mature r-ts-mode exists, these two packages can be removed.
(use-package tree-sitter
  :diminish tree-sitter-mode
  :hook (ess-r-mode . tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (setq tree-sitter-debug-jump-buttons t
	tree-sitter-debug-highlight-jump-region t))
(use-package tree-sitter-langs)


;;;; Evil Integration

(defmacro jds~bind-evil-textobj-ts (key group-inner group-outer)
  "Thin wrapper around define-key and evil-textobj-tree-sitter-get-textobj to bind to inner and outer object maps.
Also binds under goto motions using evil-textobj-tree-sitter-goto-textobj."
  `(progn (define-key evil-inner-text-objects-map ,key (evil-textobj-tree-sitter-get-textobj ,group-inner))
	  (define-key evil-outer-text-objects-map ,key (evil-textobj-tree-sitter-get-textobj ,group-outer))
	  (define-key evil-normal-state-map (concat "]" ,key)
	    (lambda () (interactive)
	      (evil-textobj-tree-sitter-goto-textobj ,group-outer)))
	  (define-key evil-normal-state-map (concat "[" ,key)
	    (lambda () (interactive)
	      (evil-textobj-tree-sitter-goto-textobj ,group-outer t)))
	  (define-key evil-normal-state-map (concat "]" (upcase ,key))
	    (lambda () (interactive)
	      (evil-textobj-tree-sitter-goto-textobj ,group-outer nil t)))
	  (define-key evil-normal-state-map (concat "[" (upcase ,key))
	    (lambda () (interactive)
	      (evil-textobj-tree-sitter-goto-textobj ,group-outer t t)))))



(use-package evil-textobj-tree-sitter
  :straight (evil-textobj-tree-sitter :type git
				      :host github
				      :repo "meain/evil-textobj-tree-sitter"
				      :files (:defaults "queries" "treesit-queries"))
  :config

  ;; Patch: upstream treesit-queries/python/textobjects.scm is missing
  ;; call.inner/call.outer captures. This advice appends them at runtime.
  ;; Remove this if upstream adds call captures to treesit-queries/python/.
  (defun jds~evil-textobj-ts-add-python-call-query (orig-fn language)
    "Advice to append call captures for Python treesit queries."
    (let ((result (funcall orig-fn language)))
      (if (and (evil-textobj-tree-sitter--use-builtin-treesitter)
	       (string= language "python")
	       result)
	  (concat result "\n"
		  "(call) @call.outer\n"
		  "(call arguments: (argument_list . \"(\" (_)+ @call.inner \")\"))\n")
	result)))
  (advice-add 'evil-textobj-tree-sitter--get-query :around
	       #'jds~evil-textobj-ts-add-python-call-query)

  ;; Text objects: inner/outer via i/a prefix, goto via ]/[ prefix
  ;; e.g., "vif" = select inner function, "]f" = goto next function, "[F" = goto prev function end
  ;; These override "a"/"c" in evil text object maps (previously evil-inner-arg and
  ;; evilnc-inner-comment in bindings.el); tree-sitter versions are language-aware.
  (jds~bind-evil-textobj-ts "f" "function.inner" "function.outer")
  (jds~bind-evil-textobj-ts "c" "call.inner" "call.outer")
  (jds~bind-evil-textobj-ts "k" "conditional.inner" "conditional.outer")
  (jds~bind-evil-textobj-ts "l" "loop.inner" "loop.outer")
  (jds~bind-evil-textobj-ts "a" ("parameter.inner" "call.inner") ("parameter.outer" "call.outer"))
  (jds~bind-evil-textobj-ts "v" "comment.outer" "comment.outer")
)
