;;; treesitter.el --- summary -*- lexical-binding: t -*-

(use-package tree-sitter
  :diminish tree-sitter-mode
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  ;; better debugging
  (setq tree-sitter-debug-jump-buttons t
	tree-sitter-debug-highlight-jump-region t))
(use-package tree-sitter-langs)


;;;; Evil Integration

;; (use-package evil-textobj-tree-sitter)

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
				      :files (:defaults "queries"))
  :config

  (jds~bind-evil-textobj-ts "f" "function.inner" "function.outer")
  (jds~bind-evil-textobj-ts "h" "call.inner" "call.outer")
  (jds~bind-evil-textobj-ts "v" "conditional.inner" "conditional.outer")
  (jds~bind-evil-textobj-ts "l" "loop.inner" "loop.outer")
  (jds~bind-evil-textobj-ts "a" ("parameter.inner" "call.inner") ("parameter.outer" "call.outer"))
  (jds~bind-evil-textobj-ts "c" "comment.outer" "comment.outer")
)
