;;; snippets.el --- setup yasnippets and such -*- lexical-binding: t; -*-

;;; setup consult-yassnippet

(use-package yasnippet
  :straight t
  :ensure t
  :bind ("M-SPC" . yas-expand)
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"))
  (yas-global-mode 1))

;; setup auto-activating snippets

(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :hook (org-mode . aas-activate-for-major-mode)
  :config

  (defmacro jds~yas-lambda-expand (str)
    `'(lambda () (interactive)
	(yas-expand-snippet ,str)))

  (defun jds~string-just-one-space (str)
    (progn
      (just-one-space)
      (insert str)
      (just-one-space)))

  (defmacro jds~aas-setup-insert-math (mode)
    `(progn (aas-set-snippets ,mode
	      :cond #'(lambda () (not (texmathp)))
	      ";m" (jds~yas-lambda-expand "\\\\($1\\\\)")
	      ";M" (jds~yas-lambda-expand "\\\[$1\\\]"))
	    (aas-set-snippets ,mode
	      :cond #'texmathp
	      ";u"      (jds~yas-lambda-expand "_\\{$1\\}")
	      ";U"      (jds~yas-lambda-expand "^\\{$1\\}")
	      ";sdot"   (jds~yas-lambda-expand "_\\{\\cdot $1\\}")
	      ";aeq"   #'(lambda () (interactive) (jds~string-just-one-space "&="))
	      ";asim"  #'(lambda () (interactive) (jds~string-just-one-space "&\\sim"))
	      ";inv"    (jds~yas-lambda-expand "^\\{-1\\}")
	      ";inset"    (jds~yas-lambda-expand "\\in\\\\{$1\\\\}")
	      ";perp"   (jds~yas-lambda-expand "^\\{\\perp\\}")
	      ";para"   (jds~yas-lambda-expand "^\\{\\parallel\\}")
	      ";text"   (jds~yas-lambda-expand "\\text\\{$1\\}"))))
  (jds~aas-setup-insert-math 'org-mode)
  (jds~aas-setup-insert-math 'latex-mode)

  ;; (aas-set-snippets 'text-mode
  ;;   ;; expand unconditionally
  ;;   "o-" "ō"
  ;;   "i-" "ī"
  ;;   "a-" "ā"
  ;;   "u-" "ū"
  ;;   "e-" "ē")
  ;; (aas-set-snippets 'latex-mode
  ;;   ;; set condition!
  ;;   :cond #'texmathp ; expand only while in math
  ;;   "supp" "\\supp"
  ;;   "On" "O(n)"
  ;;   "O1" "O(1)"
  ;;   "Olog" "O(\\log n)"
  ;;   "Olon" "O(n \\log n)"
  ;;   ;; bind to functions!
  ;;   "//" (lambda () (interactive)
  ;;          (yas-expand-snippet "\\frac{$1}{$2}$0"))
  ;;   "Span" (lambda () (interactive)
  ;;            (yas-expand-snippet "\\Span($1)$0")))
  ;; disable snippets by redefining them with a nil expansion
  ;; (aas-set-snippets 'latex-mode
  ;;   "supp" nil)
  )



;; latex autoactivating snippets

(use-package laas
  :straight (laas :type git :host github :repo "jsilve24/LaTeX-auto-activating-snippets")
  :hook (LaTeX-mode . laas-mode)
  :hook (org-mode . laas-mode))



;;; abbreviations

(provide 'snippets)
;;; snippets.el ends here

