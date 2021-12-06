;;; snippets.el --- setup yasnippets and such -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/snippets
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  setup yasnippets and such
;;
;;; Code:

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

  (defmacro jds~aas-setup-insert-math (mode)
    `(progn (aas-set-snippets ,mode
	      :cond #'(lambda () (not (texmathp)))
	      ";m" (jds~yas-lambda-expand "\\\\($1\\\\)")
	      ";M" (jds~yas-lambda-expand "\\\[$1\\\]"))
	    (aas-set-snippets ,mode
	      :cond #'texmathp
	      ";u" (jds~yas-lambda-expand "_\\{$1\\}")
	      ";U" (jds~yas-lambda-expand "^\\{$1\\}"))))

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

