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
  :hook (latex-mode . aas-activate-for-major-mode)
  :hook (org-mode . aas-activate-for-major-mode)
  :hook (emacs-lisp-mode . aas-activate-for-major-mode)
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
	      ";M" (jds~yas-lambda-expand "\\\[$1\\\]")
	      ";begin" (jds~yas-lambda-expand "\\begin\\{$1\\}\n$0\n\\end\\{$1\\}"))
	    (aas-set-snippets ,mode
	      :cond #'texmathp
	      ";u" (jds~yas-lambda-expand "_\\{$1\\}")
	      ";U" (jds~yas-lambda-expand "^\\{$1\\}")
	      ";sdot" (jds~yas-lambda-expand "_\\{\\cdot $1\\}")
	      ";aeq" #'(lambda () (interactive) (jds~string-just-one-space "&="))
	      ";asim" #'(lambda () (interactive) (jds~string-just-one-space "&\\sim"))
	      ";inv" (jds~yas-lambda-expand "^\\{-1\\}")
	      ";inset" (jds~yas-lambda-expand "\\in\\\\{$1\\\\}")
	      ";perp" (jds~yas-lambda-expand "^\\{\\perp\\}")
	      ";para" (jds~yas-lambda-expand "^\\{\\parallel\\}")
	      ";text" (jds~yas-lambda-expand "\\text\\{$1\\}"))))
  (jds~aas-setup-insert-math 'org-mode)
  (jds~aas-setup-insert-math 'latex-mode)

  ;; org mode links
  (defun jds~org-agenda-link ()
    "Quickly Insert Org Agenda Links using consult-org-agenda."
    (interactive)
    (save-excursion
      (let ((buf (current-buffer)))
	(consult-org-agenda)
	(let ((link (call-interactively
		     #'(lambda () (interactive) (org-store-link t)))))
	  (switch-to-buffer buf)
	  (insert link)))))

  (aas-set-snippets 'org-mode
    ";lo" #'jds~org-agenda-link)

  ;; elisp snippets
  (aas-set-snippets 'emacs-lisp-mode
    ";auto" ";;;###autoload"
    ";straight" (jds~yas-lambda-expand ":straight ($1 :type git :host github :repo \"$2\")")
    ";defun" (jds~yas-lambda-expand "(defun $1 ($2)\n\"$3\"\n$4)")
    ";defmacro" (jds~yas-lambda-expand "(defmacro $1 ($2)\n\"$3\"\n\`($4))")
    ";setq" (jds~yas-lambda-expand "(setq $1 $2)")
    ";use" (jds~yas-lambda-expand "(use-package $1)"))

  (defun jds~comment-rule (string)
    "Prompts for `STRING` and horizontal rule starting with comment char as heading."
    (interactive "sHeadline: ")
    (comment-normalize-vars)
    ;; make a blank line if not already on one
    (if (progn (beginning-of-line)
	       (looking-at-p "[[:space:]]*$"))
	(delete-region (point) (progn (skip-chars-forward " \t") (point)))
      (move-end-of-line 1)
      (open-line 1)
      (forward-line))
    (let ((nchar (if (string= major-mode "emacs-lisp-mode")
		     3
		   1)))
      (insert-char (string-to-char comment-start) nchar))
    (just-one-space)
    (insert string)
    (end-of-line)
    (just-one-space)
    (while (< (current-column) 80)
      (insert "-")))

  (defmacro jds~aas-setup-headings (mode)
    `(aas-set-snippets ,mode
       ";h " #'jds~comment-rule))
  (jds~aas-setup-headings 'latex-mode)
  (jds~aas-setup-headings 'prog-mode)

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

