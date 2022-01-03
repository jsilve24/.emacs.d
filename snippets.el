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
  :hook (markdown-mode . aas-activate-for-major-mode)
  :hook (ess-r-mode . aas-activate-for-major-mode)
  :hook (inferior-ess-mode . aas-activate-for-major-mode)
  :hook (python-mode . aas-activate-for-major-mode)
  :hook (text-mode . aas-activate-for-major-mode)
  :hook (prog-mode . aas-activate-for-major-mode)
  :config

  (defmacro jds~yas-lambda-expand (str)
    `'(lambda () (interactive)
	(yas-expand-snippet ,str)))

  (defmacro jds~yas-lambda-expand-snippet-by-key (name)
    "Return function that programatically expand snippet by NAME"
    `(quote  (lambda () (interactive)
	       (yas-expand-snippet
		(yas-lookup-snippet ,name)))))

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
	      ";begin" (jds~yas-lambda-expand "\\begin\\{$1\\}\n$0\n\\end\\{$1\\}")
	      ";figure" (jds~yas-lambda-expand "\\begin\\{figure\\}[ht]
  \\\centering
  \\\includegraphics[${1:options}]\\{figures/${2:path.pdf}\\}
  \\\caption\\{\\\label\\{fig:${3:label}\\} $0\\}
\\end\\{figure\\}")
	      ";article" (jds~yas-lambda-expand-snippet-by-key "article-template"))
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
	      ";text" (jds~yas-lambda-expand "\\text\\{$1\\}")
	      ";cases" (jds~yas-lambda-expand "\\begin\\{cases\\}\n$0 \\\\\n\\end\\{cases\\}"))))
  (jds~aas-setup-insert-math 'org-mode)
  (jds~aas-setup-insert-math 'latex-mode)
  (jds~aas-setup-insert-math 'markdown-mode)


  ;; latex mode citations
  (aas-set-snippets 'latex-mode
    ";cite" 'citar-insert-citation)

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
    ";defun" (jds~yas-lambda-expand "(defun $1 ($2)\n\"$3\"${4:\n(interactive${5: \"$6\"})}\n$7)")
    ";defmacro" (jds~yas-lambda-expand "(defmacro $1 ($2)\n\"$3\"\n\`($4))")
    ";defvar" (jds~yas-lambda-expand "(defvar ${1:symbol} ${2:initvalue} \"${3:docstring}\")")
    ";defcustom" (jds~yas-lambda-expand "(defcustom ${1:symbol} ${2:standard} \"${3:docstring}\"${4: args})")
    ";setq" (jds~yas-lambda-expand "(setq $1 $2)")
    ";use" (jds~yas-lambda-expand "(use-package $1)")
    ";cond" (jds~yas-lambda-expand "(cond\n(${1:condition} ${2:body})$0)")
    ";hook" (jds~yas-lambda-expand "(add-hook '${1:name}-hook ${2:'${3:function}})$0")
    ";bound" (jds~yas-lambda-expand "(if (fboundp '$1)\n$0)")
    ";lambda" (jds~yas-lambda-expand "(lambda ($1) ${2:(interactive)} $0)") 
    ";let" (jds~yas-lambda-expand "(let${1:*} (${2:args})\n$0)")
    ";not" (jds~yas-lambda-expand "(not $0)")
    ";or" (jds~yas-lambda-expand "(org $0)")
    ";header" (jds~yas-lambda-expand-snippet-by-key "package-header"))

  (defmacro jds~aas-setup-ess (mode)
    `(aas-set-snippets ,mode
       ";;" #'r/insert-assign 
       ";m" #'r/insert-pipe))
  (jds~aas-setup-ess 'ess-r-mode)
  (jds~aas-setup-ess 'inferior-ess-mode)
  
  
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
  (jds~aas-setup-headings 'python-mode)

  (aas-set-snippets 'text-mode
    ";phone" "(310) 806-2315"
    ";home" "123 Autumn Circle\nPort Matilda, PA 16870")

  (aas-set-snippets 'org-msg-edit-mode
    :cond #'message--in-tocc-p
    "; " (lambda () (interactive) (insert ", ") (completion-at-point)))

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

