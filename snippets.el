;;; snippets.el --- setup yasnippets and such -*- lexical-binding: t; -*-

;;; setup consult-yassnippet

(use-package yasnippet
  :ensure t
  :commands (yas-expand yas-expand-snippet)
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
    `'(lambda () (interactive)
	(yas-expand-snippet
	 (yas-lookup-snippet ,name))))

  (defun jds~string-just-one-space (str)
    (progn
      (just-one-space)
      (insert str)
      (just-one-space)))


  (defmacro jds~aas-insert-math-symbol (symbol)
    "Add aas snippet that expands to SYMBOL wrapped in math if
not in math mode or SYMBOL in math mode. Don't add backslash
escape."
    `(lambda () (interactive)
       (if (texmathp)
	   (yas-expand-snippet (concat "\\" ,symbol))
	 (yas-expand-snippet (concat "\\\\(\\" ,symbol "$0\\\\)")))))

  (defmacro jds~aas-setup-insert-math (mode)
    `(progn (aas-set-snippets ,mode
	      :cond #'(lambda () (not (texmathp)))
	      ";m" (jds~yas-lambda-expand "\\\\( $1 \\\\)")
	      ";M" (jds~yas-lambda-expand "\\\[ $1 \\\]")
	      ";begin" (jds~yas-lambda-expand "\\begin\\{$1\\}\n$0\n\\end\\{$1\\}")
	      ";align" (jds~yas-lambda-expand "\\begin\\{align\\}\n$0\n\\end\\{align\\}")
	      ";cite" (jds~yas-lambda-expand  "\\cite\\{$0\\}")
	      ";fig" (jds~yas-lambda-expand "\\begin\\{figure\\}[ht]
  \\\centering
  \\\includegraphics[${1:options}]\\{figures/${2:path.pdf}\\}
  \\\caption\\{ $0\\}
\\\label\\{fig:${3:label}\\}
\\end\\{figure\\}")
	      ";article" (jds~yas-lambda-expand-snippet-by-key "article-template"))
	    (aas-set-snippets ,mode
	      :cond #'texmathp
	      "'" #'(lambda () (interactive) (cdlatex-math-modify nil))
	      ";u" (jds~yas-lambda-expand "_\\{$1\\}")
	      ";U" (jds~yas-lambda-expand "^\\{$1\\}")
	      ";y" (jds~yas-lambda-expand "^\\{$1\\}")
	      ";sdot" (jds~yas-lambda-expand "_\\{\\cdot $1\\}")
	      ";inv" (jds~yas-lambda-expand "^\\{-1\\}")
	      ";inset" (jds~yas-lambda-expand "\\in\\\\{$1\\\\}")
	      ";perp" (jds~yas-lambda-expand "^\\{\\perp\\}")
	      ";para" (jds~yas-lambda-expand "^\\{\\parallel\\}")
	      ";dag" (jds~yas-lambda-expand "^\\{\\dagger\\}")
	      ";star" (jds~yas-lambda-expand "^\\{\*\\}")
	      ";text" (jds~yas-lambda-expand "\\text\\{$1\\}")
	      ";cases" (jds~yas-lambda-expand "\\begin\\{cases\\}\n$0 \n\\end\\{cases\\}")
	      ";frac" (jds~yas-lambda-expand "\\frac\\{$1\\}\\{$2\\}")
	      ";all" "\\forall"
	      ";set" (jds~yas-lambda-expand "\\\\{$0\\\\}")
	      ";sim" (jds~yas-lambda-expand "\\sim")
	      ";pp" (jds~yas-lambda-expand "p($0)")
	      ";pr" (jds~yas-lambda-expand "P($0)")
	      ";pc" (jds~yas-lambda-expand "p($1 \\vert $2)")
	      ";approx" "\\approx"
	      ";norm" (jds~yas-lambda-expand "N($1,$2)")
	      ";cov" (jds~yas-lambda-expand "\\text{Cov}($0)")
	      ";var" (jds~yas-lambda-expand "\\text{Var}($0)")
	      ";ii" "_{ii}"
	      ";ij" "_{ij}"
	      ";jj" "_{jj}"
	      ";;2" "^{2}"
	      ";;3" "^{3}"
	      ";;T" "^{T}"
	      ";tt" "_{t}"
	      ";xx" "\\times"
	      ">>" "\\gg"
	      "<<" "\\ll"
	      ">=" "\\geq"
	      "<=" "\\leq"
	      "!=" "\\neq"
	      ";neq" "\\neq"
	      ";leq" "\\leq"
	      ";geq" "\\geq"
	      "..." "\\dots"
	      "cc" "\\subset"
	      "ceq" "\\subseteq"
	      "EE" #'(lambda () (interactive) (jds~string-just-one-space "="))
	      "AE" #'(lambda () (interactive) (jds~string-just-one-space "&="))
	      "AA" #'(lambda () (interactive) (jds~string-just-one-space "&"))
	      "inn" "\\in"
	      "AS" #'(lambda () (interactive) (jds~string-just-one-space "&\\sim"))
	      "SS" #'(lambda () (interactive) (jds~string-just-one-space "\\sim"))
	      "//" (jds~yas-lambda-expand "\\frac\\{$1\\}\\{$2\\}"))
	    (aas-set-snippets ,mode
	      ";;a" (jds~aas-insert-math-symbol "alpha")
	      ";;b" (jds~aas-insert-math-symbol "beta")
	      ";;B" (jds~aas-insert-math-symbol "Beta")
	      ";;c" (jds~aas-insert-math-symbol "chi")
	      ";;d" (jds~aas-insert-math-symbol "delta")
	      ";;D" (jds~aas-insert-math-symbol "Delta")
	      ";;e" (jds~aas-insert-math-symbol "epsilon")
	      ";;E" (jds~aas-insert-math-symbol "Epsilon")
	      ";;f" (jds~aas-insert-math-symbol "phi")
	      ";;F" (jds~aas-insert-math-symbol "Phi")
	      ";;g" (jds~aas-insert-math-symbol "gamma")
	      ";;G" (jds~aas-insert-math-symbol "Gamma")
	      ";;h" (jds~aas-insert-math-symbol "eta")
	      ";;H" (jds~aas-insert-math-symbol "Eta")
	      ";;k" (jds~aas-insert-math-symbol "kappa")
	      ";;l" (jds~aas-insert-math-symbol "lambda")
	      ";;L" (jds~aas-insert-math-symbol "Lambda")
	      ";;m" (jds~aas-insert-math-symbol "mu")
	      ";;n" (jds~aas-insert-math-symbol "nu")
	      ";;o" (jds~aas-insert-math-symbol "omega")
	      ";;O" (jds~aas-insert-math-symbol "Omega")
	      ";;p" (jds~aas-insert-math-symbol "pi")
	      ";;P" (jds~aas-insert-math-symbol "Pi")
	      ";;q" (jds~aas-insert-math-symbol "theta")
	      ";;Q" (jds~aas-insert-math-symbol "Theta")
	      ";;s" (jds~aas-insert-math-symbol "sigma")
	      ";;S" (jds~aas-insert-math-symbol "Sigma")
	      ";;r" (jds~aas-insert-math-symbol "rho")
	      ";;t" (jds~aas-insert-math-symbol "tau")
	      ";;u" (jds~aas-insert-math-symbol "upsilon")
	      ";;U" (jds~aas-insert-math-symbol "Upsilon")
	      ";;v" "\\vert"
	      ";;x" (jds~aas-insert-math-symbol "xi")
	      ";;X" (jds~aas-insert-math-symbol "Xi")
	      ";;y" (jds~aas-insert-math-symbol "psi")
	      ";;Y" (jds~aas-insert-math-symbol "Psi")
	      ";;z" (jds~aas-insert-math-symbol "zeta")
	      ";;." (jds~aas-insert-math-symbol "cdot")
	      ";;>" (jds~aas-insert-math-symbol "rightarrow")
	      ";;<" (jds~aas-insert-math-symbol "leftarrow")
	      ";;[" (jds~aas-insert-math-symbol "subset")
	      ";;]" (jds~aas-insert-math-symbol "supset")
	      ";;=" (jds~aas-insert-math-symbol "leftrightarrow")
	      ";;0" (jds~aas-insert-math-symbol "emptyset")
	      ";;^" (jds~aas-insert-math-symbol "uparrow")
	      ";;_" (jds~aas-insert-math-symbol "downarrow")
	      ";;8" (jds~aas-insert-math-symbol "infty")
	      ";;+" (jds~aas-insert-math-symbol "oplus")
	      ";;-" (jds~aas-insert-math-symbol "ominus"))))
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
    ";lo" #'jds~org-agenda-link
    ";sq" (jds~yas-lambda-expand "#+BEGIN_QUOTE\n$0\n#+END_QUOTE")
    ";sr" (jds~yas-lambda-expand "#+begin_src R :exports ${1:$$(yas-choose-value '(\"both\" \"code\" \"results\" \"none\"))} :session \"*R*\" \n$0\n#+end_src")
    ";sb" (jds~yas-lambda-expand "#+begin_src bibtex\n$0\n#+end_src")
    ;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-stan.html
    ";sstan" (jds~yas-lambda-expand "#+name: ${1:model-stan}\n#+begin_src stan :file ${2:model.stan}\n$0\n#+end_src\n\n #+RESULTS: $1")
    ";scode" (jds~yas-lambda-expand "#+begin_src ${1:R} :eval none\n $0\n#+end_src")
    ";name" (jds~yas-lambda-expand "#+name: $0")
    ";results" (jds~yas-lambda-expand "#+RESULTS: $0")
    ";center" (jds~yas-lambda-expand "#+begin_center\n$0\n#+end_center")
    ";header" (jds~yas-lambda-expand-snippet-by-key "org-header-for-export")
    ";beamer" (jds~yas-lambda-expand-snippet-by-key "org-beamer-template")
    ";pdf" (jds~yas-lambda-expand-snippet-by-key "org-beamer-include-pdf")
    ";plot" (jds~yas-lambda-expand-snippet-by-key "org-beamer-r-plot")
    ";width" (jds~yas-lambda-expand "#+ATTR_LATEX: :width ${1:0.9\\linewidth}")
    ";alatex" (jds~yas-lambda-expand "#+ATTR_LATEX: $0")
    ";pause" "#+BEAMER: \\pause"
    ";overlay" "#+ATTR_BEAMER: :overlay <+->")


  ;; elisp snippets
  (aas-set-snippets 'emacs-lisp-mode
    ";auto" ";;;###autoload"
    ";straight" (jds~yas-lambda-expand ":straight ($1 :type git :host github :repo \"$2\")")
    ";defun" (jds~yas-lambda-expand "(defun $1 ($2)\n\"$3\"${4:\n(interactive${5: \"$6\"})}\n$7)")
    ";func" (jds~yas-lambda-expand "(defun $1 ($2)\n\"$3\"${4:\n(interactive${5: \"$6\"})}\n$7)")
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
       ";in" "%in%"
       ";*"  "%*%"
       ";m" #'r/insert-pipe
       ";func" (jds~yas-lambda-expand "$1 <- function($2) {\n $0 \n}")
       ";for" (jds~yas-lambda-expand "for (${1:i} in ${2:1:$3}) {\n$0\n}")))
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
  ;; ;; expand unconditionally
  ;; "o-" "ō"
  ;; "i-" "ī"
  ;; "a-" "ā"
  ;; "u-" "ū"
  ;; "e-" "ē")
  ;; (aas-set-snippets 'latex-mode
  ;; ;; set condition!
  ;; :cond #'texmathp ; expand only while in math
  ;; "supp" "\\supp"
  ;; "On" "O(n)"
  ;; "O1" "O(1)"
  ;; "Olog" "O(\\log n)"
  ;; "Olon" "O(n \\log n)"
  ;; ;; bind to functions!
  ;; "//" (lambda () (interactive)
  ;; (yas-expand-snippet "\\frac{$1}{$2}$0"))
  ;; "Span" (lambda () (interactive)
  ;; (yas-expand-snippet "\\Span($1)$0")))
  ;; disable snippets by redefining them with a nil expansion
  ;; (aas-set-snippets 'latex-mode
  ;; "supp" nil)
  )




;; latex autoactivating snippets

;; (use-package laas
;;   :straight (laas :type git :host github :repo "jsilve24/LaTeX-auto-activating-snippets")
;;   :hook (LaTeX-mode . laas-mode)
;;   :hook (org-mode . laas-mode))



;;; abbreviations

(provide 'snippets)
;;; snippets.el ends here

