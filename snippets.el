;;; snippets.el --- setup yasnippets and such -*- lexical-binding: t; -*-

(use-package yasnippet
  :ensure t
  :commands (yas-expand yas-expand-snippet)
  :diminish yas-minor-mode
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


  (defmacro jds~aas-insert-math (symbol)
    "Add aas snippet that expands to SYMBOL wrapped in math if
not in math mode or SYMBOL in math mode. Don't add backslash
escape."
    `(lambda () (interactive)
       (if (texmathp)
	   (yas-expand-snippet ,symbol)
	 (yas-expand-snippet (concat "\\\\(" ,symbol "\\\\)")))))

  (defun jds~expand-math-symbol-p ()
      "Predicate for when to expand math snippets. In non-mathmode, only expand if preceded by non alphanumeric character"
      (cond
       ((and (not (texmathp)) (looking-back "[a-zA-Z0-9]" 1)) nil)
       (t t)))

  (defmacro jds~aas-setup-insert-math (mode)
    `(progn (aas-set-snippets ,mode
	      :cond #'(lambda () (not (texmathp)))
	      ";m" (jds~yas-lambda-expand "\\\\($0\\\\)")
	      ";M" (jds~yas-lambda-expand "\\\[$0\\\]")
	      "mdim" (jds~yas-lambda-expand "\\\\($1 \\times $2\\\\) matrix $0")
	      "vdim" (jds~yas-lambda-expand "\\\\($1\\\\)-vector $0")
	      ";begin" (jds~yas-lambda-expand "\\begin\\{$1\\}\n$0\n\\end\\{$1\\}")
	      ";align" (jds~yas-lambda-expand "\\begin\\{align\\}\n$0\n\\end\\{align\\}")
	      ";Align" (jds~yas-lambda-expand "\\begin\\{align*\\}\n$0\n\\end\\{align*\\}")
	      ";cite" 'jds/citar-insert-cite-prioritize-local-bib
	      ";ref"  (jds~yas-lambda-expand "\\ref\\{$1\\}$0")
	      ";it" (jds~yas-lambda-expand "\\textit\\{$1\\}$0")
	      ";bf" (jds~yas-lambda-expand "\\textbf\\{$1\\}$0")
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
	      ";inv" (jds~yas-lambda-expand "^\\{-1\\}")
	      ";inset" (jds~yas-lambda-expand "\\in\\\\{$1\\\\}")
	      ";perp" (jds~yas-lambda-expand "^\\{\\perp\\}")
	      ";para" (jds~yas-lambda-expand "^\\{\\parallel\\}")
	      ";dag" (jds~yas-lambda-expand "^\\{\\dagger\\}")
	      ";ddag" (jds~yas-lambda-expand "^\\{\\ddagger\\}")
	      ";star" (jds~yas-lambda-expand "^\\{\*\\}")
	      ";text" (jds~yas-lambda-expand "\\text\\{$1\\}")
	      ";cases" (jds~yas-lambda-expand "\\begin\\{cases\\}\n$0 \n\\end\\{cases\\}")
	      ";approx" "\\approx"
	      "lg" "\\log"
	      ";ii" "_{ii}"
	      ";ij" "_{ij}"
	      ";jj" "_{jj}"
	      ";2" "^{2}"
	      ";3" "^{3}"
	      ";T" "^{T}"
	      ";tt" "_{t}"
	      "xx" "\\times"
	      ";times" "\\times"
	      ";gg" "\\gg"
	      ";ll" "\\ll"
	      ";ge" "\\geq"
	      ";le" "\\leq"
	      ";ne" "\\neq"
	      "..." "\\dots"
	      "v..." "\\vdots"
	      "c..." "\\cdots"
	      "d..." "\\ddots"
	      "cc" "\\subset"
	      "CC" "\\supset"
	      "ceq" "\\subseteq"
	      "CEQ" "\\supseteq"
	      "EE" #'(lambda () (interactive) (jds~string-just-one-space "="))
	      "AE" #'(lambda () (interactive) (jds~string-just-one-space "&="))
	      "AA" #'(lambda () (interactive) (jds~string-just-one-space "&"))
	      "inn" "\\in "
	      "nin" "\\notin"
	      "AS" #'(lambda () (interactive) (jds~string-just-one-space "&\\sim"))
	      "SS" #'(lambda () (interactive) (jds~string-just-one-space "\\sim"))
	      "NN" #'(lambda () (interactive) (progn (jds~string-just-one-space "\\\\") (newline))))
	    (aas-set-snippets ,mode
	      :cond #'jds~expand-math-symbol-p
	      ";ee" (jds~aas-insert-math "\\mathbb\\{E\\}$0")
	      ";rr" (jds~aas-insert-math "\\mathbb\\{R\\}$0")
	      ";inf" (jds~aas-insert-math "\\inf$0")
	      ";sup" (jds~aas-insert-math "\\sup$0")
	      ";frac" (jds~aas-insert-math "\\frac\\{$1\\}\\{$2\\}$0")
	      "//" (jds~aas-insert-math "\\frac\\{$1\\}\\{$2\\}$0")
	      ";sum" (jds~aas-insert-math  "\\sum$0")
	      ";;sum" (jds~aas-insert-math  "\\sum_{$1}${2:^{$3\\}}$0")
	      ";int" (jds~aas-insert-math  "\\int$0")
	      ";;int" (jds~aas-insert-math  "\\int${1:_{$2\\}}${3:^{$4\\}}$0")
	      ";prod" (jds~aas-insert-math  "\\prod${1:_{$2\\}}${3:^{$4\\}}$0")
	      ";set" (jds~aas-insert-math "\\\\{$0\\\\}")
	      ";pc" (jds~aas-insert-math "p($1 \\mid $2)")
	      ";norm" (jds~aas-insert-math "N($1,$2)")
	      ";cov" (jds~aas-insert-math "\\text{Cov}($0)")
	      ";var" (jds~aas-insert-math "\\text{Var}($0)")
	      ";angle" (jds~aas-insert-math "\\langle $0 \\rangle")
	      ";reset" (jds~aas-insert-math "(-\\infty, \\infty)")
	      ";bmat" (jds~aas-insert-math "\\begin\\{bmatrix\\} $0 \\end\\{bmatrix\\}")
	      ";lab"  (jds~aas-insert-math "\\label\\{$1\\}$0")
	      ;; ";real" (jds~aas-insert-math "\\mathbb\\{R\\}$0")
	      ";simplex" (jds~aas-insert-math "\\mathbb\\{S\\}$0")
	      ;; ";natural" (jds~aas-insert-math "\\mathbb\\{N\\}$0")
	      ";choose" (jds~aas-insert-math "\\{$1 \\choose $2\\}$0")
	      ";all" (jds~aas-insert-math "\\forall$0")
	      ".a" (jds~aas-insert-math "\\alpha$0")
	      ".b" (jds~aas-insert-math "\\beta$0")
	      ".B" (jds~aas-insert-math "\\Beta$0")
	      ".c" (jds~aas-insert-math "\\chi$0")
	      ".d" (jds~aas-insert-math "\\delta$0")
	      ".D" (jds~aas-insert-math "\\Delta$0")
	      "..d" (jds~aas-insert-math "\\partial$0")
	      "..D" (jds~aas-insert-math "\\nabla$0")
	      ".e" (jds~aas-insert-math "\\epsilon$0")
	      ".E" (jds~aas-insert-math "\\Epsilon$0")
	      "..e" (jds~aas-insert-math "\\varepsilon$0")
	      ".f" (jds~aas-insert-math "\\phi$0")
	      "..f" (jds~aas-insert-math "\\varphi$0")
	      ".F" (jds~aas-insert-math "\\Phi$0")
	      ".g" (jds~aas-insert-math "\\gamma$0")
	      ".G" (jds~aas-insert-math "\\Gamma$0")
	      ".h" (jds~aas-insert-math "\\eta$0")
	      ".H" (jds~aas-insert-math "\\Eta$0")
	      ".k" (jds~aas-insert-math "\\kappa$0")
	      "..k" (jds~aas-insert-math "\\varkappa$0")
	      ".l" (jds~aas-insert-math "\\lambda$0")
	      "..l" (jds~aas-insert-math "\\ell$0")
	      ".L" (jds~aas-insert-math "\\Lambda$0")
	      ".m" (jds~aas-insert-math "\\mu$0")
	      ".n" (jds~aas-insert-math "\\nu$0")
	      ".o" (jds~aas-insert-math "\\omega$0")
	      ".O" (jds~aas-insert-math "\\Omega$0")
	      "..o" (jds~aas-insert-math "\\circ$0")
	      ".p" (jds~aas-insert-math "\\pi$0")
	      "..p" (jds~aas-insert-math "\\varpi$0")
	      ".P" (jds~aas-insert-math "\\Pi$0")
	      ".q" (jds~aas-insert-math "\\theta$0")
	      "..q" (jds~aas-insert-math "\\vartheta$0")
	      ".Q" (jds~aas-insert-math "\\Theta$0")
	      ".s" (jds~aas-insert-math "\\sigma$0")
	      "..s" (jds~aas-insert-math "\\varsigma$0")
	      ".S" (jds~aas-insert-math "\\Sigma$0")
	      ".r" (jds~aas-insert-math "\\rho$0")
	      ".t" (jds~aas-insert-math "\\tau$0")
	      ".u" (jds~aas-insert-math "\\upsilon$0")
	      ".U" (jds~aas-insert-math "\\Upsilon$0")
	      ".v" "\\mid "
	      ".x" (jds~aas-insert-math "\\xi$0")
	      ".X" (jds~aas-insert-math "\\Xi$0")
	      ".y" (jds~aas-insert-math "\\psi$0")
	      ".Y" (jds~aas-insert-math "\\Psi$0")
	      ".z" (jds~aas-insert-math "\\zeta$0")
	      ".;" (jds~aas-insert-math "\\cdot$0")
	      ;; ".(" (jds~aas-insert-math "\\cup$0")
	      ;; ".)" (jds~aas-insert-math "\\cap$0")
	      ;; "..(" (jds~aas-insert-math "\\bigcup$0")
	      ;; "..)" (jds~aas-insert-math "\\bigcap$0")
	      ".>" (jds~aas-insert-math "\\rightarrow$0")
	      ".<" (jds~aas-insert-math "\\leftarrow$0")
	      ".<" (jds~aas-insert-math "\\leftarrow$0")
	      ".=" (jds~aas-insert-math "\\leftrightarrow$0")
	      "..>" (jds~aas-insert-math "\\Rightarrow$0")
	      "..<" (jds~aas-insert-math "\\Leftarrow$0")
	      "..<" (jds~aas-insert-math "\\Leftarrow$0")
	      "..=" (jds~aas-insert-math "\\Leftrightarrow$0")
	      ".0" (jds~aas-insert-math "\\varnothing$0")
	      ".^" (jds~aas-insert-math "\\uparrow$0")
	      "._" (jds~aas-insert-math "\\downarrow$0")
	      ".8" (jds~aas-insert-math "\\infty$0")
	      ".*" (jds~aas-insert-math "\\odot$0")
	      ".#" (jds~aas-insert-math "\\otimes$0")
	      "./" (jds~aas-insert-math "\\oslash$0")
	      ".+" (jds~aas-insert-math "\\oplus$0")
	      ".-" (jds~aas-insert-math "\\ominus$0"))))
  (jds~aas-setup-insert-math 'org-mode)
  (jds~aas-setup-insert-math 'latex-mode)
  (jds~aas-setup-insert-math 'markdown-mode)



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
    ";lr" #'org-roam-node-insert
    ";sq" (jds~yas-lambda-expand "#+BEGIN_QUOTE\n$0\n#+END_QUOTE")
    ";sr" (jds~yas-lambda-expand "#+begin_src R :exports ${1:$$(yas-choose-value '(\"both\" \"code\" \"results\" \"none\"))} :session \"*R*\" :results output \n$0\n#+end_src")
    ";sb" (jds~yas-lambda-expand "#+begin_src bibtex\n$0\n#+end_src")
    ;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-stan.html
    ";sstan" (jds~yas-lambda-expand "#+name: ${1:model-stan}\n#+begin_src stan :file ${2:model.stan}\n$0\n#+end_src\n\n #+RESULTS: $1")
    ";scode" (jds~yas-lambda-expand "#+begin_src ${1:R} :eval none\n $0\n#+end_src")
    ";name" (jds~yas-lambda-expand "#+name: $0")
    ";results" (jds~yas-lambda-expand "#+RESULTS: $0")
    ";center" (jds~yas-lambda-expand "#+begin_center\n$0\n#+end_center")
    ";header" (jds~yas-lambda-expand-snippet-by-key "org-header-for-export")
    ";beamer" (jds~yas-lambda-expand-snippet-by-key "org-beamer-template")
    ";cols"   (jds~yas-lambda-expand-snippet-by-key "org-beamer-cols")
    ";pdf" (jds~yas-lambda-expand-snippet-by-key "org-beamer-include-pdf")
    ";rplot" (jds~yas-lambda-expand-snippet-by-key "org-beamer-r-plot")
    ";ir" (jds~yas-lambda-expand "src_R[:session \"*R*\"]{$1}$0")
    ";width" (jds~yas-lambda-expand "#+ATTR_LATEX: :width ${1:0.9}\\linewidth$0")
    ";latex" (jds~yas-lambda-expand "#+LATEX: $0")
    ";atex" (jds~yas-lambda-expand "#+ATTR_LATEX: $0")
    ";alatex" (jds~yas-lambda-expand "#+ATTR_LATEX: $0")
    ";caption" (jds~yas-lambda-expand "#+CAPTION:$0")
    ";ofig" (jds~yas-lambda-expand "[[./$0]]")
    ";pause" "#+BEAMER: \\pause"
    ";fill" "#+BEAMER: \\vfill"
    ";overlay" (jds~yas-lambda-expand "#+ATTR_BEAMER: :overlay <${1:+-}>"))


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
    ";or" (jds~yas-lambda-expand "(or $0)")
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
		   3)))
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
    "; " (lambda () (interactive) (insert ", ") (completion-at-point))))

;; latex autoactivating snippets

;; (use-package laas
;;   :straight (laas :type git :host github :repo "jsilve24/LaTeX-auto-activating-snippets")
;;   :hook (LaTeX-mode . laas-mode)
;;   :hook (org-mode . laas-mode))



;;; abbreviations

(provide 'snippets)
;;; snippets.el ends here

