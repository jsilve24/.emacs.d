;;; latex.el --- latex related config -*- lexical-binding: t; -*-

(use-package auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook (LaTeX-mode . reftex-mode)
  :hook (TeX-mode . visual-line-mode)   ; Enable word wrapping
  :hook (Tex-mode . outline-minor-mode) ; Enable folding
  :ensure t
  :diminish reftex-mode
  :config
  (setq TeX-parse-self t ;; parse on load
	TeX-auto-save t	 ;; parse on save
	TeX-auto-local ".auctex-auto"
	TeX-style-local ".auctex-style"
	;; automatically insert braces after sub/superscript in math mode
	TeX-electric-sub-and-superscript t
	;; just save, dont ask me before each compilation
	TeX-save-query nil)
  ;; Finally, if you often use \include or \input, you should make AUCTeX aware of the multi-file
  ;; document structure. You can do this by inserting
  ;; however doing this makes it promp for master every time (doom sets this to t)
  (setq-default TeX-master nil)

  ;; set-up chktex -- from doom
  ;; (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")
  (add-hook 'TeX-mode-hook (lambda ()
			     (setq ispell-parser 'tex
				   fill-nobreak-predicate (cons #'texmathp fill-nobreak-predicate)))))

(with-eval-after-load 'tex
  (setq TeX-source-correlate-mode t
	;; don't start the emacs server when correlating sources
	TeX-source-correlate-start-server nil
	TeX-source-correlate-method 'synctex
	;; support more electric pair braces, e.g., \{...\}
	LaTeX-electric-left-right-brace t)
  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-source-correlate-start-server t)
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; texmathp should detect align(*) environments
  (setq texmathp-tex-commands '(("align*" env-on) ("align" env-on)))
  (texmathp-compile))

;;; setup cdlatex

(use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :hook (org-mode . turn-on-org-cdlatex)
  ;; smartparens takes care of inserting closing delimiters, and if you
  ;; don't use smartparens you probably won't want these also.
  ;; also auctex takes care of inserting _ and ^
  ;; also auctex already provides `LaTeX-insert-item' so C-ret not needed
  :diminish cdlatex-mode
  :bind
  (:map cdlatex-mode-map
	;; ("$" . nil)
	("(" . nil)
	("{" . nil)
	("[" . nil)
	("|" . nil)
	("<" . nil)
	("_" . nil)
	("^" . nil)
	;; ("TAB" . cdlatex-tab)
	;; ([(control return)] . nil)
	)
  :config
  ;; I think there might be a bug in my config such that I need the following line: 
  (defalias 'cdlatex--texmathp 'texmathp)

  (setq cdlatex-math-symbol-alist
	'((?< ("\\leftarrow" "\\Leftarrow" "\\longleftarrow" "\\Longleftarrow"))
	  (?> ("\\rightarrow" "\\Rightarrow" "\\longrightarrow" "\\Longrightarrow"))
	  (?\\ ("\\parallel"))
	  (?| ("\\perp"))))
  (setq cdlatex-math-modify-alist '((?\p "\\proc" nil t nil nil)
				    ;; mathbb requires the amssymb library in latex
				    (?\B "\\mathbb" nil t nil nil)))
  ;; Keep cdlatex from taking backtick key (this functionality is now done by aas snippets)
  (define-key cdlatex-mode-map (kbd "`") nil)

  ;; also turn off backtick from org mode 
  ;; don't have cdlatex take over the backtick symbol (funcationality done by aas snippets now)
  (defun jds~org-cdlatex-hook-function ()
    (define-key org-cdlatex-mode-map (kbd "`") nil))
  (add-hook 'org-mode-hook 'jds~org-cdlatex-hook-function)


  ;;  give me back my ' key
  (defun jds/cdlatex-math-modify (&optional arg)
    "Just wraps cdlatex-math-modify and really makes sure its only active in texmathp"
    (interactive "P")
    (if (texmathp)
	(cdlatex-math-modify arg)
      (self-insert-command 1)))
  (define-key cdlatex-mode-map (kbd "'") 'jds/cdlatex-math-modify))

;;; setup latexmk
(use-package auctex-latexmk
  :ensure t
  :defer t
  :functions auctex-latexmk-setup
  :preface
  ;; solving temporary issue with tex-buf.el not being found
  ;; https://github.com/tom-tan/auctex-latexmk/issues/39
  (defun my-auctex-latexmk-advice (req feature &rest args)
    "Call REQ with FEATURE and ARGS, unless FEATURE is `tex-buf'."
    (unless (eq feature 'tex-buf)
      (apply req feature args)))
  :init
  (unwind-protect
      (progn (advice-add 'require :around #'my-auctex-latexmk-advice)
	     (auctex-latexmk-setup))
    (advice-remove 'require #'my-auctex-latexmk-advice))

  ;; Pass the -pdf flag when TeX-PDF-mode is active
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  ;; Set LatexMk as the default
  (add-hook 'LaTeX-mode
	    (lambda () (setq TeX-command-default "LatexMk")))
  (add-hook 'latex-mode
	    (lambda () (setq TeX-command-default "LatexMk"))))


;;; setup evil-tex

(use-package evil-tex
  :straight (evil-tex :type git :host github :repo "iyefrat/evil-tex"
		      :fork t)
  :hook (LaTeX-mode . evil-tex-mode)
  :hook (org-mode . evil-tex-mode)
  :init
  (setq evil-tex-toggle-override-m nil
	evil-tex-toggle-override-t nil)
  :config
  (define-key evil-tex-inner-text-objects-map "y" 'evil-tex-inner-superscript)
  (define-key evil-tex-outer-text-objects-map "y" 'evil-tex-a-superscript)
  (define-key evil-tex-inner-text-objects-map "u" 'evil-tex-inner-subscript)
  (define-key evil-tex-outer-text-objects-map "u" 'evil-tex-a-subscript)

  

  ;; place evil tex toggles on <localleader>z for relevant modes
  (jds/localleader-def
    :keymaps '(LaTeX-mode-map org-mode-map)
    "z" evil-tex-toggle-map))


;;; setup company-auctex and company-reftex (and perhaps company-math)

(use-package company-auctex
   :hook (LaTeX-mode . evil-tex-mode)
  :config
  (add-to-list 'company-backends
	     '(company-auctex-macros company-auctex-symbols company-auctex-environments))
  ;; (company-auctex-init)
  )

;; Nicely indent lines that have wrapped when visual line mode is activated
(use-package adaptive-wrap
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init (setq-default adaptive-wrap-extra-indent 0))




;;; setup reftex corfu interface
(use-package company-reftex)


(defun jds~setup-capf-latex ()
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
	       (cape-company-to-capf #'company-reftex-citations))
  (add-to-list 'completion-at-point-functions
	       (cape-company-to-capf #'company-reftex-labels)))

(add-hook 'LaTeX-mode-hook 'jds~setup-capf-latex)


;;;###autoload
(defun latex-word-count ()
  (interactive)
  (shell-command (concat "/sbin/texcount "
                         ; "uncomment then options go here "
                         (buffer-file-name))))

;;;###autoload
(defun jds/tex-command-run-all-and-save ()
  (interactive)
  (save-buffer)
  (call-interactively #'TeX-command-run-all))

;;; keybindings

(jds/localleader-def
  :keymaps 'LaTeX-mode-map
  "m" #'jds/tex-command-run-all-and-save
  "e" #'LaTeX-environment
  "s" #'LaTeX-section
  "C" #'LaTeX-close-environment
  "n" #'TeX-next-error
  "N" #'TeX-previous-error
  ;; "i" #'LaTeX-insert-item
  "t" #'reftex-toc)

(jds/localleader-def
  :keymaps '(LaTeX-mode-map)
  ;; "c" jds/citation-map ; reserved
  "l" #'reftex-label)

(jds/localleader-def
  :keymaps 'bibtex-mode-map
  "c" '(:ignore t)
  "cS" #'biblio-lookup)


;; some key bindings that should be in org and latex
(general-define-key
 :keymaps '(LaTeX-mode-map org-mode-map org-capture-mode-map)
 :states 'normal
 "[m" #'jds/latex-previous-math-start
 "]m" #'jds/latex-next-math-start)

(jds/localleader-def
 :keymaps '(LaTeX-mode-map org-mode-map org-capture-mode-map)
 :states 'normal
 "g" '(:ignore t :which-key "goto") 
 "gm" #'jds/avy-latex-math)

