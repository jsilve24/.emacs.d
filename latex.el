;;; latex.el --- latex related config -*- lexical-binding: t; -*-

(use-package auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook (LaTeX-mode . reftex-mode)
  :ensure t
  :config
  (setq TeX-parse-self t ;; parse on load
        TeX-auto-save t  ;; parse on save
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
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")
  (add-hook 'TeX-mode-hook (lambda ()
                             (setq ispell-parser 'tex
                                   fill-nobreak-predicate (cons #'texmathp fill-nobreak-predicate))))

  ;; Enable word wrapping
  (add-hook 'TeX-mode-hook #'visual-line-mode)

  ;; enable folding
  ;; (add-hook 'LaTeX-mode-hook #'outline-minor-mode) ;; was in my doom config
  (add-hook 'TeX-mode-hook #'outline-minor-mode))


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
  ;; :hook (org-mode . turn-on-cdlatex)
  :defer t
  ;; smartparens takes care of inserting closing delimiters, and if you
  ;; don't use smartparens you probably won't want these also.
  ;; also auctex takes care of inserting _ and ^
  ;; also auctex already provides `LaTeX-insert-item' so C-ret not needed
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


;;; setup tecosaurs thing...


;; (use-package laas
;;   :straight t
;;   :hook (LaTeX-mode . laas-mode)
;;   :config ; do whatever here
;;   (aas-set-snippets 'laas-mode
;;                     ;; set condition!
;;                     :cond #'texmathp ; expand only while in math
;;                     "supp" "\\supp"
;;                     "On" "O(n)"
;;                     "O1" "O(1)"
;;                     "Olog" "O(\\log n)"
;;                     "Olon" "O(n \\log n)"
;;                     ;; bind to functions!
;;                     "Sum" (lambda () (interactive)
;;                             (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
;;                     "Span" (lambda () (interactive)
;;                              (yas-expand-snippet "\\Span($1)$0"))
;;                     ;; add accent snippets
;;                     :cond #'laas-object-on-left-condition
;;                     "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))


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

  ;; redefine evil-tex-a-delim to fall back to jds~evil-a/inner-paren

  (define-key evil-tex-inner-text-objects-map "d" nil)
  (define-key evil-tex-outer-text-objects-map "d" nil)
  
  ;; (evil-define-text-object evil-tex-a-delim (count &optional beg end type)
  ;;   "Select a delimiter, e.g. (foo), \\left[bar\\right] or \\bigl\\=\\{baz\\bigr\\}."
  ;;   :extend-selection nil
  ;;   (if (texmathp)
  ;; 	(nbutlast (evil-tex--select-delim beg end type count) 2)
  ;;     (jds~evil-paren-range count beg end type t)))

  ;; (evil-define-text-object evil-tex-inner-delim (count &optional beg end type)
  ;;   "Select inner delimiter, e.g. (foo), \\left[bar\\right] or \\bigl\\=\\{baz\\bigr\\}."
  ;;   :extend-selection nil
  ;;   (if (texmathp)
  ;; 	(last (evil-tex--select-delim beg end type count) 2)
  ;;     (jds~evil-paren-range count beg end type nil)))

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

;;; setup adaptive-wrap for nicer look

;; Nicely indent lines that have wrapped when visual line mode is activated
(use-package adaptive-wrap
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init (setq-default adaptive-wrap-extra-indent 0))


;;; setup citar

(use-package bibtex-completion
  :straight t
  :defer t
  :config
  (setq bibtex-completion-bibliography '("~/Dropbox/org/roam/references/references.bib")
	;; this is used by org-roam-bibtex note actions
	bibtex-completion-library-path '("~/Dropbox/org/roam/references/articles/")))


;;; setup reftex corfu interface
(use-package company-reftex)


(defun jds~setup-capf-latex ()
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
	       (cape-company-to-capf #'company-reftex-citations))
  (add-to-list 'completion-at-point-functions
	       (cape-company-to-capf #'company-reftex-labels)))

(add-hook 'LaTeX-mode-hook 'jds~setup-capf-latex)


;; ;;; setup company-reftex
;; (use-package company-reftex
;; :hook (LaTeX-mode . evil-tex-mode)
;; :config
;; (add-to-list 'company-backends 'company-reftex-labels)
;; (add-to-list 'company-backends 'company-reftex-citations))

;;;###autoload
(defun latex-word-count ()
  (interactive)
  (shell-command (concat "/sbin/texcount "
                         ; "uncomment then options go here "
                         (buffer-file-name))))

;;; keybindings

(jds/localleader-def
  :keymaps 'LaTeX-mode-map
  "m" #'TeX-command-run-all
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

