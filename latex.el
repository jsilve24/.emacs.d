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
        TeX-source-correlate-method 'synctex)
  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-source-correlate-start-server t)
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

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
 (setq cdlatex-math-modify-alist '((?\p "\\proc" nil t nil nil))))

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
  :hook (LaTeX-mode . evil-tex-mode)
  :hook (org-mode . evil-tex-mode))

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
  :defer t)


;; ;;; setup company-reftex
;; (use-package company-reftex
;;   :hook (LaTeX-mode . evil-tex-mode)
;;   :config
;;   (add-to-list 'company-backends 'company-reftex-labels)
;;   (add-to-list 'company-backends 'company-reftex-citations))

;;; keybindings

(jds/localleader-def
  :keymaps 'LaTeX-mode-map
  "\\" #'TeX-command-run-all
  "e" #'LaTeX-environment
  "s" #'LaTeX-section
  "C" #'LaTeX-close-environment
  "n" #'TeX-next-error
  "N" #'TeX-previous-error
  "i" #'LaTeX-insert-item
  "t" #'reftex-toc)

(if (not (boundp 'jds/citation-map))
    (defvar jds/citation-map nil)) 
(jds/localleader-def
  :keymaps '(LaTeX-mode-map)
  "c" jds/citation-map
  "l" #'reftex-label)

(jds/localleader-def
  :keymaps 'bibtex-mode-map
  "c" '(:ignore t)
  "cS" #'biblio-lookup)

