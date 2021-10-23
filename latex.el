;;; latex.el --- latex related config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/latex
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  latex related config
;;
;;; Code:

;;; setup auctex

(use-package auctex
  :straight t;;(auctex :type git :host github :repo "emacs-stright/auctex" :branch "master")
  :defer t
  :config
  (setq TeX-parse-self t ;; parse on load
        TeX-auto-save t  ;; parse on save
        TeX-auto-local ".auctex-auto"
        TeX-style-local ".auctex-style"
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        ;; don't start the emacs server when correlating sources
        TeX-source-correlate-start-server nil
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



;;; setup cdlatex

(use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :hook (org-mode . turn-on-cdlatex)
  :defer t
  ;; smartparens takes care of inserting closing delimiters, and if you
  ;; don't use smartparens you probably won't want these also.
  ;; also auctex takes care of inserting _ and ^
  ;; also auctex already provides `LaTeX-insert-item' so C-ret not needed
  :bind
  (:map cdlatex-mode-map
   ("$" . nil)
   ("(" . nil)
   ("{" . nil)
   ("[" . nil)
   ("|" . nil)
   ("<" . nil)
   ("_" . nil)
   ("^" . nil)
   ("TAB" . cdlatex-tab)
   ([(control return)] . nil))
 :config
 (setq cdlatex-math-symbol-alist
        '((?< ("\\leftarrow" "\\Leftarrow" "\\longleftarrow" "\\Longleftarrow"))
          (?> ("\\rightarrow" "\\Rightarrow" "\\longrightarrow" "\\Longrightarrow"))
          (?\\ ("\\parallel"))
          (?| ("\\perp")))))

;;; setup latexmk

(use-package auctex-latexmk
  :straight t
  :after cdlatex
  :init
  ;; Pass the -pdf flag when TeX-PDF-mode is active
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  ;; Add latexmk as a TeX target
  (auctex-latexmk-setup)
 ;; Set LatexMk as the default
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
  :straight t
  :after latex
  :hook (LaTeX-mode . evil-tex-mode))

;;; setup company-auctex and company-reftex (and perhaps company-math)

(use-package company-auctex
  :straight t
  :after cdlatex
  :config
  (company-auctex-init))

;;; setup adaptive-wrap for nicer look

;; Nicely indent lines that have wrapped when visual line mode is activated
(use-package adaptive-wrap
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init (setq-default adaptive-wrap-extra-indent 0))


;;; setup bibtex-actions

(use-package bibtex-completion
  :straight t
  :defer t)

(use-package bibtex-actions
  :straight t
  :defer t
  :config
  ;; watch for changes in bib files
  (bibtex-actions-filenotify-setup '(LaTeX-mode-hook org-mode-hook))

  ;; nice file icons
  (setq bibtex-actions-symbols
  `((file . (,(all-the-icons-icon-for-file "foo.pdf" :face 'all-the-icons-dred) .
            ,(all-the-icons-icon-for-file "foo.pdf" :face 'bibtex-actions-icon-dim)))
    (note . (,(all-the-icons-icon-for-file "foo.txt") .
            ,(all-the-icons-icon-for-file "foo.txt" :face 'bibtex-actions-icon-dim)))
    (link .
        (,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'all-the-icons-dpurple) .
        ,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'bibtex-actions-icon-dim)))))
;; Here we define a face to dim non 'active' icons, but preserve alignment
(defface bibtex-actions-icon-dim
    '((((background dark)) :foreground "#282c34")
     (((background light)) :foreground "#fafafa"))
     "Face for obscuring/dimming icons"
     :group 'all-the-icons-faces)
)


;;; setup biblio


;;; keybindings

(jds/localleader-def
 :keymap 'LaTeX-mode-map
 "r" '(:ignore :which-key "reftex")
 "rb" #'bibtex-actions-insert-citation
 "rR" #'bibtex-actions-refresh)

(provide 'latex)
;;; latex.el ends here
