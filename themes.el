;;; themes.el --- theme related config -*- lexical-binding: t; -*-


;;; Font stuff

(setq jds/default-font-size 95)
(setq jds/default-variable-font-size 95)
;; (setq jds/default-font-fixed-width "Hack")
(setq jds/default-font-fixed-width "JetBrainsMono")
;; (setq jds/default-font-fixed-width "DejaVuSansMono")
;; (setq jds/default-font-fixed-width "Iosevka")
;; (setq jds/default-font-fixed-width "Fira Code Retina") ; no italics


(set-face-attribute 'default nil :font jds/default-font-fixed-width
                    :height jds/default-font-size)
;; (set-face-attribute 'default nil :font "DejaVuSansMono"
;;                     :height jds/default-font-size)


;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font jds/default-font-fixed-width
                    :height jds/default-font-size)

;; Set the variable pitch face
;; (set-face-attribute 'variable-pitch nil :font "Cantarell"
;;                     :height jds/default-variable-font-size :weight 'regular)


;;; main themes

(use-package doom-themes
  ;; :init (load-theme 'doom-vibrant t)
  :config
  ;; (setq doom-themes-padded-modeline t)
  )


(use-package kaolin-themes
  :config
  ;; (load-theme 'kaolin-aurora t)
  ;; (kaolin-treemacs-theme)
  )

(use-package gruvbox-theme)

(use-package emacs
  :init
  (setq modus-themes-mode-line '(accented borderless)
	modus-themes-org-blocks 'tinted-background
	modus-themes-deuteranopia nil
	modus-themes-bold-constructs t
	modus-themes-italic-constructs t
	modus-themes-syntax '(yellow-comments green-strings alt-syntax)
	modus-themes-mixed-fonts t
	modus-themes-links '(neutral-underline)
	;; modus-themes-links nil
	;; modus-themes-box-buttons '(flat faint)
	modus-themes-box-buttons nil
	modus-themes-prompts nil
	modus-themes-fringe nil
	modus-themes-lang-checkers '(straight-underline)
	modus-themes-hl-line nil
	modus-themes-subtle-line-numbers t
	modus-themes-markup nil
	modus-themes-region '(no-extend)
	modus-themes-org-agenda '((scheduled . uniform))
	;; modus-themes-org-agenda '((header-block . (1.5 variable-pitch))
	;; 			  (header-date . (grayscale workaholic bold-today))
	;; 			  (event . (accented varied))
	;; 			  (scheduled . uniform)
	;; 			  (habit . traffic-light))
	modus-themes-scale-headings t
	modus-themes-headings
	'((1 . (background rainbow overline bold  1.2))
	  (2 . (rainbow overline bold 1.1))
	  (3 . (overline rainbow bold 1.0))
	  (4 . (rainbow bold))
	  (t . (bold)))
	)
  :config
  (load-theme 'modus-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

(use-package ef-themes
  :commands (ef-themes-toggle)
  :bind ("<f6>" . ef-themes-toggle)
  :config
  (setq ef-themes-mixed-fonts t
	ef-themes-variable-pitch-ui nil
	ef-themes-headings
	'((1 . (background rainbow overline bold 1.2))
	  (2 . (rainbow overline bold 1.1))
	  (3 . (overline rainbow bold 1.0))
	  (4 . (rainbow bold))
	  (t . (bold))))

  (mapc #'disable-theme custom-enabled-themes)

  ;; fix up hl-todo(defun my-ef-themes-hl-todo-faces ()
  "Configure `hl-todo-keyword-faces' with Ef themes colors.
The exact color values are taken from the active Ef theme."
  ;; (ef-themes-with-colors
  ;;   (setq hl-todo-keyword-faces
  ;; 	  `(("HOLD" . ,yellow)
  ;; 	    ("TODO" . ,red)
  ;; 	    ("NEXT" . ,blue)
  ;; 	    ("THEM" . ,magenta)
  ;; 	    ("PROG" . ,cyan-warmer)
  ;; 	    ("OKAY" . ,green-warmer)
  ;; 	    ("DONT" . ,yellow-warmer)
  ;; 	    ("FAIL" . ,red-warmer)
  ;; 	    ("BUG" . ,red-warmer)
  ;; 	    ("DONE" . ,green)
  ;; 	    ("NOTE" . ,blue-warmer)
  ;; 	    ("KLUDGE" . ,cyan)
  ;; 	    ("HACK" . ,cyan)
  ;; 	    ("TEMP" . ,red)
  ;; 	    ("FIXME" . ,red-warmer)
  ;; 	    ("XXX+" . ,red-warmer)
  ;; 	    ("REVIEW" . ,red)
  ;; 	    ("DEPRECATED" . ,yellow))))

  ;; (add-hook 'ef-themes-post-load-hook #'my-ef-themes-hl-todo-faces)
  )


;;; hl-line-mode ---------------------------------------------------------------

(use-package emacs
  :config
  (add-hook 'dired-mode-hook #'hl-line-mode)
  (add-hook 'org-agenda-mode-hook #'hl-line-mode)
  (add-hook 'ibuffer-mode-hook #'hl-line-mode))


;;; modline

(straight-use-package 'all-the-icons)
(use-package all-the-icons)

;; NOTE: The first time you load your configuration on a new machine, you’ll need to run `M-x
;; all-the-icons-install-fonts` so that mode line icons display correctly.
;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1)
;;   :custom ((doom-modeline-height 15))
;;   :config
;;   (doom-themes-visual-bell-config)
;;   ;; (setq doom-modeline-modal-icon nil)
;;   )

(use-package smart-mode-line
  :config
  ;; give a bit extra space to battery indicator 
  (setq	sml/battery-format " %p ")

  ;; setup 
  (sml/setup)

  ;; seem to need to toggle display-time-mode to make system load not show up
  ;; this is very hacky but it seems to work. 
  (display-time-mode 0)
  (setq display-time-default-load-average nil)
  (display-time-mode 1)

  ;; don't show line number in mode-line (already show line-numbers when needed with fringe)
  (line-number-mode 0))

(use-package diminish
  :defer t
  :config
  (mapc 'diminish
	'(yas-minor-mode
	  citar-embark-mode
	  citar-org-roam-mode
	  org-roam-bibtex-mode
	  evil-snipe-mode
	  evil-snipe-override-mode
	  evil-snipe-local-mode
	  undo-tree-mode
	  auto-revert-mode
	  dired-hide-dotfiles-mode
	  eldoc-mode
	  abbrev-mode
	  which-key-mode
	  lispyville-mode
	  evil-owl-mode
	  evil-collection-unimpaired-mode
	  desktop-environment-mode
	  consult-org-roam-mode
	  lispy-mode
	  synosaurus-mode
	  evil-goggles-mode
	  org-indent-mode
	  flyspell-mode
	  evil-org-mode
	  org-cdlatex-mode
	  org-indent-mode
	  cdlatex-mode
	  reftex-mode
	  visual-line-mode)))


;;; org-mode theme -------------------------------------------------------------

;; (with-eval-after-load 'org
;;   (custom-set-faces
;;    '(org-level-1 ((t (:inherit outline-1 :height 1.25))))
;;    '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
;;    '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
;;    '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
;;    '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
;;    '(org-agenda-structure ((t (:inherit outline-1 :height 1.10))))))

(provide 'themes)
;;; themes.el ends here
