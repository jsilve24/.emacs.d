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

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font jds/default-font-fixed-width
                    :height jds/default-font-size)

;; Set the variable pitch face
;; (set-face-attribute 'variable-pitch nil :font "Cantarell"
;;                     :height jds/default-variable-font-size :weight 'regular)


;;; main themes

(use-package doom-themes)

(use-package gruvbox-theme
  :disabled)

(use-package modus-themes
  :ensure nil
  :demand t
  :config
  (setq modus-themes-bold-constructs t
	modus-themes-italic-constructs t
	;; Make the fringe invisible
	modus-themes-common-palette-overrides '((fringe unspecified)))
;; Like the above, but use a shade of red for the current line number
(setq modus-themes-common-palette-overrides
      '((fg-line-number-inactive unspecified)
        (fg-line-number-active red-cooler)
        (bg-line-number-inactive unspecified)
        (bg-line-number-active unspecified)))

;; Make code blocks (in Org, for example) use a more colorful style
;; for their delimiter lines as well as their contents.  Give this a
;; purple feel.  Make the delimiter lines distinct from the contents.
(setq modus-themes-common-palette-overrides
      '((bg-prose-block-contents bg-magenta-nuanced)
        (bg-prose-block-delimiter bg-lavender)
        (fg-prose-block-delimiter fg-main)))

(setq modus-themes-headings
 '((1 . (background rainbow overline bold 1.2))
   (2 . (rainbow overline bold 1.1))
   (3 . (overline rainbow bold 1.0))
   (4 . (rainbow bold))
   (t . (bold))))

  ;; ;; Remove the border
  ;; (setq modus-themes-common-palette-overrides
  ;; 	'((border-mode-line-active unspecified)
  ;; 	  (border-mode-line-inactive unspecified)))
  ;; Keep the border but make it the same color as the background of the
  ;; mode line (thus appearing borderless).  The difference with the
  ;; above is that this version is a bit thicker because the border are
  ;; still there.
  (setq modus-themes-common-palette-overrides
	'((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)))

  (load-theme 'modus-vivendi t)
  :bind ("<f5>" . modus-themes-toggle))

;; (use-package emacs
;;   :init
;;   (setq modus-themes-mode-line '(accented borderless)
;; 	modus-themes-org-blocks 'tinted-background
;; 	modus-themes-deuteranopia nil
;; 	modus-themes-bold-constructs t
;; 	modus-themes-italic-constructs t
;; 	modus-themes-syntax '(yellow-comments green-strings alt-syntax)
;; 	modus-themes-mixed-fonts t
;; 	modus-themes-links '(neutral-underline)
;; 	;; modus-themes-links nil
;; 	;; modus-themes-box-buttons '(flat faint)
;; 	modus-themes-box-buttons nil
;; 	modus-themes-prompts nil
;; 	modus-themes-fringe nil
;; 	modus-themes-lang-checkers '(straight-underline)
;; 	modus-themes-hl-line nil
;; 	modus-themes-subtle-line-numbers t
;; 	modus-themes-markup nil
;; 	modus-themes-region '(no-extend)
;; 	modus-themes-org-agenda '((scheduled . uniform))
;; 	;; modus-themes-org-agenda '((header-block . (1.5 variable-pitch))
;; 	;; 			  (header-date . (grayscale workaholic bold-today))
;; 	;; 			  (event . (accented varied))
;; 	;; 			  (scheduled . uniform)
;; 	;; 			  (habit . traffic-light))
;; 	modus-themes-scale-headings t
;; 	modus-themes-headings
;; 	'((1 . (background rainbow overline bold  1.2))
;; 	  (2 . (rainbow overline bold 1.1))
;; 	  (3 . (overline rainbow bold 1.0))
;; 	  (4 . (rainbow bold))
;; 	  (t . (bold))))
;;   :config
;;   (load-theme 'modus-vivendi)
;;   :bind ("<f5>" . modus-themes-toggle))

;; themes I like:
;; ef-duo-dark
;; doom-dark+
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

  (mapc #'disable-theme custom-enabled-themes))


;;; hl-line-mode ---------------------------------------------------------------

(use-package emacs
  :config
  (add-hook 'dired-mode-hook #'hl-line-mode)
  (add-hook 'org-agenda-mode-hook #'hl-line-mode)
  (add-hook 'ibuffer-mode-hook #'hl-line-mode))


;;; modline

(straight-use-package 'all-the-icons)
(use-package all-the-icons)

(use-package smart-mode-line
  :config
  ;; give a bit extra space to battery indicator 
  (setq	sml/battery-format " %p ")
  (setq sml/extra-filler -6)

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

  (defvar jds~hidden-minor-modes
    '(yas-minor-mode
      citar-embark-mode
      evil-traces-mode
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
      visual-line-mode))

  (defun jds~purge-minor-modes ()
    (mapc 'diminish jds~hidden-minor-modes))

  (add-hook 'after-change-major-mode-hook 'jds~purge-minor-modes))

;;; Olivetti Mode

(use-package olivetti
  :disabled t)

;;; Nice Display of Colors

  (use-package rainbow-mode
    :diminish rainbow-mode
    :config
    (setq rainbow-x-colors nil)
    (add-hook 'prog-mode-hook 'rainbow-mode))


(use-package hide-mode-line
  :disabled)


(provide 'config-themes)
