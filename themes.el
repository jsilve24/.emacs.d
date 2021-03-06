;;; themes.el --- theme related config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jds6696@psu.edu>
;; Created: October 21, 2021
;; Modified: October 21, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/themes
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Theme related config
;;
;;; Code:

;;; Fonts

;;; Font stuff

(setq jds/default-font-size 90)
(setq jds/default-variable-font-size 90)
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

(straight-use-package 'doom-themes)

(use-package doom-themes
  :init (load-theme 'doom-vibrant t)
  )


(use-package kaolin-themes
  :config
  ;; (load-theme 'kaolin-aurora t)
  ;; (kaolin-treemacs-theme)
  )

(use-package modus-themes)

;; (setq modus-themes-mode-line '(accented borderless)
;;       modus-themes-bold-constructs t
;;       modus-themes-italic-constructs t
;;       modus-themes-fringes 'subtle
;;       modus-themes-tabs-accented t
;;       modus-themes-paren-match '(bold intense)
;;       modus-themes-prompts '(bold intense)
;;       modus-themes-completions 'opinionated
;;       modus-themes-org-blocks 'tinted-background
;;       modus-themes-scale-headings t
;;       modus-themes-region '(bg-only)
;;       modus-themes-headings
;;       '((1 . (rainbow overline background 1.3))
;;         (2 . (rainbow background 1.2))
;;         (3 . (rainbow bold 1.1))
;;         (t . (semilight 1.0))))

;; ;; Load the dark theme by default
;; (load-theme 'modus-operandi t)

;; (straight-use-package '(shanty-theme :host github :repo "qhga/shanty-theme"))
;; (use-package shanty-theme)

;;; modline

(straight-use-package 'all-the-icons)
(use-package all-the-icons)

;; NOTE: The first time you load your configuration on a new machine, you???ll need to run `M-x
;; all-the-icons-install-fonts` so that mode line icons display correctly.
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15))
  :config 
  (doom-themes-visual-bell-config))


;;; dashboard



;; use kisses mode to make things nice on startup
(use-package kisses
  :straight (kisses :local-repo "~/.emacs.d/local-packages/kisses/")
  :ensure t
  :config 

  (setq kisses-banner "      ____________
     /\\  ________ \\
    /  \\ \\______/\\ \\
   / /\\ \\ \\  / /\\ \\ \\
  / / /\\ \\ \\/ / /\\ \\ \\
 / / /__\\_\\/ / /__\\_\\ \\
/ /_/_______/ /________\\
\\ \\ \\______ \\ \\______  /
 \\ \\ \\  / /\\ \\ \\  / / /
  \\ \\ \\/ / /\\ \\ \\/ / /
   \\ \\/ / /__\\_\\/ / /
    \\  / /______\\/ /
     \\/___________/")
  
  ;; (setq kisses-banner
;; 	"RRRRRRRRRRRRRRRRR                  AAA                      JJJJJJJJJJJIIIIIIIIII
;; R::::::::::::::::R                A:::A                     J:::::::::JI::::::::I
;; R::::::RRRRRR:::::R              A:::::A                    J:::::::::JI::::::::I
;; RR:::::R     R:::::R            A:::::::A                   JJ:::::::JJII::::::II
;;   R::::R     R:::::R           A:::::::::A                    J:::::J    I::::I
;;   R::::R     R:::::R          A:::::A:::::A                   J:::::J    I::::I
;;   R::::RRRRRR:::::R          A:::::A A:::::A                  J:::::J    I::::I
;;   R:::::::::::::RR          A:::::A   A:::::A                 J:::::j    I::::I
;;   R::::RRRRRR:::::R        A:::::A     A:::::A                J:::::J    I::::I
;;   R::::R     R:::::R      A:::::AAAAAAAAA:::::A   JJJJJJJ     J:::::J    I::::I
;;   R::::R     R:::::R     A:::::::::::::::::::::A  J:::::J     J:::::J    I::::I
;;   R::::R     R:::::R    A:::::AAAAAAAAAAAAA:::::A J::::::J   J::::::J    I::::I
;; RR:::::R     R:::::R   A:::::A             A:::::AJ:::::::JJJ:::::::J  II::::::II
;; R::::::R     R:::::R  A:::::A               A:::::AJJ:::::::::::::JJ   I::::::::I
;; R::::::R     R:::::R A:::::A                 A:::::A JJ:::::::::JJ     I::::::::I
;; RRRRRRRR     RRRRRRRAAAAAAA                   AAAAAAA  JJJJJJJJJ       IIIIIIIIII")
  (put-text-property 0 (length kisses-banner) 'face 'outline-1
		     kisses-banner)
  )

(setq initial-buffer-choice 'kisses-initial-buffer)
;; initial-buffer-choice (lambda () (progn
      ;; 					 (switch-to-buffer (get-buffer-create "*splash*"))
      ;; 					 (splash--setup)))


;;; org-mode theme -------------------------------------------------------------

(with-eval-after-load 'org
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.25))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   '(org-agenda-structure ((t (:inherit outline-1 :height 1.10))))))

;; (straight-use-package 'dashboard)
;; (use-package dashboard
;;   :ensure t
;;   :config
;;   ;; In addition to the above, configure initial-buffer-choice to show Dashboard in frames created
;;   ;; with emacsclient -c as follows:
;;   ;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))


;;   (dashboard-setup-startup-hook)

;;   ;; Set the title
;;   (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
;;   ;; Set the banner
;;   (setq dashboard-startup-banner 'logo)
;;   ;; Value can be
;;   ;; 'official which displays the official emacs logo
;;   ;; 'logo which displays an alternative emacs logo
;;   ;; 1, 2 or 3 which displays one of the text banners
;;   ;; "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer

;;   ;; Content is not centered by default. To center, set
;;   (setq dashboard-center-content t)

;;   ;; To disable shortcut "jump" indicators for each section, set
;;   (setq dashboard-show-shortcuts nil)


;;   ;; To customize which widgets are displayed, you can use the following snippet
;;   (setq dashboard-items '()))


(provide 'themes)
;;; themes.el ends here
