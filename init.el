;;; init.el --- Justin Silverman Config -*- lexical-binding: t; -*-

(defun load-config (fn)
  (load (expand-file-name fn user-emacs-directory)))

;; setup native compilation
(if (and (fboundp 'native-comp-available-p)
 	        (native-comp-available-p))
  (progn
    (message "Native compilation is available")
    (setq native-comp-deferred-compilation t))
    (message "Native complation is *not* available"))

(setq jds~skip-wm nil
      jds~skip-email nil)

;;; Modules
(load-config "core.el")
(load-config "defaults.el")
(load-config "macros.el")
(load-config "evil.el")
(load-config "org.el") ;; put before anything that might call org (e.g., themes or completing-read)
(load-config "themes.el")
(load-config "popups.el")
(load-config "completing-read.el")
(load-config "projects.el")
(load-config "git.el")
(load-config "term.el")
(load-config "system-jump.el")
(load-config "avy.el")
(load-config "autoloads/avy.el")
(load-config "autoloads/window.el")
(load-config "window.el")
(unless jds~skip-wm
  (load-config "wm.el"))
(load-config "hydra.el")
(load-config "eval.el")
(load-config "dired.el")
(load-config "autoloads/dired.el")
(load-config "spelling.el")
(load-config "latex.el")		
(load-config "autoloads/sow.el")
(load-config "pdf.el")
(unless jds~skip-email t
	(load-config "autoloads/email.el")
	(load-config "email.el"))
(load-config "calendar.el")
(load-config "autoloads/movement.el")
(load-config "ibuffer.el")
(load-config "editor.el")
(load-config "snippets.el")
;; after everything else
(load-config "bindings.el")
(load-config "debugging.el")
;; (load-config "languages.el")
(load-config "references.el")
(load-config "autoloads/references.el")
(load-config "ssh.el")
(load-config "org-roam.el")
(load-config "lsp.el")
(load-config "treesitter.el")
(load-config "autoloads/avy-tex-math.el")


;; for some reason has to be at the end to ensure 
;; that company-active-map is defined
;; (load-config "company.el")
(load-config "capf.el")


;;; languages
(load-config "python.el")
(load-config "ess.el")
(load-config "autoloads/ess-autoloads.el")
(load-config "stan.el")
(load-config "ledger.el")

;;; "optional" applications
(load-config "slack.el")
(load-config "spotify.el")
(load-config "bitwarden.el")

;; Start Server if not already running
(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

(setq-default fill-column 100)



;;; miscellaneous

(use-package comment-dwim-2
  :straight (comment-dwim-2 :type git :host github :repo "remyferre/comment-dwim-2" :branch "master")
  :commands (comment-dwim-2 org-comment-dwim-2)
  :bind (("M-;" . comment-dwim-2)
         :map org-mode-map
         ("M-;" . org-comment-dwim-2))
  :config
  ;; make sure to add a space after the comment charater in inline-comments
  ;; I like the way this looks better
  (advice-add 'comment-indent :after #'just-one-space))



(use-package epa
  :config
  (epa-file-enable)
  (setq epa-file-cache-passphrase-for-symmetric-encryption t
	epa-pinentry-mode 'loopback))



;;; profiling and debugging
;(use-package esup
;  :ensure t
;  :pin melpa
;  :config
;  (setq esup-depth 0))


(put 'narrow-to-page 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5fdc0f5fea841aff2ef6a75e3af0ce4b84389f42e57a93edc3320ac15337dc10" "e87f48ec4aebdca07bb865b90088eb28ae4b286ee8473aadb39213d361d0c45f" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(safe-local-variable-values
   '((eval setq-local ess-startup-directory default-directory)))
 '(warning-suppress-log-types '((org-babel) (comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.5 :foreground "red")))))
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'list-timers 'disabled nil)
