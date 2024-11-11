;; don't load wm.el
(setq jds~skip-wm t
      jds~skip-email t)

;; ess don't ask for startup directory
(setq ess-ask-for-ess-directory nil
      ess-startup-directory 'default-directory)

;; load main init.el file
(load (expand-file-name "init.el" user-emacs-directory))
(require 'ox-beamer)
;; (require 'ox-bibtex)

