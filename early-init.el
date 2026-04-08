;;; early-init.el --- Description -*- lexical-binding: t; -*-

;; To make sure straight works - prevent package.el loading packages prior to
;; init-file loading
(setq package-enable-at-startup nil
      ;; Keep built-in package.el state away from stale ~/.emacs.d/elpa
      ;; contents so straight.el doesn't warn if a package requires it later.
      package-user-dir (expand-file-name "var/package-el" user-emacs-directory))

(provide 'early-init)
;;; early-init.el ends here
