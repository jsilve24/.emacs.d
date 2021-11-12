;;; popups.el --- config related to managing popups -*- lexical-binding: t; -*-

(use-package popper
  :ensure t
  ;; :bind (:map popper-mode-map
  ;; 	      ("q" . popper-kill-latest-popup))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
	  "\\*Apropos\\*"
          "\\*Async Shell Command\\*"
          help-mode
	  helpful-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
  (setq popper-display-control t) ; default is t
  (setq popper-display-function #'display-buffer-below-selected)
  (setq popper-window-height 22))         

(provide 'popups)