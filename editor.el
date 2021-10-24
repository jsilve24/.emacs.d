;;; editor.el --- core editing features -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 24, 2021
;; Modified: October 24, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/editor
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  core editing features
;;
;;; Code:

;; (use-package smartparens
;;   :straight t
;;   :defer t
;;   :config
;;   (require 'smartparens-config)
;;   (show-smartparens-global-mode t))

;; seems lighter weight than smartparens
(electric-pair-mode 1)

;;; evil-suround and evil-embrace
(use-package evil-surround
  :straight t
  :ensure t
  :config
  (global-evil-surround-mode 1)
  ;; not sure why its bound to gS or S in visual state but I don't like the asymmetry
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-Surround-region))


(use-package evil-embrace
  :straight t
  :after evil-surround
  :config
  (evil-embrace-enable-evil-surround-integration)
  (add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook))
;; see here: https://github.com/cute-jumper/embrace.el#adding-more-surrounding-pairs
;; for how to add more custom pairs


;;; setup evil alignment (evil-lion)
(use-package evil-lion
  :straight t
  :defer t
  :config
  ;; these need to be called before evil-lion-mode is called
  (setq evil-lion-left-align-key (kbd "z l"))
  (setq evil-lion-right-align-key (kbd "z L"))
  (evil-lion-mode))



(provide 'editor)
;;; editor.el ends here
