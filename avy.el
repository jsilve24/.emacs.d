;;; avy.el --- jumping config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/avy
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  jumping config
;;
;;; Code:


;;; avy
(use-package avy
  :straight t
  :after evil
  :config
  (avy-setup-default)
  (setq avy-keys (number-sequence ?a ?z)
        avy-style 'de-bruijn
        avy-all-windows 'nil
        avy-case-fold-search t
        avy-highlight-first t
        avy-single-candidate-jump t
        avy-styles-alst '((avy-goto-line . pre))
        avy-orders-alist '((avy-goto-char . avy-order-closest)
                           (avy-goto-word-0 . avy-order-closest)
                           (avy-goto-line . avy-order-closest))))

;;; link-hint

(use-package link-hint
  :straight t
  :after avy
  :config
  ;; Use qutebrowser to open urls
  (setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program "qutebrowser")
  ;; Open urls in a new tab instead of window; can also be set in the config file
  (setq browse-url-generic-args '("--target" "tab"))

  ;; to override avy settings
  ;; (setq link-hint-ayy-style 'at)
  )

;;; evil-easymotion
(use-package evil-easymotion
  :straight t
  :after avy
  :config
  ;; Use evil-search backend, instead of isearch
  (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                      :bind ((evil-ex-search-highlight-all nil)))

  ;; Rebind scope of w/W/e/E/ge/gE evil-easymotion motions to the visible
  ;; buffer, rather than just the current line.
  (put 'visible 'bounds-of-thing-at-point (lambda () (cons (window-start) (window-end))))
  (evilem-make-motion evilem-motion-forward-word-begin #'evil-forward-word-begin :scope 'visible)
  (evilem-make-motion evilem-motion-forward-WORD-begin #'evil-forward-WORD-begin :scope 'visible)
  (evilem-make-motion evilem-motion-forward-word-end #'evil-forward-word-end :scope 'visible)
  (evilem-make-motion evilem-motion-forward-WORD-end #'evil-forward-WORD-end :scope 'visible)
  (evilem-make-motion evilem-motion-backward-word-begin #'evil-backward-word-begin :scope 'visible)
  (evilem-make-motion evilem-motion-backward-WORD-begin #'evil-backward-WORD-begin :scope 'visible)
  (evilem-make-motion evilem-motion-backward-word-end #'evil-backward-word-end :scope 'visible)
  (evilem-make-motion evilem-motion-backward-WORD-end #'evil-backward-WORD-end :scope 'visible)
  )

(provide 'avy)
;;; avy.el ends here
