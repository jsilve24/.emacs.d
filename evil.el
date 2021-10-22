;;; evil.el --- Evil Bindings and Setup -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jds6696@psu.edu>
;; Created: October 20, 2021
;; Modified: October 20, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/evil
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;

;;
;;; Code:

(straight-use-package 'evil)
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  :config
  (evil-mode 1)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; stop cursor creep when switching back to normal mode
  (setq evil-move-cursor-back nil)
  ;; allow cursor to move 1 character past end of line.
  (setq evil-move-beyond-eol t)

  ;; Don't put overwritten text in the kill ring
  (setq evil-kill-on-visual-paste nil)

  ;; Get better undo history in insert mode
  (setq evil-want-fine-undo t)


  )

(straight-use-package 'evil-collection)
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(provide 'evil)
;;; evil.el ends here
