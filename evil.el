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

;;; simple undo system

;; (use-package undo-fu
;;   :straight t)

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))

;;; main evil config
(straight-use-package 'evil)
(use-package evil
  :init
  (setq evil-respect-visual-line-mode t) ; needs to be front and center

  (setq evil-want-C-i-jump t
	evil-want-Y-yank-to-eol t
	evil-want-C-u-scroll nil
	evil-want-C-d-scroll nil
	evil-want-keybinding nil
	evil-want-integration t)
  ;; from doom
  (defun +evil-default-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-normal-color)))
  (defun +evil-emacs-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-emacs-color)))

  (setq evil-ex-search-vim-style-regexp t
	evil-ex-visual-char-range t
	evil-mode-line-format nil
	evil-symbol-word-search t
	;; if the current state is obvious from the cursor's color/shape, then
	;; we won't need superfluous indicators to do it instead.
	evil-default-cursor '+evil-default-cursor-fn
	evil-normal-state-cursor 'box
	evil-emacs-state-cursor '(box +evil-emacs-cursor-fn)
	evil-insert-state-cursor 'bar
	evil-visual-state-cursor 'hollow
	;; Only do highlighting in selected window so that Emacs has less work
	;; to do highlighting them all.
	evil-ex-interactive-search-highlight 'selected-window
	;; It's infuriating that innocuous "beginning of line" or "end of line"
	;; errors will abort macros, so suppress them:
	evil-kbd-macro-suppress-motion-error t
	;; evil-undo-system 'undo-fu
	evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)



  ;; leader key
  ;; set leader in all states
  (evil-set-leader nil (kbd "M-SPC"))
  ;; set leader in normal state
  (evil-set-leader 'motion (kbd "SPC"))
  ;; set leader in normal state
  (evil-set-leader 'operator (kbd "SPC"))
  ;; set leader in normal state
  (evil-set-leader 'normal (kbd "SPC"))
  ;; set local leader
  (evil-set-leader 'normal "\\" t)
  (evil-set-leader 'motion "\\" t)
  (evil-set-leader 'operator "\\" t)


  ;; remove ret binding as its pretty much useless but blocks some modes
  (define-key evil-motion-state-map (kbd "RET") nil)



  ;; make normal state the default always
  (setq evil-emacs-state-modes '(vterm-mode))
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)

  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; stop copying each visual state move to the clipboard:
  ;; https://github.com/emacs-evil/evil/issues/336
  ;; grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (advice-add #'evil-visual-update-x-selection :override #'ignore)

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

  ;; deal with whitespace polluting kill-ring
  ;;from here: https://emacs.stackexchange.com/questions/39434/evil-dont-yank-with-only-whitespace-to-register
  ;; (define-key evil-normal-state-map "x" 'delete-forward-char)
  ;; (define-key evil-normal-state-map "X" 'delete-backward-char)
  ;; (evil-define-operator evil-delete-without-register-if-whitespace (beg end type reg yank-handler)
  ;;   (interactive "<R><y>")
  ;;   (let ((text (replace-regexp-in-string "\n" "" (filter-buffer-substring beg end))))
  ;;     (if (string-match-p "^\\s-*$" text)
  ;; 	  (evil-delete beg end type ?_)
  ;; 	(evil-delete beg end type reg yank-handler))))
  ;; (define-key evil-normal-state-map "d" #'evil-delete-without-register-if-whitespace)
  )

;;; evil-collection

(use-package evil-collection
  :straight (evil-collection :local-repo "~/.emacs.d/local-packages/evil-collection")
  :after evil
  :config
  ;; (setq evil-collection-mode-list
  ;;       (delq 'pdf evil-collection-mode-list))
  (evil-collection-init))


;;; Additional Evil packages




(use-package evil-snipe
  ;; :commands evil-snipe-local-mode evil-snipe-override-local-mode
  :ensure t
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'whole-visible
        evil-snipe-repeat-scope 'whole-visible
        evil-snipe-char-fold t)
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  (evil-define-key 'visual evil-snipe-local-mode-map "q" 'evil-snipe-s)
  (evil-define-key 'visual evil-snipe-local-mode-map "Q" 'evil-snipe-S)
  (evil-define-key 'visual evil-snipe-local-mode-map "x" 'evil-snipe-x)
  (evil-define-key 'visual evil-snipe-local-mode-map "X" 'evil-snipe-X)

  ;; To map [ to any opening parentheses or bracket in all modes:
  (push '(?\[ "[[{(]") evil-snipe-aliases)

  ;; It seems evil-snipe-override-mode causes problems in Magit buffers, to fix this:
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)

  (setq evil-snipe-smart-case t))

(use-package evil-nerd-commenter
  :straight t
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general ([remap comment-line] #'evilnc-comment-or-uncomment-lines))



;;; evil-suround and evil-embrace
(use-package evil-surround
  :straight t
  :ensure t
  :config
  (global-evil-surround-mode 1)
  ;; not sure why its bound to gS or S in visual state but I don't like the asymmetry
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-Surround-region))


(use-package evil-embrace
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


(use-package evil-args
  :demand t
  :after evil
  :commands (evil-inner-arg evil-outer-arg
             evil-forward-arg evil-backward-arg
             evil-jump-out-args))


(use-package posframe)

(use-package evil-owl
  :config
  (setq evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args '(:width 70 :height 20)
        evil-owl-max-string-length 50)
  (evil-owl-mode))


;; (use-package evil-traces
;;   :straight t
;;   :after evil-ex
;;   :config
;;   (pushnew! evil-traces-argument-type-alist
;;             '(+evil:align . evil-traces-global)
;;             '(+evil:align-right . evil-traces-global))
;;   (evil-traces-mode))


(provide 'evil)
;;; evil.el ends here

