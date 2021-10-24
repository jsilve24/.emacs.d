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

(use-package undo-fu
  :straight t)

;;; main evil config

(straight-use-package 'evil)
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
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
        evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window
        ;; It's infuriating that innocuous "beginning of line" or "end of line"
        ;; errors will abort macros, so suppress them:
        evil-kbd-macro-suppress-motion-error t
        evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)


  ;; make normal state teh default always
  (setq evil-emacs-state-modes nil)
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



  ;; leader key
  ;; set leader in all states
  (evil-set-leader nil (kbd "M-SPC"))
  ;; set leader in normal state
  (evil-set-leader 'normal (kbd "SPC"))
  ;; set local leader
  (evil-set-leader 'normal "\\" t))


;;; evil-collection

(straight-use-package 'evil-collection)
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list
        (delq 'pdf evil-collection-mode-list))
  (evil-collection-init))


;;; Additional Evil packages

;; (use-package evil-embrace
;;   :straight t
;;   :commands embrace-add-pair embrace-add-pair-regexp
;;   :hook (LaTeX-mode . embrace-LaTeX-mode-hook)
;;   :hook (LaTeX-mode . +evil-embrace-latex-mode-hook-h)
;;   :hook (org-mode . embrace-org-mode-hook)
;;   :hook (ruby-mode . embrace-ruby-mode-hook)
;;   :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
;;   :hook ((lisp-mode emacs-lisp-mode clojure-mode racket-mode hy-mode)
;;          . +evil-embrace-lisp-mode-hook-h)
;;   :hook ((c++-mode rustic-mode csharp-mode java-mode swift-mode typescript-mode)
;;          . +evil-embrace-angle-bracket-modes-hook-h)
;;   :hook (scala-mode . +evil-embrace-scala-mode-hook-h)
;;   :init
;;   (after! evil-surround
;;     (evil-embrace-enable-evil-surround-integration))
;;   :config
;;   (setq evil-embrace-show-help-p nil)

;;   (defun +evil-embrace-scala-mode-hook-h ()
;;     (embrace-add-pair ?$ "${" "}"))

;;   (defun +evil-embrace-latex-mode-hook-h ()
;;     (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" #'+evil--embrace-latex))

;;   (defun +evil-embrace-lisp-mode-hook-h ()
;;     ;; Avoid `embrace-add-pair-regexp' because it would overwrite the default
;;     ;; `f' rule, which we want for other modes
;;     (push (cons ?f (make-embrace-pair-struct
;;                     :key ?f
;;                     :read-function #'+evil--embrace-elisp-fn
;;                     :left-regexp "([^ ]+ "
;;                     :right-regexp ")"))
;;           embrace--pairs-list))

;;   (defun +evil-embrace-angle-bracket-modes-hook-h ()
;;     (let ((var (make-local-variable 'evil-embrace-evil-surround-keys)))
;;       (set var (delq ?< evil-embrace-evil-surround-keys))
;;       (set var (delq ?> evil-embrace-evil-surround-keys)))
;;     (embrace-add-pair-regexp ?< "\\_<[a-z0-9-_]+<" ">" #'+evil--embrace-angle-brackets)
;;     (embrace-add-pair ?> "<" ">"))

;;   ;; Add escaped-sequence support to embrace
;;   (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
;;         (make-embrace-pair-struct
;;          :key ?\\
;;          :read-function #'+evil--embrace-escaped
;;          :left-regexp "\\[[{(]"
;;          :right-regexp "\\[]})]")))



(use-package evil-snipe
  ;; :commands evil-snipe-local-mode evil-snipe-override-local-mode
  :ensure t
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'whole-visible
        evil-snipe-repeat-scope 'while-visible
        evil-snipe-char-fold t)
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  (evil-define-key 'visual evil-snipe-local-mode-map "z" 'evil-snipe-s)
  (evil-define-key 'visual evil-snipe-local-mode-map "Z" 'evil-snipe-S)
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


;;; evil-surround
(use-package evil-surround
  :straight t
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))




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
