;;; term.el --- vterm setup -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/term
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  vterm setup
;;
;;; Code:

;; (straight-use-package 'vterm)
;; (use-package vterm
;;   :commands vterm
;;   :hook (vterm-mode . hide-mode-line-mode)
;;   ;; :bind (:map vterm-mode-map
;;   ;; 	      ("C-c C-c" . vterm-send-C-c)
;;   ;; 	      ("C-c C-d" . vterm-send-C-d))
;;   :config
;;   (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
;;   ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
;;   (setq vterm-max-scrollback 10000)
;;   (setq vterm-kill-buffer-on-exit t)
;;   (setq vterm-copy-exclude-prompt t)

;;   ;; don't show line numbers
;;   (add-hook 'vterm-mode-hook  (lambda() (display-line-numbers-mode 0))))

;; ;; (general-define-key
;; ;;  :keymaps 'vterm-mode-map
;; ;;  :state '(n v i m e)
;; ;;  "C-c C-c" #'vterm-send-C-c
;; ;;  "C-c C-d" #'vterm-send-C-d)


(use-package hide-mode-line)

(use-package multi-vterm
        ;; :hook  (vterm-mode . hide-mode-line-mode)
	:config
	(add-hook 'vterm-mode-hook
			(lambda ()
			;; (setq-local evil-insert-state-cursor 'box)
			(evil-insert-state)))
	(define-key vterm-mode-map [return]                      #'vterm-send-return)

	(setq vterm-keymap-exceptions nil)
	(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
	;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
	(setq vterm-max-scrollback 10000)
	(setq vterm-kill-buffer-on-exit t)
	(setq vterm-copy-exclude-prompt t)

	(evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
	(evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
	;; (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
	;; ;; (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
	;; ;; (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
	;; (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
	(evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume))

(jds/localleader-def
  :keymaps 'vterm-mode-map
  :states 'normal
  "c" #'multi-vterm
  "h" #'multi-vterm-next
  "l" #'multi-vterm-prev)

(provide 'term)
;;; term.el ends here
