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


(use-package expand-region)

;;; fill and unfill
(use-package unfill
  :commands (unfill-paragraph unfill-region unfill-toggle))


;;; easier lisp editing with lispyville
(use-package lispy
  :config
  (lispy-set-key-theme '(lispy c-digits))
  (add-hook 'emacs-lisp-mode-hook (lambda () (progn (lispy-mode 1) (diminish 'lispy-mode))))

  ;; use lispy in minibuffer eval-expression
  ;; (defun conditionally-enable-lispy ()
  ;;   (when (eq this-command 'eval-expression)
  ;;     (lispy-mode 1)))
  ;; (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
  )

(use-package lispyville
  :after lispy
  :diminish lispyville-mode
  :hook (lispy-mode . lispyville-mode)
  :config
  ;; choose key-themes I want
  (lispyville-set-key-theme
   '(operators
     text-objects
     atom-motions
     ;; additional-motions ;; these are just weird I just bind these myself
     prettify 
     ;; commentary ;; doesn't work since I set gc in override see hook below instead
     slurp/barf-cp
     wrap
     additional
     additional-insert
     ;; mark
     ))
  ;; hack to activate some commentary (and more) functions 
  (defun lispyville-activate-commentary-theme ()
    ;; override default comment
    (evil-define-key 'motion 'local (kbd "M-;") #'lispyville-comment-or-uncomment-line)
    (evil-define-key 'insert 'local (kbd "M-;") #'lispyville-comment-or-uncomment-line)
    (evil-define-key 'insert 'local (kbd ";") #'self-insert-command)
    (evil-define-key '(motion operator) 'local (kbd "[[") 'lispyville-previous-opening)
    (evil-define-key '(motion operator) 'local (kbd "{") 'lispyville-previous-closing)
    (evil-define-key '(motion operator) 'local (kbd "]]") 'lispyville-next-opening)
    (evil-define-key '(motion operator) 'local (kbd "}") 'lispyville-next-closing)
    (evil-define-key 'insert 'local (kbd "[") 'self-insert-command)
    (evil-define-key 'insert 'local (kbd "]") 'self-insert-command)
    (evil-define-key '(motion) 'local (kbd "(") 'lispyville-backward-up-list)
    (evil-define-key '(motion) 'local (kbd ")") 'lispyville-up-list)
    (evil-define-key '(insert) 'local (kbd "C-w") 'lispyville-delete-backward-word))
  (add-hook 'lispyville-mode-hook #'lispyville-activate-commentary-theme))



;; stolen from here: http://xahlee.info/emacs/emacs/elisp_title_case_text.html
;;;###autoload
(defun jds/title-case-region-or-line (@begin @end)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, *begin *end are region boundaries.
URL `http://xahlee.info/emacs/emacs/elisp_title_case_text.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           $p1
           $p2
           ($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward $skipChars (line-beginning-position))
         (setq $p1 (point))
         (skip-chars-forward $skipChars (line-end-position))
         (setq $p2 (point)))
       (list $p1 $p2))))
  (let* (
         ($strPairs [
                     [" A " " a "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" Via " " via "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ["'T " "'t "]
                     ]))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while
                 (search-forward (aref $x 0) nil t)
               (replace-match (aref $x 1) "FIXEDCASE" "LITERAL")))
           $strPairs))))))


;;; customize imenu listings ---------------------------------------------------

;; no longer needed with outli (below) 
;; 
;; (with-eval-after-load 'imenu
;;   (defun jds~add-to-imenu-hook-function ()
;;     "Add this hook to major modes to customize imenu supported expressions"
;;     (interactive)
;;     (add-to-list 'imenu-generic-expression '("Sections" "^\\(.+\\)----$" 1)))
;;   (add-hook 'text-mode-hook 'jds~add-to-imenu-hook-function)
;;   (add-hook 'prog-mode-hook 'jds~add-to-imenu-hook-function))

;;; outli mode -----------------------------------------------------------------

;; https://github.com/jdtsmith/outli
(use-package outli
  :straight (outli :type git :host github :repo "jdtsmith/outli")
  :hook (emacs-lisp-mode . outli-mode)
  :hook (ess-r-mode . outli-mode)
  :config
  (add-to-list 'outli-heading-config '(ess-r-mode "##" ?# t)))

(jds/localleader-def
  :keymaps '(text-mode-map prog-mode-map)
  "n" #'outli-toggle-narrow-to-subtree)

;;; autocapitalize -------------------------------------------------------------

;; (use-package captain
;; :config
;; (global-captain-mode)
;; ;; only work in comments in programming modes
;; (add-hook 'prog-mode-hook
;; (lambda ()
;; (setq captain-predicate (lambda () (nth 8 (syntax-ppss (point)))))))
;; ;; Or for text modes, work all the time:
;; (add-hook 'text-mode-hook
;; (lambda ()
;; (setq captain-predicate (lambda () t))))
;; ;; turn on in slack buffers
;; ;; (add-hook 'text-mode-hook
;; ;;            (lambda ()
;; ;;              (setq captain-predicate (lambda () t))))
;; ;; Or don't work in source blocks in Org mode:
;; (add-hook
;; 'org-mode-hook
;; (lambda ()
;; (setq captain-predicate
;; (lambda () (not (org-in-src-block-p)))))))

;;; tags -----------------------------------------------------------------------

;; (use-package ggtags)


;;; dumb jump ------------------------------------------------------------------

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;; highlighting ---------------------------------------------------------------

(use-package hl-todo
  ;; :hook (LaTeX-mode hl-todo-mode)
  ;; :hook (ess-r-mode hl-todo-mode)
  ;; :hook (emacs-lisp-mode hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
	'(("HOLD" . "#d0bf8f")
	  ("TODO" . "#cc9393")
	  ("NEXT" . "#dca3a3")
	  ("THEM" . "#dc8cc3")
	  ("PROG" . "#7cb8bb")
	  ("OKAY" . "#7cb8bb")
	  ("DONT" . "#5f7f5f")
	  ("FAIL" . "#8c5353")
	  ("DONE" . "#afd8af")
	  ("NOTE" . "#d0bf8f")
	  ("CITE" . "#d0bf8f")
	  ("HACK" . "#d0bf8f")
	  ("TEMP" . "#d0bf8f")
	  ("FIXME" . "#cc9393")
	  ("XXX+" . "#cc9393")))
  (global-hl-todo-mode))

(jds/localleader-def
  :keymaps '(LaTeX-mode-map emacs-lisp-mode-map ess-r-mode-map)
  "f" #'hl-todo-occur)

(general-def
  :states 'n
  :keymaps '(LaTeX-mode-map emacs-lisp-mode-map ess-r-mode-map)
  "]t" #'hl-todo-next
  "[t" #'hl-todo-previous)

(provide 'editor)
;;; editor.el ends here
