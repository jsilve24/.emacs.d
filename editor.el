;;; editor.el --- core editing features -*- lexical-binding: t; -*-

;; guess indentation style 
(use-package dtrt-indent
  :disabled t
  :diminish dtrt-indent-global-mode
  :diminish dtrt-indent-mode
  :config
  (dtrt-indent-global-mode))


;; autorevert org and dired buffers
(use-package autorevert
  :ensure t
  :config
  (setq auto-revert-check-vc-info t)
  (global-auto-revert-mode t))


;; seems lighter weight than smartparens
(electric-pair-mode 1)
;; don't complete <>
;;  https://www.topbug.net/blog/2016/09/29/emacs-disable-certain-pairs-for-electric-pair-mode/
(setq electric-pair-inhibit-predicate
      (lambda (c)
	(if (char-equal c ?\<) t (electric-pair-default-inhibit c))))
;; another good option for less agressive completion:
;; https://www.reddit.com/r/emacs/comments/1h6b8ww/comment/m0x0o0f/?share_id=G76LIh8BuFenOhvn_Kdaz

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
     ;; text-objects modify to be buffer local
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
  (add-hook 'lispyville-mode-hook #'lispyville-activate-commentary-theme)

  ;; hack baed on evil-surround approach to define buffer local text objects
  (defun lispyville-activate-text-objects-theme ()
    "override default text objects activation and make bindings buffer local"
    (define-key evil-visual-state-local-map "ia" #'lispyville-inner-atom)
    (define-key evil-visual-state-local-map "il" #'lispyville-inner-list)
    (define-key evil-visual-state-local-map "io" #'lispyville-inner-sexp)
    (define-key evil-visual-state-local-map "if" #'lispyville-inner-function)
    (define-key evil-visual-state-local-map "iv" #'lispyville-inner-comment)
    (define-key evil-visual-state-local-map "iq" #'lispyville-inner-string)


    (define-key evil-visual-state-local-map "aa" #'lispyville-a-atom)
    (define-key evil-visual-state-local-map "al" #'lispyville-a-list)
    (define-key evil-visual-state-local-map "ao" #'lispyville-a-sexp)
    (define-key evil-visual-state-local-map "af" #'lispyville-a-function)
    (define-key evil-visual-state-local-map "av" #'lispyville-a-comment)
    (define-key evil-visual-state-local-map "aq" #'lispyville-a-string)


    (define-key evil-operator-state-local-map "ia" #'lispyville-inner-atom)
    (define-key evil-operator-state-local-map "il" #'lispyville-inner-list)
    (define-key evil-operator-state-local-map "io" #'lispyville-inner-sexp)
    (define-key evil-operator-state-local-map "if" #'lispyville-inner-function)
    (define-key evil-operator-state-local-map "iv" #'lispyville-inner-comment)
    (define-key evil-operator-state-local-map "iq" #'lispyville-inner-string)


    (define-key evil-operator-state-local-map "aa" #'lispyville-a-atom)
    (define-key evil-operator-state-local-map "al" #'lispyville-a-list)
    (define-key evil-operator-state-local-map "ao" #'lispyville-a-sexp)
    (define-key evil-operator-state-local-map "af" #'lispyville-a-function)
    (define-key evil-operator-state-local-map "av" #'lispyville-a-comment)
    (define-key evil-operator-state-local-map "aq" #'lispyville-a-string))

  (add-hook 'lispyville-mode-hook 'lispyville-activate-text-objects-theme))



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
  :disabled
  :straight (outli :type git :host github :repo "jdtsmith/outli")
  :hook (emacs-lisp-mode . outli-mode)
  :hook (ess-r-mode . outli-mode)
  :diminish outline-minor-mode
  :config
  (add-to-list 'outli-heading-config '(ess-r-mode "##" ?# t))

  (jds/localleader-def
  :keymaps '(text-mode-map prog-mode-map)
  "n" #'outli-toggle-narrow-to-subtree))


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
  "f" #'hl-todo-occur
  "F" #'hl-todo-rgrep
  "i" #'hl-todo-insert)

(general-def
  :states 'n
  :keymaps '(LaTeX-mode-map emacs-lisp-mode-map ess-r-mode-map)
  "]t" #'hl-todo-next
  "[t" #'hl-todo-previous)

;;; better deleting ------------------------------------------------------------

(use-package hungry-delete
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode 1))

(defun jds~delete-pairs-balanced ()
  "Delete pairs if previous character is pair opening. Returns t if
within pair, nil if not within pair."
  (if (looking-back (rx (or
			 (literal "(")
			 (literal "[")
			 (literal "{")))
		    1)
      (progn
	(electric-pair-delete-pair 1)
	t)
    nil))

;;;###autoload
(defun jds/hungry-delete-or-kill-sexp (&optional arg)
  "Hungry delete if point is on whitespace, otherwise kill-sexp. Delete forwards with ARG.
If point is within empty delmiters, kill the delimiters."
  (interactive "P")
  (unless (jds~delete-pairs-balanced)
    (cond
     ((string= major-mode "vterm-mode")
      (vterm-delete-region  (save-excursion (progn (backward-sexp) (point))) (point)))
     ((looking-back (rx (or (literal "(")
			    (literal "[")
			    (literal  "{")
			    (literal "\{")
			    (literal "\(")
			    (literal "\left(")
			    (literal "\left{")
			    (literal "\left["))))
      (electric-pair-delete-pair 1))
     ((looking-back (rx (>= 2 (char blank))) 1)
      (if arg
	  (hungry-delete-forward 1)
	(hungry-delete-backward 1)))
     (t
      (if arg
	  (kill-sexp)
	(backward-kill-sexp))))))

(use-package grugru
  :config
  (grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("t" "nil"))
  (grugru-define-on-major-mode 'ess-r-mode 'symbol '("TRUE" "FALSE"))
  (grugru-define-on-major-mode 'conf-javaprop-mode 'symbol '("true" "false"))
  (grugru-define-on-major-mode '(org-mode latex-mode LaTeX-mode) 'tex-command '("\\parallel" "\\perp"))
  (grugru-define-on-major-mode '(latex-mode LaTeX-mode) 'tex-command '("\\section" "\\subsection" "\\subsubsection" "\\paragraph")))


;;; indent

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config
  (aggressive-indent-mode +1))

(use-package comment-dwim-2
  :straight (comment-dwim-2 :type git :host github :repo "remyferre/comment-dwim-2" :branch "master")
  :commands (comment-dwim-2 org-comment-dwim-2)
  :bind (("M-;" . comment-dwim-2)
         :map org-mode-map
         ("M-;" . org-comment-dwim-2))
  :config
  ;; make sure to add a space after the comment charater in inline-comments
  ;; I like the way this looks better
  (advice-add 'comment-indent :after #'just-one-space))


(provide 'config-editor)
