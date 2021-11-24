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
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-Surround-region))


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


;;; fill and unfill
(use-package unfill
  :commands (unfill-paragraph unfill-region unfill-toggle))

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


(provide 'editor)
;;; editor.el ends here
