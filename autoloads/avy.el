;;; avy.el --- avy functions -*- lexical-binding: t; -*-
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
;;  avy functions

;;
;;; Code:

;;;###autoload
(defun jds/evil-search-convert-avy-jump ()
  "Repeat the last evil-search but this time using avy-goto-char-timer."
  (interactive)
  (avy-with jds/evil-search-convert-avy-jump
    (avy-jump (regexp-quote (car evil-ex-search-pattern))
              :beg (window-start) :end (window-end))))


;;;###autoload
(defun jds/spaceless-key-to-string (keys)
  "Convert list of key-codes to string without adding spaces."
  (concat
   (mapc 'single-key-description keys)))

;;;###autoload
(defun jds/evil-snipe-convert-avy-jump ()
  "Repeat the last evil-search but this time using avy-goto-char-timer."
  (interactive)
  (avy-with jds/evil-search-convert-avy-jump
    (avy-jump (regexp-quote (jds/spaceless-key-to-string (nth 1 evil-snipe--last)))
              :beg (window-start) :end (window-end))))



;;;###autoload
(defun jds/avy-goto-word-0-end (arg &optional beg end)
  "Jump to a word start. The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
  (interactive "P")
  (avy-with avy-goto-word-0
    (avy-jump "\\sw\\b" :window-flip arg :beg beg :end end)))


;;;###autoload
(defun jds/avy-goto-delim-start (arg &optional beg end)
  "Jump to a word start. The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
  (interactive "P")
  (avy-with jds/avy-goto-delim-start
    (avy-jump "\\s(" :window-flip arg :beg beg :end end)))

;;;###autoload
(defun jds/avy-goto-delim-end (arg &optional beg end)
  "Jump to a word start. The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
  (interactive "P")
  (avy-with jds/avy-goto-delim-end
    (avy-jump "\\s)" :window-flip arg :beg beg :end end)))

;;;###autoload
(defun jds/avy-goto-punctuation (arg &optional beg end)
  "Jump to a punctuation. The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
  (interactive "P")
  (avy-with jds/avy-goto-punctuation
    (avy-jump "\\s." :window-flip arg :beg beg :end end)))

;;;###autoload
(defun jds/avy-goto-quote (arg &optional beg end)
  "Jump to a punctuation. The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
  (interactive "P")
  (avy-with jds/avy-goto-quote
    (avy-jump "[\\s\"\\s\']" :window-flip arg :beg beg :end end)))


;;;###autoload
(defun avy--org-agenda-cands ()
  (let (candidates point)
    (save-excursion
      (save-restriction
        (narrow-to-region (window-start) (window-end (selected-window) t))
        (setq point (goto-char (point-min)))
        (while (setq point (text-property-not-all point (window-end) 'org-marker nil))
          (push (cons point (selected-window)) candidates)
          (setq point (text-property-any point (window-end) 'org-marker nil)))))
    (nreverse candidates)))

;;;###autoload
(defun avy-org-agenda ()
  "Goto a visible item in an `org-mode-agenda' buffer."
  (interactive)
  (avy-action-goto (avy-with avy-org-agenda
                     (avy-process (avy--org-agenda-cands)))))

;;;###autoload
(defun jds/avy-org-agenda-and-jump ()
  "Jump to headline after selecting with avy-org-agenda"
  (interactive)
  (progn
    (avy-org-agenda)
    (org-agenda-switch-to)))


;; stolen from doom
;;;###autoload
(defun jds/org-headline-avy ()
  "TODO"
  (require 'avy)
  (save-excursion
    (when-let* ((org-reverse-note-order t)
                (pos (avy-with avy-goto-line (avy-jump (rx bol (1+ "*") (1+ blank))))))
      (when (integerp (car pos))
        ;; If avy is aborted with "C-g", it returns `t', so we know it was NOT
        ;; aborted when it returns an int. If it doesn't return an int, we
        ;; return nil.
        (copy-marker (car pos))))))

;;stolen from doom
;;;###autoload
(defun jds/org-goto-visible ()
  "TODO"
  (interactive)
  (goto-char (jds/org-headline-avy)))






;;; link-hint

;;;###autoload
(defun jds/link-hint-goto-link ()
  "Use link-hint to jump to link but do nothing."
  (interactive)
  (link-hint--one :goto))

(provide 'avy)
;;; avy.el ends here
