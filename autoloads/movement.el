;;; movement.el --- quick movement commands -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 24, 2021
;; Modified: October 24, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/movement
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  quick movement commands
;;
;;; Code:

;;; more intuitive paragraph movement

;; I want to define a paragraph as just a contiguous block of text without a blank line in it.


;;;###autoload

(defun jds/goto-next-non-blank-line ()
  "Move point to next non-blank line"
  (beginning-of-line)
  (while (looking-at-p "^[:blank:]*$")
    (next-line)))

;;;###autoload
(defun jds/goto-last-non-blank-line ()
  "Move point to next non-blank line"
  (beginning-of-line)
  (while (looking-at-p "^[:blank:]*$")
  (previous-line)))


;;;###autoload
(evil-define-motion jds/paragraph-forward ()
  "Move to first character after next blank-line."
  :type line
  (progn
    (while (not (looking-at-p "^[:blank:]*$"))
      (next-line))
    (jds/goto-next-non-blank-line)))


;;;###autoload
(evil-define-motion jds/paragraph-backwards ()
  "Move to first character on line before prior blank-line."
  :type line
  (progn
    (while (not (looking-at-p "^[:blank:]*$"))
      (previous-line))
    (jds/goto-last-non-blank-line)))


(provide 'movement)
;;; movement.el ends here
