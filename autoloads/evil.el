;;; evil.el --- random evil stuff -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/evil
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  random evil stuff
;;
;;; Code:

;; from doom
;;;###autoload
(defun +evil/reselect-paste ()
  "Return to visual mode and reselect the last pasted region."
  (interactive)
  (cl-destructuring-bind (_ _ _ beg end &optional _)
      evil-last-paste
    (evil-visual-make-selection
     (save-excursion (goto-char beg) (point-marker))
     end)))

(provide 'evil)
;;; evil.el ends here
