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


;;; link-hint

;;;###autoload
(defun jds/link-hint-goto-link ()
    "Use link-hint to jump to link but do nothing."
    (interactive)
    (link-hint--one :goto))

(provide 'avy)
;;; avy.el ends here
