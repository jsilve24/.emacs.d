;;; org.el --- org functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/org
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  org functions org-functions
;;
;;; Code:


;;;###autoload
(defun jds/org-agenda-show-custom-day (&optional arg)
  (interactive "P")
  (progn
    (org-agenda arg "d")
    (evil-goto-first-line)))


;;;###autoload
(defun jds/open-custom-day-agenda-new-frame ()
  (interactive)
  (select-frame (make-frame))
  (org-agenda nil "d"))

;;;###autoload
(defun jds/quoteless-day-agenda ()
  "intended only to be called by i3"
  (org-agenda nil "d"))



;; Save excusion and window position
;;;###autoload
(defun jds/save-excursion-and-min-point (fun)
  "Execute FUN but save excusion but also reset window 'point-min'.
Should pass FUN quoted e.g., #'org-agenda-refile."
  (interactive)
  (let ((cpoint (point))
          (minpoint (window-start)))
      (funcall fun)
      (set-window-start (selected-window) minpoint)
      (goto-char cpoint)))


;; custom agenda filtering
;; https://emacs.stackexchange.com/questions/19664/hide-items-with-a-certain-tag-in-agenda-based-on-time-of-day
;;;###autoload
(defun skip-tag (tag)
  (let* ((next-headline (save-excursion
                          (or (outline-next-heading) (point-max))))
        (current-headline (or (and (org-at-heading-p)
                                   (point))
                              (save-excursion (org-back-to-heading))))
        )
    (if (member tag (org-get-tags-at current-headline))
            next-headline
            nil)))
;;;###autoload
(defun skip-not-tag (tag)
  (let* ((next-headline (save-excursion
                          (or (outline-next-heading) (point-max))))
        (current-headline (or (and (org-at-heading-p)
                                   (point))
                              (save-excursion (org-back-to-heading))))
        )
    (if (not (member tag (org-get-tags-at current-headline)))
            next-headline
            nil)))


;; custom function to convert pdf to image
;; source: https://emacs.stackexchange.com/questions/390/display-pdf-images-in-org-mode

;;;###autoload
(defun org-include-img-from-pdf (&rest _)
  "Convert pdf files to image files in org-mode bracket links.

    # ()convertfrompdf:t # This is a special comment; tells that the upcoming
                         # link points to the to-be-converted-to file.
    # If you have a foo.pdf that you need to convert to foo.png, use the
    # foo.png file name in the link.
    [[./foo.png]]
"
  (interactive)
  (if (executable-find "convert")
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\s-+()convertfrompdf\\s-*:\\s-*t"
                                  nil :noerror)
          ;; Keep on going to the next line till it finds a line with bracketed
          ;; file link.
          (while (progn
                   (forward-line 1)
                   (not (looking-at org-bracket-link-regexp))))
          ;; Get the sub-group 1 match, the link, from `org-bracket-link-regexp'
          (let ((link (match-string-no-properties 1)))
            (when (stringp link)
              (let* ((imgfile (expand-file-name link))
                     (pdffile (expand-file-name
                               (concat (file-name-sans-extension imgfile)
                                       "." "pdf")))
                     (cmd (concat "convert -density 96 -quality 85 "
                                  pdffile " " imgfile)))
                (when (and (file-readable-p pdffile)
                           (file-newer-than-file-p pdffile imgfile))
                  ;; This block is executed only if pdffile is newer than
                  ;; imgfile or if imgfile does not exist.
                  (shell-command cmd)
                  (message "%s" cmd)))))))
    (user-error "`convert' executable (part of Imagemagick) is not found")))



(provide 'org)
;;; org.el ends here
