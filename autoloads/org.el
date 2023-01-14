;;; org.el --- org functions -*- lexical-binding: t; -*-

;;;###autoload
(defun jds/org-agenda-show-custom-day (&optional arg)
  (interactive "P")
  (progn
    (org-agenda arg "d")
    (evil-goto-first-line)))


;;;###autoload
(defun jds/open-custom-day-agenda-new-frame ()
  (interactive)
  (jds~new-frame-or-new-window)
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
;; how to get day of week to use in this function in the future: (calendar-day-of-week (calendar-current-date))
;;;###autoload
(defun skip-tag (tag)
  (let* ((next-headline (save-excursion
                          (or (outline-next-heading) (point-max))))
        (current-headline (or (and (org-at-heading-p)
                                   (point))
                              (save-excursion (org-back-to-heading)))))
    (if (member tag (org-get-tags-at current-headline))
            next-headline
            nil)))

;;;###autoload
(defun skip-not-tag (tag)
  (let* ((next-headline (save-excursion
                          (or (outline-next-heading) (point-max))))
        (current-headline (or (and (org-at-heading-p)
                                   (point))
                              (save-excursion (org-back-to-heading)))))
    (if (not (member tag (org-get-tags-at  current-headline)))
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


;;; insert-item stolen from doom
(defun +org--insert-item (direction)
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       ;; Position determines where org-insert-todo-heading and org-insert-item
       ;; insert the new list item.
       (if (eq direction 'above)
           (org-beginning-of-item)
         (org-end-of-item)
         (backward-char))
       (org-insert-item (org-element-property :checkbox context))
       ;; Handle edge case where current item is empty and bottom of list is
       ;; flush against a new heading.
       (when (and (eq direction 'below)
                  (eq (org-element-property :contents-begin context)
                      (org-element-property :contents-end context)))
         (org-end-of-item)
         (org-end-of-line)))

      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (save-excursion (org-table-insert-row t))
                 (org-table-next-row))
         ('above (save-excursion (org-shiftmetadown))
                 (+org/table-previous-row))))

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
         (pcase direction
           (`below
            (let (org-insert-heading-respect-content)
              (goto-char (line-end-position))
              (org-end-of-subtree)
              (insert "\n" (make-string level ?*) " ")))
           (`above
            (org-back-to-heading)
            (insert (make-string level ?*) " ")
            (save-excursion (insert "\n"))))
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo
            (cond ((eq todo-type 'done)
                   ;; Doesn't make sense to create more "DONE" headings
                   (car (+org-get-todo-keywords-for todo-keyword)))
                  (todo-keyword)
                  ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))

;;;###autoload
(defun +org-get-todo-keywords-for (&optional keyword)
  "Returns the list of todo keywords that KEYWORD belongs to."
  (when keyword
    (cl-loop for (type . keyword-spec)
             in (cl-remove-if-not #'listp org-todo-keywords)
             for keywords =
             (mapcar (lambda (x) (if (string-match "^\\([^(]+\\)(" x)
                                     (match-string 1 x)
                                   x))
                     keyword-spec)
             if (eq type 'sequence)
             if (member keyword keywords)
             return keywords)))

;; I use these instead of `org-insert-item' or `org-insert-heading' because they
;; impose bizarre whitespace rules depending on cursor location and many
;; settings. These commands have a much simpler responsibility.
;;;###autoload
(defun +org/insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'below)))

;;;###autoload
(defun +org/insert-item-above (count)
  "Inserts a new heading, table cell or item above the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'above)))


;;; Table stuff stolen from doom

;;;###autoload
(defun +org/table-previous-row ()
  "Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')"
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
           org-table-may-need-update)
      (org-table-align))
  (let ((col (org-table-current-column)))
    (beginning-of-line 0)
    (when (or (not (org-at-table-p)) (org-at-table-hline-p))
      (beginning-of-line))
    (org-table-goto-column col)
    (skip-chars-backward "^|\n\r")
    (when (org-looking-at-p " ")
      (forward-char))))


;; Row/Column insertion

;;;###autoload
(defun +org/table-insert-column-left ()
  "Insert a new column left of the current column."
  (interactive)
  (org-table-insert-column)
  (org-table-move-column-left))

;;;###autoload
(defun +org/table-insert-row-below ()
  "Insert a new row below the current row."
  (interactive)
  (org-table-insert-row 'below))


;;;###autoload
(defun jds/convert-zoom-url-to-org-link ()
  "Convert a zoom url to a shorted org-link for agendas"
  (interactive)
  (save-excursion 
    (goto-char 1)
    (while (re-search-forward "https?://[a-z]+\.zoom\.us/[^\n\s]*" nil t)
      (replace-match "[[\\&][(ZOOM)]]" nil nil))))

;;;###autoload
;; from here: https://emacs.stackexchange.com/questions/10597/how-to-refile-into-a-datetree
;; modified for month based date-tree rather than day
(defun jds/org-refile-to-month-datetree (&optional file)
  "Refile a subtree to a datetree corresponding to it's timestamp.

The current time is used if the entry has no timestamp. If FILE
is nil, refile in the current file."
  (interactive "f")
  (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP_IA" t)
			    (org-read-date t nil "now")))
	 (date (org-date-to-gregorian datetree-date)))
    (with-current-buffer (current-buffer)
      (save-excursion
	(org-cut-subtree)
	(if file (find-file file))
	(org-datetree-find-month-create date)
	(org-narrow-to-subtree)
	(show-subtree)
	(org-end-of-subtree t)
	(newline)
	(goto-char (point-max))
	(org-paste-subtree 4)
	(widen))))
  )

;;;###autoload
(defun org-super-links-quick-insert-via ()
  (interactive)
  (let ((org-super-links-link-prefix "\nvia: "))
    (org-super-links-insert-link)))


;;;###autoload
(defun jds/super-link-at-point-capture ()
  "Start Org Capture, Afterwords return to point where capture was started and insert org-super-link to last stored capture."
  (interactive)
  (let ((point (point))
	(buffer (current-buffer)))
    (org-capture)
    (jds~add-hook-run-once 'org-capture-after-finalize-hook
			   (lambda ()
			     (org-capture-goto-last-stored)
			     (org-super-links-store-link)
			     (switch-to-buffer buffer)
			     (goto-char point)
			     (let ((org-super-links-backlink-into-drawer "VIA"))
			       (org-super-links-insert-link))))))




;; from here: https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link
(defun jds/org-replace-link-by-link-description ()
    "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2) 
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))


;;;###autoload
(defun jds/org-refile-current-buffer ()
    "Run org-refile but only suggest headings in the currently visited buffer"
  (interactive)
  (let ((org-refile-targets '((nil :maxlevel . 9)))
	(org-refile-use-outline-path t))
    (org-refile)))


;;;###autoload
(defun jds/scan-org-roam-for-org-id ()
  "Force Org to scan all agenda and roam files for org ids"
  (interactive)
  (unless org-agenda-files
    (user-error "No agenda files"))
  (unless org-roam-directory
    (user-error "org-roam-directory is not set"))
  (let* ((files (org-agenda-files))
	 (files (cl-union files
			  (mapcar (lambda (x) (file-name-concat org-roam-directory x))
				  (seq-remove (lambda (x) (or  (string-match "^.#" x)
							       (not (string-match "org$" x))))
					      (cl-remove-if (lambda (x) (member x '("." "..")))
							    (directory-files org-roam-directory)))))))
    (org-id-update-id-locations files)))


;;; autoloads
;;;###autoload
(defun jds/open-custom-day-agenda-new-frame ()
  (interactive)
  (select-frame (make-frame))
  (org-agenda nil "d"))
