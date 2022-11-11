;;; org-roam.el --- org related config -*- lexical-binding: t; -*-

(use-package org-roam
  :custom
  (org-roam-directory "~/Dropbox/org/roam")
  ;; (org-roam-complete-everywhere t)
  ;; :bind (("C-c n l" . org-roam-buffer-toggle)
  ;; ("C-c n f" . org-roam-node-find)
  ;; ("C-c n i" . org-roam-node-insert))
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
	   :unnarrowed t)
	  ("l" "lecturenotes" plain "%?"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: %u ${title}\n#+date: %U\n#+filetags: :lecture:\n")
	   :unnarrowed t
	   :jump-to-captured t)
	  ("r" "bibliography reference" plain
	   (file "~/.emacs.d/capture-templates/org-roam-bibtex-noter-template.org")
	   :target
	   (file+head "references/notes/${citekey}.org" "#+title: ${author-abbrev} :: ${title}\n#+filetags: :reference:\n")
	   :unnarrowed t))	  )

  ;; get tags when searching
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:200} " (propertize "${tags:30}" 'face 'org-tag)))

  ;; exclude from database all headlines with tag "ROAM_EXCLUDE"
  (setq org-roam-db-node-include-function
	(lambda ()
	  (not (member "ROAM_EXCLUDE" (org-get-tags)))))

  (org-roam-setup))

;;;###autoload
(defun jds~org-move-beyond-header ()
  "Move point down a line until at a line that doesn't start with \"#+\"."
  (while (string-match "^#\+" (thing-at-point 'line))
    (next-line)))

;; the follow is heavily inspired by org-super-links-insert-relatedlink
;;;###autoload
(defun jds/org-roam-insert-quick-link (&optional filter-fn &key templates info)
    "Insert org-roam-node Link into RELATED drawer. See org-roam-node-insert for optional argument documentation."
    (interactive)
    (save-excursion
      (let* ((node (org-roam-node-at-point 'assert)))
	(goto-char (org-roam-node-point node))
	(org-roam-end-of-meta-data)
	(jds~org-move-beyond-header)
	(let* ((drawer-beg-regexp (concat "^[ \t]*:" (downcase "RELATED") ":[ \t]*$"))
	       (drawer-end-regexp "^[ \t]*:end:[ \t]*$")
	       (bound (save-excursion
			(if (search-forward-regexp org-heading-regexp nil t)
			    (line-beginning-position)
			  (buffer-end 1))))
	       (beg)
	       (end))
	  (when (search-forward-regexp drawer-beg-regexp bound t)
	    (setq beg (line-beginning-position))
	    (goto-char beg)
	    (when (search-forward-regexp drawer-end-regexp bound t)
	      (setq end (line-end-position))))
	  (if (and (not (null beg))
		   (not (null end)))
	      (progn			; drawer already  present
		(goto-char end)
		;; (previous-line)
		(end-of-line 0)
		(open-line 1)
		(next-line)
		(insert "- ")
		(org-roam-node-insert filter-fn :templates templates :info info))
	    (org-insert-drawer nil "RELATED")
	    (insert "- ")
	    (org-roam-node-insert filter-fn :templates templates :info info))))))

;;;###autoload
(defun jds/org-roam-exclude-node ()
  "Add tag ROAM_EXCLUDE to node."
  (interactive)
  (org-roam-tag-add '("ROAM_EXCLUDE")))


;; tools
;; org-roam-end-of-meta-data
;; org-roam-up-heading-or-point-min

(use-package consult-org-roam
  :ensure t
  :diminish consult-org-roam-mode
  :init
  (require 'consult-org-roam)
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep))

;;; ###autoload
;; (defun jds/consult-org-roam-and-agenda (&optional match)
;;   "Like consult-org-agenda but also search org-roam directory."
;;   (interactive)
;;   (unless org-agenda-files
;;     (user-error "No agenda files"))
;;   (unless org-roam-directory
;;     (user-error "org-roam-directory is not set"))
;;   (let* ((files (org-agenda-files))
;; 	 (files (cl-union files
;; 			  (directory-files-recursively org-roam-directory "\\.org$"))))
;;     (consult-org-heading match files)))


(use-package citar-org-roam
  :after citar org-roam org-roam-bibtex
  :diminish citar-org-roam-mode
  :no-require
  :config
  (citar-org-roam-mode)
  (setq citar-org-roam-subdir "references/notes")

  (require 'citar-org-roam)
  (citar-register-notes-source
   'orb-citar-source (list :name "Org-Roam Notes"
			   :category 'org-roam-node
			   :items #'citar-org-roam--get-candidates
			   :hasitems #'citar-org-roam-has-notes
			   :open #'citar-org-roam-open-note
			   :create #'orb-citar-edit-note
			   :annotate #'citar-org-roam--annotate))

  (setq citar-notes-source 'orb-citar-source))

(use-package org-roam-bibtex
  :after org-roam
  :after citar
  :diminish org-roam-bibtex-mode
  :config

  (setq orb-preformat-keywords
	'("citekey" "title" "url" "author-or-editor" "keywords" "file" "keywords" "date" "note?" "author" "editor" "author-abbrev")
	orb-process-file-keyword nil
	orb-attached-file-extensions '("pdf" "epub" "html")
	orb-roam-ref-format 'org-cite)

  (org-roam-bibtex-mode))

(use-package org-noter
  :config
  (setq org-noter-notes-search-path '("~/Dropbox/org/roam/references/notes/")
	org-noter-always-create-frame nil
	org-noter-kill-frame-at-session-end nil))
(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(general-def
  :keymaps 'org-noter-doc-mode-map
  "M-i" #'org-noter-insert-note
  "M-I" #'org-noter-pdftools-insert-precise-note)

(jds/localleader-def
  :keymaps 'org-mode-map
  "ll" #'org-roam-buffer-toggle
  "lf" #'org-roam-node-find
  "li" #'org-roam-node-insert
  "lI" #'jds/org-roam-insert-quick-link
  "l," #'org-roam-capture
  "lr" #'org-roam-refile
  "lR" #'org-roam-link-replace-all
  "la" #'org-roam-alias-add
  "lA" #'org-roam-alias-remove
  "lt" #'org-roam-tag-add
  "le" #'jds/org-roam-exclude-node
  "lT" #'org-roam-tag-remove
  "lo" #'org-roam-ref-add
  "lO" #'org-roam-ref-remove)
