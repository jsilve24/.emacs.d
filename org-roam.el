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
  ;; better export for org-roam files
  (require 'org-roam-export)
  (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+STARTUP: latexpreview\n")
	   :unnarrowed t)
	  ("w" "wiki" plain "%?"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: :wiki:\n#+STARTUP: latexpreview\n")
	   :unnarrowed t)
	  ("c" "cookbook" plain "%?"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: :cookbook:\n#+STARTUP: latexpreview\n")
	   :unnarrowed t)
	  ("l" "lecturenotes" plain "%?"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: %u ${title}\n#+date: %U\n#+filetags: :lecture:\n#+STARTUP: latexpreview\n")
	   :unnarrowed t
	   :jump-to-captured t)
	  ("r" "bibliography reference" plain
	   (file "~/.emacs.d/capture-templates/org-roam-bibtex-noter-template.org")
	   :target
	   (file+head "references/notes/${citekey}.org" "#+title: ${author-abbrev} :: ${title}\n#+filetags: :reference:\n#+STARTUP: latexpreview\n")
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

(use-package org-drawer-list
  :straight (org-drawer-list :type git :host github :repo "d12frosted/org-drawer-list"))

;;;###autoload
(defun jds/add-to-references-drawer ()
  "Adds references drawer under org-headline or adds new list
item to existing references drawer."
  (interactive)
  (let* ((range (org-drawer-list-block "REFERENCES" t t))
	 (end (cdr range)))
    (goto-char end)
    (open-line 1)
    (insert "- ")))

(use-package consult-org-roam
  :ensure t
  :diminish consult-org-roam-mode
  :init
  (require 'consult-org-roam)
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep))

(use-package org-roam-ui
  :diminish org-roam-ui-mode
  :diminish org-roam-ui-follow-mode
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;;; ###autoload
;; (defun jds/consult-org-roam-and-agenda (&optional match)
;; "Like consult-org-agenda but also search org-roam directory."
;; (interactive)
;; (unless org-agenda-files
;; (user-error "No agenda files"))
;; (unless org-roam-directory
;; (user-error "org-roam-directory is not set"))
;; (let* ((files (org-agenda-files))
;; (files (cl-union files
;; (directory-files-recursively org-roam-directory "\\.org$"))))
;; (consult-org-heading match files)))


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
  (diminish 'org-roam-bibtex-mode)

  (setq orb-preformat-keywords
	'("citekey" "title" "url" "author-or-editor" "keywords" "file" "keywords" "date" "note?" "author" "editor" "author-abbrev")
	orb-process-file-keyword nil
	orb-attached-file-extensions '("pdf" "epub" "html")
	orb-roam-ref-format 'org-cite)

  (org-roam-bibtex-mode))

(use-package org-noter
  :config
  ;; (evil-collection-define-key 'normal 'org-noter-notes-mode-map
  ;; (kbd "C-.") #'org-noter-sync-current-note)
  (defun jds~org-noter-bindings ()
    (evil-local-set-key 'normal (kbd "C-.") #'org-noter-sync-current-note))
  (add-hook 'org-noter-notes-mode-hook #'jds~org-noter-bindings)
  (setq org-noter-notes-search-path '("~/Dropbox/org/roam/references/notes/")
	org-noter-always-create-frame nil
	org-noter-kill-frame-at-session-end nil)


  ;; fix bug in org-noter: https://github.com/weirdNox/org-noter/issues/125
(defun org-noter-kill-session (&optional session)
  "Kill an `org-noter' session.

When called interactively, if there is no prefix argument and the
buffer has an annotation session, it will kill it; else, it will
show a list of open `org-noter' sessions, asking for which to
kill.

When called from elisp code, you have to pass in the SESSION you
want to kill."
  (interactive "P")
  (when (and (called-interactively-p 'any) (> (length org-noter--sessions) 0))
    ;; NOTE(nox): `session' is representing a prefix argument
    (if (and org-noter--session (not session))
        (setq session org-noter--session)
      (setq session nil)
      (let (collection default doc-display-name notes-file-name display)
        (dolist (session org-noter--sessions)
          (setq doc-display-name (org-noter--session-display-name session)
                notes-file-name (file-name-nondirectory
                                 (org-noter--session-notes-file-path session))
                display (concat doc-display-name " - " notes-file-name))
          (when (eq session org-noter--session) (setq default display))
          (push (cons display session) collection))
        (setq session (cdr (assoc (completing-read "Which session? " collection nil t
                                                   nil nil default)
                                  collection))))))

  (when (and session (memq session org-noter--sessions))
    (setq org-noter--sessions (delq session org-noter--sessions))

    (when (eq (length org-noter--sessions) 0)
      (remove-hook 'delete-frame-functions 'org-noter--handle-delete-frame)
      (advice-remove 'doc-view-goto-page 'org-noter--location-change-advice)
      (advice-remove 'nov-render-document 'org-noter--nov-scroll-handler))

    (let* ((ast (org-noter--parse-root session))
	   (frame (org-noter--session-frame session))
	   (notes-buffer (org-noter--session-notes-buffer session))
	   (base-buffer (buffer-base-buffer notes-buffer))
	   (notes-modified (buffer-modified-p base-buffer))
	   (doc-buffer (org-noter--session-doc-buffer session)))

      (dolist (window (get-buffer-window-list notes-buffer nil t))
	(with-selected-frame (window-frame window)
	  (if (= (count-windows) 1)
	      (when (org-noter--other-frames) (delete-frame))
	    (delete-window window))))

      (with-current-buffer notes-buffer
	(remove-hook 'kill-buffer-hook 'org-noter--handle-kill-buffer t)
	(restore-buffer-modified-p nil))
      (kill-buffer notes-buffer)

      (with-current-buffer base-buffer
	(org-noter--unset-text-properties ast)
	(set-buffer-modified-p notes-modified))

      (with-current-buffer doc-buffer
	(remove-hook 'kill-buffer-hook 'org-noter--handle-kill-buffer t))
      (if (not org-noter-kill-frame-at-session-end) (set-window-dedicated-p (get-buffer-window doc-buffer) nil))
      (kill-buffer doc-buffer)

      (when (frame-live-p frame)
	(if (and (org-noter--other-frames) org-noter-kill-frame-at-session-end)
	    (delete-frame frame)
	  (progn
	    (delete-other-windows)
	    (set-frame-parameter nil 'name nil)))))))
  )

;; (general-define-key
;;  :keymaps 'org-noter-notes-mode-map
;;  :states '(n m v e)
;;  "C-." #'org-noter-sync-current-note)

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link)
  :config
  ;; As a quick workaround until the corresponding lines in org-pdftools are
  ;; fixed, you can simply add this to your configuration:
  ;; https://github.com/fuxialexander/org-pdftools/issues/110
  (defalias 'find-if 'cl-find-if)
  (defalias 'getf 'cl-getf))

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
  "lc" #'jds/add-to-references-drawer
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
