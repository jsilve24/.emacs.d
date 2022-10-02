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
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: %u ${title}\n#+date: %U\n#+filetags: :lecturenotes:\n")
	   :unnarrowed t
	   :jump-to-captured t)
	  ("r" "bibliography reference" plain "%?
%^{author}, %^{date}"
	   :target
	   (file+head "references/notes/${citekey}.org" "#+title: ${title}\n")
	   :unnarrowed t)))

  ;; get tags when searching
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:30}" 'face 'org-tag)))

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
	(org-insert-drawer nil "RELATED")
	(insert "- ")
	(org-roam-node-insert filter-fn :templates templates  :info info))))

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
  :after citar org-roam
  :no-require
  :config
  (citar-org-roam-mode)
  (setq citar-org-roam-subdir "references/notes"))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (setq orb-roam-ref-format 'org-cite
	orb-process-file-keyword t)
  (org-roam-bibtex-mode))


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
