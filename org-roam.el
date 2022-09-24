;;; org-roam.el --- org related config -*- lexical-binding: t; -*-

(use-package org-roam
  :custom
  (org-roam-directory "~/Dropbox/org/roam")
  ;; (org-roam-complete-everywhere t)
  (setq org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("r" "bibliography reference" plain "%?
%^{author}, %^{date}"
      :target
      (file+head "references/notes/${citekey}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)))
  ;; :bind (("C-c n l" . org-roam-buffer-toggle)
  ;; ("C-c n f" . org-roam-node-find)
  ;; ("C-c n i" . org-roam-node-insert))
  :init
  (setq org-roam-v2-ack t)
  :config
  (org-roam-setup))



(use-package consult-org-roam
  :ensure t
  :init
  (require 'consult-org-roam)
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep))

;;; ###autoload
(defun jds/consult-org-roam-and-agenda (&optional match)
  "Like consult-org-agenda but also search org-roam directory."
  (interactive)
  (unless org-agenda-files
    (user-error "No agenda files"))
  (unless org-roam-directory
    (user-error "org-roam-directory is not set"))
  (let* ((files (org-agenda-files))
	 (files (cl-union files
			  (directory-files-recursively org-roam-directory "\\.org$"))))
    (consult-org-heading match files)))

(use-package citar-org-roam
  :after citar org-roam
  :no-require
  :config
  (citar-org-roam-mode)
  (setq citar-org-roam-subdir "references/notes")
  )

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
  "l," #'org-roam-capture
  "lr" #'org-roam-refile
  "lR" #'org-roam-link-replace-all
  "la" #'org-roam-alias-add
  "lA" #'org-roam-alias-remove
  "lt" #'org-roam-tag-add
  "lT" #'org-roam-tag-remove
  "lo" #'org-roam-ref-add
  "lO" #'org-roam-ref-remove)
