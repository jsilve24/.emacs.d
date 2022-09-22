;;; org-roam.el --- org related config -*- lexical-binding: t; -*-

(use-package org-roam
  :custom
  (org-roam-directory "~/Dropbox/org/roam")
  ;; (org-roam-complete-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ))
  ;; :bind (("C-c n l" . org-roam-buffer-toggle)
  ;; ("C-c n f" . org-roam-node-find)
  ;; ("C-c n i" . org-roam-node-insert))
  :init
  (setq org-roam-v2-ack t)
  :config
  (org-roam-setup)

(use-package consult-org-roam
  :ensure t
  :init
  (require 'consult-org-roam)
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  )

;;;###autoload
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
  


  (jds/localleader-def
    :keymaps 'org-mode-map
    "nl" #'org-roam-buffer-toggle
    "nf" #'org-roam-node-find
    "ni" #'org-roam-node-insert
    "n," #'org-roam-capture
    "nr" #'org-roam-refile
    "nR" #'org-roam-link-replace-all
    "na" #'org-roam-alias-add
    "nA" #'org-roam-alias-remove
    "nt" #'org-roam-tag-add
    "nT" #'org-roam-tag-remove
    "no" #'org-roam-ref-add
    "nO" #'org-roam-ref-remove)
  )
