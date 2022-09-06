;;; references.el --- summary -*- lexical-binding: t -*-

(setq jds/global-literate-bib "~/Dropbox/org/references.org")


(use-package citar
  :after (:any latex org)
  :config
  (setq citar-select-multiple t)
  
  ;; nice file icons
  (setq citar-symbols
	`((file . (,(all-the-icons-icon-for-file "foo.pdf" :face 'all-the-icons-dred) .
		   ,(all-the-icons-icon-for-file "foo.pdf" :face 'citar-icon-dim)))
	  (note . (,(all-the-icons-icon-for-file "foo.txt") .
		   ,(all-the-icons-icon-for-file "foo.txt" :face 'citar-icon-dim)))
	  (link .
		(,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'all-the-icons-dpurple) .
		 ,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'citar-icon-dim)))))
  ;; Here we define a face to dim non 'active' icons, but preserve alignment
  (defface citar-icon-dim
    '((((background dark)) :foreground "#282c34")
      (((background light)) :foreground "#fafafa"))
    "Face for obscuring/dimming icons"
    :group 'all-the-icons-faces)
  :custom
  (org-cite-global-bibliography '("~/Dropbox/org/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography))

;;; setup biblio
(use-package biblio
  :after (:any latex org))

;; org-bib for literate bibtex
;; (use-package org-bib
;;   :straight `(org-bib :type git :host github :repo "jsilve24/org-bib-mode")
;;   ;; :straight `(org-bib :type git :host github :repo "rougier/org-bib-mode" :branch "org-imenu")
;;   :after (:any latex org)
;;   :config
;;   (setq org-bib-library-paths '("~/Dropbox/org/papers/")
;; 	org-bib-default-library "~/Dropbox/org/references.org"
;; 	org-bib-unsorted-header "Unsorted"
;; 	org-bib-url-icon "|URL|"
;; 	org-bib-doi-icon "|DOI|"
;; 	org-bib-filename-icon "|FILE|"))

;; (use-package pdf-drop-mode
;;   :straight (pdf-drop-mode :type git :host github :repo "rougier/pdf-drop-mode"))

;; ;;;###autoload
;; (defun jds/literate-bib-tangle-and-refresh ()
;;   "Tangle citarlit-global-literate-bib and refresh citar."
;;   (interactive)
;;   (org-babel-tangle-file jds/global-literate-bib)
;;   (citar-refresh))

;; ;;;###autoload
;; (defun jds/goto-global-org-bib ()
;;   "Open new org-buffer for the "
;;   (interactive)
;;   (find-file jds/global-literate-bib))

;; ;;;###autoload
;; (defun jds/literate-bib-search ()
;;   "Open new org-buffer for the "
;;   (interactive)
;;   (if (get-file-buffer jds/global-literate-bib)
;;       nil
;;     (progn
;;       (find-file jds/global-literate-bib)
;;       (bury-buffer)))
;;   (with-current-buffer (get-file-buffer jds/global-literate-bib)
;;     (consult-outline)))


(setq jds/citation-map (make-sparse-keymap))

(general-define-key
 :keymaps 'jds/citation-map
 "c" #'citar-insert-citation
 ;; "r" #'jds/literate-bib-tangle-and-refresh
 ;; "o" #'jds/goto-global-org-bib
 ;; "i" #'jds/literate-bib-search
 "s" #'biblio-lookup
 ;; "s" #'org-bib-new-from-doi
 )

(jds/localleader-def
  :keymaps '(org-mode-map LaTeX-mode-map)
  "c" jds/citation-map)

;; (use-package citar-literate
;;   :straight (citar-literate :local-repo "~/.emacs.d/local-packages/citar-literate/")
;;   :config
;;   (setq citarlit-global-literate-bib "~/Dropbox/org/references.org"))


