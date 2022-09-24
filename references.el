;;; references.el --- summary -*- lexical-binding: t -*-



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
  (org-cite-global-bibliography '("~/Dropbox/org/roam/references/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  :config
  (setq citar-library-paths '("~/Dropbox/org/roam/references/articles/")))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

;;; setup biblio
(use-package biblio
  :after (:any latex org))

(use-package zotra
  :straight (zotra :type git :host github :repo "mpedramfar/zotra"))


;;; setup ebib -----------------------------------------------------------------

(use-package ebib
  :config
  (setq ebib-preload-bib-files org-cite-global-bibliography)
  ;; don't take up full frame on startup
  (setq ebib-layout 'window
	ebib-file-search-dirs '("~/Dropbox/org/roam/references/articles/"))
  ;; make emacs default pdf reader
  (setq ebib-file-associations
	'(("pdf" . nil)
	  ("ps" . nil)))
  ;; bigger index window at start
  (setq ebib-index-window-size 20))

;; setup with org-roam notes
;;;###autoload
(defun jds/ebib-popup-note (key)
    (interactive (list (ebib--get-key-at-point)))
    (citar-org-roam--create-capture-note key key)
    ;; (orb-edit-note key) ;; didn't like this template, gave terrible file names
    )

;; override ebib-popup-note to use org-roam-bibtex
(general-def
  :keymaps 'ebib-index-mode-map
  "N" 'jds/ebib-popup-note)


;;; download papers ------------------------------------------------------------

(use-package scihub
  :straight (scihub :type git :host github :repo "emacs-pe/scihub.el")
  :init
  (setq scihub-download-directory "~/Dropbox/org/roam/references/articles/"))

;; ;;;###autoload
;; (defun jds/download-from-scihub ()
;;     "foo"
;;   (interactive)
;;   (let* ((key (ebib--get-key-at-point))
;; 	 (doi (ebib-get-field-value "doi" key ebib--cur-db 'no-error 'unbraced)))
;;     )
;;   )

;;; bindings -------------------------------------------------------------------

(setq jds/citation-map (make-sparse-keymap))

(general-define-key
 :keymaps 'jds/citation-map
 "c" #'citar-insert-citation
 ;; "r" #'jds/literate-bib-tangle-and-refresh
 ;; "o" #'jds/goto-global-org-bib
 ;; "i" #'jds/literate-bib-search
 "s" #'biblio-lookup
 "u" #'zotra-add-entry-from-url
 "S" #'zotra-add-entry-from-search
 ;; "s" #'org-bib-new-from-doi
 )

(jds/localleader-def
  :keymaps '(org-mode-map LaTeX-mode-map)
  "c" jds/citation-map)


