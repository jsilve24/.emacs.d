;;; references.el --- summary -*- lexical-binding: t -*-

(use-package bibtex-completion
  :straight t
  :defer t
  :config
  (setq bibtex-completion-bibliography '("~/Dropbox/org/roam/references/references.bib")
	;; this is used by org-roam-bibtex note actions
	bibtex-completion-library-path '("~/Dropbox/org/roam/references/articles/")))

(use-package citar
  :after (:any latex org)
  :config
  (setq citar-select-multiple t)

  ;; nice file icons
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (all-the-icons-icon-for-file "foo.pdf" :face 'all-the-icons-dred)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))

  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (all-the-icons-octicon
	      "link"
	      :face 'all-the-icons-orange
	      :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))

  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (all-the-icons-icon-for-file "foo.txt")
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))

  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon
	      "circle-o"
	      :face 'all-the-icon-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))

  (setq citar-indicators
	(list citar-indicator-files-icons
	      citar-indicator-links-icons
	      citar-indicator-notes-icons
	      citar-indicator-cited-icons))

  ;; (setq citar-symbols
  ;; 	`((file . (,(all-the-icons-icon-for-file "foo.pdf" :face 'all-the-icons-dred) .
  ;; 		   ,(all-the-icons-icon-for-file "foo.pdf" :face 'citar-icon-dim)))
  ;; 	  (note . (,(all-the-icons-icon-for-file "foo.txt") .
  ;; 		   ,(all-the-icons-icon-for-file "foo.txt" :face 'citar-icon-dim)))
  ;; 	  (link .
  ;; 		(,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'all-the-icons-dpurple) .
  ;; 		 ,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'citar-icon-dim)))))
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
  (org-cite-export-processors
   '((latex biblatex)
     (t csl)))				; Fallback
  :config
  (require 'oc)
  (require 'oc-biblatex)
  (require 'oc-csl)
  (setq citar-library-paths '("~/Dropbox/org/roam/references/articles/")
	citar-file-additional-files-separator "-"))


(use-package citar-embark
  :after citar embark
  :diminish citar-embark-mode
  :no-require
  :config (citar-embark-mode))

;;; setup biblio
(use-package biblio
  :after (:any latex org))

(use-package zotra
  :straight (zotra :type git :host github :repo "mpedramfar/zotra")
  :commands (zotra-add-entry-from-search)
  :config
  (setq zotra-default-bibliography "/home/jds6696/Dropbox/org/roam/references/references.bib"
	;; todo could upgrade to the zotra-cli which can also download papers
	zotra-backend 'translation-server)
  (add-hook 'zotra-after-add-entry-hook (lambda ()
					  (bibtex-clean-entry t)
					  (bibtex-sort-buffer)))
  (defvar jds~bibtex-clean-latest-stored-key nil)
  (defun jds~bibtex-store-latest-key-hook-function ()
    "Function to be added to bibtex-clean-entry-hook and run after cleaning in the cleaned entry."
    (setq jds~bibtex-clean-latest-stored-key (bibtex-completion-get-key-bibtex)))
  (add-hook 'bibtex-clean-entry-hook 'jds~bibtex-store-latest-key-hook-function))

(use-package pdf-drop-mode
  :straight (pdf-drop-mode :type git :host github :repo "rougier/pdf-drop-mode")
  :config
  ;; this function zotra-get-json signals error if nothing found, need to handle these errors before trying to then merge pdf
  (defun jds/pdf-drop-mode-call-zotra (file doi)
    (let ((doi-string (if (listp doi)
			  (cdr doi)
			doi)))
      (zotra-add-entry-from-search doi-string)))
  (setq pdf-drop-search-hook #'jds/pdf-drop-mode-call-zotra))

;;;###autoload
(defun jds/pdf-drop-process ()
    "Call pdf-drop--process at point."
  (interactive)
  (let ((filename (dired-copy-filename-as-kill 0)))
    ;; with tmp hook on bibtex-clean-entry move current file and add as field 
    (pdf-drop--process filename)
    (jds/dired-add-file-to-bib jds~bibtex-clean-latest-stored-key filename)))

;;;###autoload
(defun jds/dired-add-file-to-bib (key file)
    "Attach file to bibtex entry with KEY located in global
bibliography. Moves file into global file library. FILE should be
a string specifying full filepath."
    (interactive (list  (car (citar-select-refs :multiple nil))
			(dired-get-filename)))
    (let* ((destname (concat (car citar-library-paths) key (file-name-extension file t)))
	   (destfile (file-name-nondirectory destname)))
      (rename-file file destname)
      (save-excursion
	(citar-open-entry key)
	(bibtex-make-field `("file" "File to Attach" ,destfile) t))))

;;bibtex-clean-entry-hook cleaned after clea
;; (bibtex-completion-get-key-bibtex)

(jds/localleader-def
  :keymaps 'dired-mode-map
  "r" #'jds/pdf-drop-process
  "f" #'jds/dired-add-file-to-bib)

;;; setup ebib -----------------------------------------------------------------

;; (straight-use-package '(ebib :includes (ebib org-ebib)))
;; (use-package ebib
;;   :config
;;   (setq ebib-preload-bib-files org-cite-global-bibliography)
;;   ;; don't take up full frame on startup
;;   (setq ebib-layout 'window
;; 	ebib-file-search-dirs '("~/Dropbox/org/roam/references/articles/"))
;;   ;; make emacs default pdf reader
;;   (setq ebib-file-associations
;; 	'(("pdf" . nil)
;; 	  ("ps" . nil)))
;;   ;; bigger index window at start
;;   (setq ebib-index-window-size 20)
;;   (require 'org-ebib))

;;;###autoload
(defun jds/citar-org-roam--create-capture-note (citekey entry)
    "Open or create org-roam node for CITEKEY and ENTRY."
   ;; adapted from https://jethrokuan.github.io/org-roam-guide/#orgc48eb0d
    (let ((title (citar-format--entry
                   citar-org-roam-note-title-template entry)))
     (org-roam-capture-
      :templates
      '(("r" "reference" plain "%?" :if-new
         (file+head
          "%(concat
 (when citar-org-roam-subdir (concat citar-org-roam-subdir \"/\")) \"${citekey}.org\")"
          "#+title: ${title}\n Ebib:%a\n")
         :immediate-finish t
         :unnarrowed t))
      :info (list :citekey citekey)
      :node (org-roam-node-create :title title)
      :props '(:finalize find-file))
     (org-roam-ref-add (concat "@" citekey))))

;; (with-eval-after-load 'citar-org-roam
;;   (setq citar-org-roam-notes-config (plist-put citar-org-roam-notes-config :create #'jds/citar-org-roam--create-capture-note)))

;; setup with org-roam notes
;; ###autoload
;; (defun jds/ebib-popup-note (key)
;;   (interactive (list (ebib--get-key-at-point)))
;;   (org-ebib-store-link)
;;   (jds/citar-org-roam--create-capture-note key key)
;;   (org-ebib-insert-link)
;;   ;; (orb-edit-note key) ;; didn't like this template, gave terrible file names
;;   )

;; override ebib-popup-note to use org-roam-bibtex
;; (general-def
;;   :keymaps 'ebib-index-mode-map
;;   "N" 'jds/ebib-popup-note)

;; configure bibtex-generate-autokey
(use-package emacs
  :config
  (setq bibtex-autokey-year-length 4
	bibtex-autokey-titlewords 1
	bibtex-autokey-titleword-separator ""
	bibtex-autokey-year-title-separator ""))


;;; download papers ------------------------------------------------------------

(use-package scihub
  :straight (scihub :type git :host github :repo "emacs-pe/scihub.el")
  :init
  (setq scihub-download-directory "~/Dropbox/org/roam/references/articles/"))

;; ###autoload
;; (defun jds/ebib-download-from-scihub ()
;;     "From ebib, download paper. Entry must have DOI or DOI URL field."
;;   (interactive)
;;   (let* ((key (ebib--get-key-at-point))
;; 	 (doi (ebib-get-field-value "doi" key ebib--cur-db 'no-error 'unbraced))
;; 	 (filename (concat key ".pdf"))
;; 	 (filepath (concat scihub-download-directory filename)))
;;     (scihub doi filepath)
;;     (ebib-set-field-value "file" filename key ebib--cur-db)
;;     (ebib--set-modified t ebib--cur-db t (seq-filter (lambda (dependent)
;;                                                            (ebib-db-has-key key dependent))
;;                                                      (ebib--list-dependents ebib--cur-db)))
;;     (ebib--update-entry-buffer)))

;;; bindings -------------------------------------------------------------------

(setq jds/citation-map (make-sparse-keymap))

(general-define-key
 :keymaps 'jds/citation-map
 "c" #'jds/citar-insert-cite-prioritize-local-bib
 "p" #'citar-org-update-pre-suffix
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


