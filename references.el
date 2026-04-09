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
  (org-cite-global-bibliography '("/home/jds6696/Dropbox/org/roam/references/references.bib"))
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

;;; PDF text extraction — top-level so it's usable independently of pdf-drop-mode
;;;###autoload
(defun jds/pdf-extract-text-pages (file &optional max-pages)
  "Extract text from first MAX-PAGES (default 4) pages of FILE via pdftotext.
Returns empty string on failure."
  (condition-case nil
      (with-temp-buffer
        (call-process "pdftotext" nil t nil
                      "-f" "1" "-l" (number-to-string (or max-pages 4))
                      (expand-file-name file) "-")
        (buffer-string))
    (error "")))

(use-package pdf-drop-mode
  :straight (pdf-drop-mode :type git :host github :repo "rougier/pdf-drop-mode")
  :config
  ;; Track file awaiting async AI BibTeX insertion
  (defvar jds~pdf-ai-pending-file nil
    "Absolute path of PDF awaiting AI-based BibTeX lookup.")

  ;; Async callback: insert AI BibTeX into .bib file, then move the PDF
  (defun jds~pdf-ai-bibtex-callback (response info)
    "Handle gptel response: insert BibTeX into bibliography and move pending PDF."
    (if (not response)
        (progn
          (message "AI BibTeX lookup failed: %s" (plist-get info :status))
          (setq jds~pdf-ai-pending-file nil))
      (let* ((bib-start (and (string-match "@[a-zA-Z]+{" response)
                             (match-beginning 0)))
             (bibtex    (if bib-start (substring response bib-start) response))
             (pending   jds~pdf-ai-pending-file)
             (bib-file  zotra-default-bibliography))
        (setq jds~pdf-ai-pending-file nil)
        (condition-case err
            (progn
              (with-current-buffer (find-file-noselect bib-file)
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (let ((entry-start (point)))
                  (insert bibtex "\n")
                  (goto-char entry-start)
                  (bibtex-clean-entry t)   ; triggers jds~bibtex-store-latest-key-hook-function
                  (bibtex-sort-buffer)
                  (save-buffer)))
              (if (and pending jds~bibtex-clean-latest-stored-key)
                  (progn
                    (message "AI BibTeX added: %s" jds~bibtex-clean-latest-stored-key)
                    (jds/dired-add-file-to-bib jds~bibtex-clean-latest-stored-key pending))
                (message "AI BibTeX inserted; could not move file (no key).")))
          (error (message "Error processing AI BibTeX: %s" (error-message-string err)))))))

  ;; Called when DOI lookup fails or when no DOI is found at all
  (defun jds/pdf-drop-ai-bibtex-fallback (file title)
    "Use AI to identify paper in FILE and create a BibTeX entry.
TITLE is the PDF metadata title (may be nil or empty)."
    (setq jds~pdf-ai-pending-file (expand-file-name file))
    (message "DOI lookup failed; asking AI to identify paper...")
    (let* ((text   (jds/pdf-extract-text-pages file 4))
           (prompt (format "I need a BibTeX bibliography entry for this academic paper.\n%s\n\nText from the first pages:\n---\n%s\n---\n\nReturn ONLY a single complete BibTeX entry starting with @. No explanation, no markdown fences."
                           (if (and title (not (string-empty-p (string-trim (or title "")))))
                               (format "PDF metadata title: %s" title)
                             "")
                           text)))
      (gptel-request prompt
        :system "You are a bibliographic assistant. Return only a single complete BibTeX entry."
        :callback #'jds~pdf-ai-bibtex-callback)))

  ;; When DOI found but zotra lookup fails, fall back to AI
  (defun jds/pdf-drop-mode-call-zotra (file doi)
    "Call zotra for DOI lookup; fall back to AI if zotra fails."
    (let ((doi-string (if (listp doi) (cdr doi) doi)))
      (condition-case nil
          (zotra-add-entry-from-search doi-string)
        (error
         (jds/pdf-drop-ai-bibtex-fallback file doi-string)))))
  (setq pdf-drop-search-hook #'jds/pdf-drop-mode-call-zotra)

  ;; When no DOI at all is extracted, use AI instead of the default title prompt
  (when (boundp 'pdf-drop-title-hook)
    (setq pdf-drop-title-hook #'jds/pdf-drop-ai-bibtex-fallback)))

;;;###autoload
(defun jds/pdf-drop-process ()
  "Process PDF at point: look up BibTeX via DOI, fall back to AI if needed.
When the DOI path succeeds synchronously, moves the file immediately.
When the AI fallback is triggered (async), the callback moves the file."
  (interactive)
  (let ((filename (dired-copy-filename-as-kill 0)))
    (setq jds~bibtex-clean-latest-stored-key nil)
    (pdf-drop--process filename)
    ;; Only move file here for the synchronous DOI path.
    ;; AI fallback sets jds~pdf-ai-pending-file; callback handles the move.
    (when (and jds~bibtex-clean-latest-stored-key
               (not jds~pdf-ai-pending-file))
      (jds/dired-add-file-to-bib jds~bibtex-clean-latest-stored-key filename))))

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

;;; AI-powered org-roam note creation -----------------------------------------

(defun jds~pdf-ai-find-pdf (citekey)
  "Return full path to PDF for CITEKEY, or nil if not found.
Tries citar-get-files first (which returns a hash-table keyed by citekey),
then constructs the expected path from citar-library-paths."
  (or (when-let* ((tbl   (citar-get-files citekey))
                  (files (gethash citekey tbl)))
        (seq-find (lambda (f)
                    (string= "pdf" (downcase (or (file-name-extension f) ""))))
                  files))
      (let ((expected (expand-file-name (concat citekey ".pdf")
                                        (car citar-library-paths))))
        (when (file-exists-p expected) expected))))

(defun jds~pdf-ai-summary-insert (citekey summary)
  "Create/open org-roam note for CITEKEY and append AI SUMMARY."
  (let* ((note-path (expand-file-name
                     (concat (file-name-as-directory citar-org-roam-subdir)
                             citekey ".org")
                     org-roam-directory)))
    (unless (file-exists-p note-path)
      (let ((entry (citar-get-entry citekey)))
        (jds/citar-org-roam--create-capture-note citekey entry)))
    (with-current-buffer (find-file-noselect note-path)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n* AI Summary\n\n" summary "\n")
      (save-buffer)
      (switch-to-buffer (current-buffer))
      (message "AI summary inserted for %s." citekey))))

;;;###autoload
(defun jds/pdf-ai-summarize-to-roam (citekey &optional extra-instructions)
  "Use AI to summarize PDF for CITEKEY and insert into its org-roam note.
Works on any citar entry — newly imported or already in library.
Creates the org-roam note if it does not yet exist.

With prefix argument \\[universal-argument], prompts for EXTRA-INSTRUCTIONS
to append to the summary request (e.g. \"focus on the statistical methods\")."
  (interactive
   (list (car (citar-select-refs :multiple nil))
         (when current-prefix-arg
           (read-string "Extra instructions for AI summary: "))))
  (let ((pdf (jds~pdf-ai-find-pdf citekey)))
    (if (not pdf)
        (user-error "No PDF found for %s" citekey)
      (message "Generating AI summary for %s..." citekey)
      (let* ((text    (jds/pdf-extract-text-pages pdf 8))
             (entry   (citar-get-entry citekey))
             (title   (citar-format--entry "${title}"  entry))
             (authors (citar-format--entry "${author}" entry))
             (extra   (when (and extra-instructions
                                 (not (string-empty-p (string-trim extra-instructions))))
                        (format "\n\nAdditional instructions: %s" extra-instructions)))
             (prompt  (format "Write a structured research note for this academic paper in org-mode format.\n\nTitle: %s\nAuthors: %s\n\nInclude these sections as org headings:\n- Research Question\n- Methods\n- Key Findings\n- Contributions & Implications%s\n\nPaper text (first pages):\n---\n%s\n---\n\nReturn only org-mode text with * headings. No preamble or explanation."
                              title authors (or extra "") text)))
        (gptel-request prompt
          :system "You are a research assistant. Produce structured org-mode notes for academic papers."
          :callback (lambda (response info)
                      (if (not response)
                          (message "AI summary failed: %s" (plist-get info :status))
                        (jds~pdf-ai-summary-insert citekey response))))))))

;;;###autoload
(defun jds/pdf-ai-summarize-to-roam-at-point ()
  "AI-summarize PDF at point in dired or pdf-view-mode.
Derives citekey from filename (assumes file is named CITEKEY.pdf).
With prefix argument \\[universal-argument], prompts for extra instructions."
  (interactive)
  (let* ((file (cond
                ((derived-mode-p 'dired-mode)    (dired-get-filename))
                ((derived-mode-p 'pdf-view-mode) (buffer-file-name))
                (t (user-error "Not in dired or pdf-view-mode"))))
         (citekey (file-name-base file))
         (extra   (when current-prefix-arg
                    (read-string "Extra instructions for AI summary: "))))
    (jds/pdf-ai-summarize-to-roam citekey extra)))

;;bibtex-clean-entry-hook cleaned after clea
;; (bibtex-completion-get-key-bibtex)

(jds/localleader-def
  :keymaps 'dired-mode-map
  "r" #'jds/pdf-drop-process
  "f" #'jds/dired-add-file-to-bib
  "S" #'jds/pdf-ai-summarize-to-roam-at-point)

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
 "a" #'jds/pdf-ai-summarize-to-roam)

(jds/localleader-def
  :keymaps '(org-mode-map LaTeX-mode-map)
  "c" jds/citation-map)


