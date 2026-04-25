;;; references.el --- summary -*- lexical-binding: t -*-

;; Parent module owns its local helper commands so `init.el` stays ignorant of
;; internal support files.
(load-config "references-helpers.el")
(require 'json)

(defconst jds/references-bibliography
  "/home/jds6696/Dropbox/org/roam/references/references.bib"
  "Primary bibliography file for references workflows.")

(defconst jds/references-library-path
  "/home/jds6696/Dropbox/org/roam/references/articles/"
  "Directory containing attached reference files.")

(defvar jds~bibtex-clean-latest-stored-key nil
  "Most recent citekey captured after `bibtex-clean-entry'.")

(use-package bibtex-completion
  :straight t
  :defer t
  :config
  (setq bibtex-completion-bibliography (list jds/references-bibliography)
	;; this is used by org-roam-bibtex note actions
	bibtex-completion-library-path (list jds/references-library-path)))

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
  (org-cite-global-bibliography (list jds/references-bibliography))
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
  (setq citar-library-paths (list jds/references-library-path)
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
  (setq zotra-default-bibliography jds/references-bibliography
	;; todo could upgrade to the zotra-cli which can also download papers
	zotra-backend 'translation-server)
  (add-hook 'zotra-after-add-entry-hook (lambda ()
					  (bibtex-clean-entry t)
					  (bibtex-sort-buffer)))
  (defun jds~bibtex-store-latest-key-hook-function ()
    "Function to be added to bibtex-clean-entry-hook and run after cleaning in the cleaned entry."
    (setq jds~bibtex-clean-latest-stored-key (bibtex-completion-get-key-bibtex)))
  (add-hook 'bibtex-clean-entry-hook 'jds~bibtex-store-latest-key-hook-function))

(defun jds~zotra-add-entry-return-inserted-key (query)
  "Import QUERY with Zotra and return the citekey of the inserted entry.
Prefer the key captured during `bibtex-clean-entry-hook', because
`zotra-add-entry-from-search' can return a stale key."
  (when-let ((query (jds~pdf-drop-normalize-metadata-string query)))
    (let ((jds~bibtex-clean-latest-stored-key nil))
      (condition-case nil
          (let* ((returned-key (zotra-add-entry-from-search query))
                 (returned-key
                  (when returned-key
                    (let ((string (format "%s" returned-key)))
                      (unless (string-empty-p string)
                        string))))
                 (cleaned-key jds~bibtex-clean-latest-stored-key))
            (when (and cleaned-key returned-key
                       (not (string= cleaned-key returned-key)))
              (message "Zotra returned stale key %s; using inserted key %s"
                       returned-key
                       cleaned-key))
            (or cleaned-key returned-key))
        (error nil)))))

;;; PDF text extraction — top-level so it's usable independently of pdf-drop-mode
;;;###autoload
(defun jds/pdf-extract-text-pages (file &optional max-pages)
  "Extract text from first MAX-PAGES (default 4) pages of FILE via pdftotext.
Returns empty string on failure."
  (if (not (executable-find "pdftotext"))
      ""
    (condition-case nil
	(with-temp-buffer
	  (call-process "pdftotext" nil t nil
			"-f" "1" "-l" (number-to-string (or max-pages 4))
			(expand-file-name file) "-")
	  (buffer-string))
      (error ""))))

(defconst jds/pdf-drop-ai-metadata-pages 8
  "Number of initial PDF pages to inspect during fallback metadata recovery.")

(defun jds~pdf-drop-clean-identifier (identifier)
  "Trim whitespace and trailing punctuation from IDENTIFIER."
  (when (stringp identifier)
    (let ((trimmed (string-trim identifier)))
      (unless (string-empty-p trimmed)
        (replace-regexp-in-string "[[:space:][:punct:]]+\\'" "" trimmed)))))

(defun jds~pdf-drop-normalize-metadata-string (value)
  "Return VALUE as a trimmed string, or nil if it is empty."
  (when value
    (let ((string (string-trim (format "%s" value))))
      (unless (string-empty-p string)
        string))))

(defun jds~pdf-drop-normalize-authors (authors)
  "Return AUTHORS as a list of non-empty author strings."
  (cond
   ((stringp authors)
    (let ((author (jds~pdf-drop-normalize-metadata-string authors)))
      (if author (list author) nil)))
   ((listp authors)
    (delq nil (mapcar #'jds~pdf-drop-normalize-metadata-string authors)))
   (t nil)))

(defun jds~pdf-drop-identifier-kind (identifier)
  "Return the kind of IDENTIFIER as `doi', `arxiv', or nil."
  (when-let ((identifier (jds~pdf-drop-clean-identifier identifier)))
    (let ((case-fold-search t))
      (cond
       ((string-match-p "\\`10\\.[0-9]\\{4,9\\}/[-._;()/:A-Z0-9]+\\'" identifier)
        'doi)
       ((string-match-p
         "\\`\\(?:arxiv:\\)?\\(?:[0-9]\\{4\\}\\.[0-9]\\{4,5\\}\\|[[:alpha:]-]+\\(?:\\.[[:alpha:]-]+\\)?/[0-9]\\{7\\}\\)\\(?:v[0-9]+\\)?\\'"
         identifier)
        'arxiv)
       (t nil)))))

(defun jds~pdf-drop-extract-doi-from-text (text)
  "Return the first DOI found in TEXT, or nil."
  (when (stringp text)
    (let ((case-fold-search t))
      (when (string-match "\\b\\(10\\.[0-9]\\{4,9\\}/[-._;()/:A-Z0-9]+\\)" text)
        (jds~pdf-drop-clean-identifier (match-string 1 text))))))

(defun jds~pdf-drop-extract-arxiv-from-text (text)
  "Return the first arXiv identifier found in TEXT, or nil."
  (when (stringp text)
    (let ((case-fold-search t))
      (or
       (when (string-match
              "\\barxiv:\\s-*\\([0-9]\\{4\\}\\.[0-9]\\{4,5\\}\\(?:v[0-9]+\\)?\\)\\b"
              text)
         (jds~pdf-drop-clean-identifier (match-string 1 text)))
       (when (string-match
              "\\b\\([[:alpha:]-]+\\(?:\\.[[:alpha:]-]+\\)?/[0-9]\\{7\\}\\(?:v[0-9]+\\)?\\)\\b"
              text)
         (jds~pdf-drop-clean-identifier (match-string 1 text)))))))

(defun jds~pdf-drop-filename-query (file)
  "Return a normalized filename-derived query for FILE."
  (when file
    (let* ((base (file-name-base file))
           (query (replace-regexp-in-string "[_-]+" " " base)))
      (jds~pdf-drop-normalize-metadata-string query))))

(defun jds~pdf-drop-local-metadata (file title text &optional seed-identifier)
  "Build local metadata hints for FILE from TITLE, TEXT, and SEED-IDENTIFIER."
  (let* ((seed (jds~pdf-drop-clean-identifier seed-identifier))
         (seed-kind (jds~pdf-drop-identifier-kind seed))
         (doi (or (and (eq seed-kind 'doi) seed)
                  (jds~pdf-drop-extract-doi-from-text text)))
         (arxiv (or (and (eq seed-kind 'arxiv)
                         (replace-regexp-in-string "\\`arxiv:" "" seed t t))
                    (jds~pdf-drop-extract-arxiv-from-text text))))
    (list :doi doi
          :arxiv arxiv
          :title (jds~pdf-drop-normalize-metadata-string title)
          :authors nil
          :year nil
          :filename-query (jds~pdf-drop-filename-query file))))

(defun jds~pdf-drop-json-object-string (response)
  "Extract the first JSON object string from RESPONSE."
  (when (stringp response)
    (let ((start (string-match "{" response))
          (end (and (string-match "}[[:space:]\n\r\t]*\\'" response)
                    (match-end 0))))
      (when (and start end (> end start))
        (substring response start end)))))

(defun jds~pdf-drop-parse-ai-metadata (response)
  "Parse structured metadata JSON from RESPONSE into a plist."
  (when-let ((json-text (jds~pdf-drop-json-object-string response)))
    (condition-case nil
        (if (fboundp 'json-parse-string)
            (json-parse-string json-text
                               :object-type 'plist
                               :array-type 'list
                               :null-object nil
                               :false-object nil)
          (let ((json-object-type 'plist)
                (json-array-type 'list)
                (json-null nil)
                (json-false nil))
            (json-read-from-string json-text)))
      (error nil))))

(defun jds~pdf-drop-merge-metadata (local ai)
  "Merge LOCAL and AI metadata plists, preferring explicit identifiers."
  (let* ((ai-arxiv (when-let ((value (jds~pdf-drop-normalize-metadata-string
                                      (plist-get ai :arxiv))))
                     (replace-regexp-in-string "\\`arxiv:" "" value t t)))
         (authors (or (jds~pdf-drop-normalize-authors (plist-get ai :authors))
                      (plist-get local :authors))))
    (list :doi (or (jds~pdf-drop-clean-identifier (plist-get local :doi))
                   (jds~pdf-drop-clean-identifier (plist-get ai :doi)))
          :arxiv (or (plist-get local :arxiv) ai-arxiv)
          :title (or (plist-get local :title)
                     (jds~pdf-drop-normalize-metadata-string (plist-get ai :title)))
          :authors authors
          :year (or (plist-get local :year)
                    (jds~pdf-drop-normalize-metadata-string (plist-get ai :year)))
          :filename-query (plist-get local :filename-query))))

(defun jds~pdf-drop-metadata-queries (metadata)
  "Return prioritized zotra search queries derived from METADATA."
  (let* ((doi (jds~pdf-drop-clean-identifier (plist-get metadata :doi)))
         (arxiv (jds~pdf-drop-clean-identifier (plist-get metadata :arxiv)))
         (title (jds~pdf-drop-normalize-metadata-string (plist-get metadata :title)))
         (authors (jds~pdf-drop-normalize-authors (plist-get metadata :authors)))
         (year (jds~pdf-drop-normalize-metadata-string (plist-get metadata :year)))
         (filename-query (plist-get metadata :filename-query))
         (author-fragment (when authors
                            (mapconcat #'identity (seq-take authors 2) " ")))
         (queries (delq nil
                        (list doi
                              (when arxiv (format "arXiv:%s" arxiv))
                              arxiv
                              title
                              (when (and title author-fragment)
                                (format "%s %s" title author-fragment))
                              (when (and title year)
                                (format "%s %s" title year))
                              filename-query))))
    (delete-dups queries)))

(defun jds~pdf-drop-try-zotra-query (query)
  "Import QUERY with Zotra and return the inserted citekey, or nil."
  (jds~zotra-add-entry-return-inserted-key query))

(defun jds~pdf-drop-try-import-from-metadata (file metadata)
  "Try importing FILE using zotra queries derived from METADATA.
Return the citekey on success, or nil otherwise."
  (catch 'done
    (dolist (query (jds~pdf-drop-metadata-queries metadata))
      (when-let ((key (jds~pdf-drop-try-zotra-query query)))
        (jds/dired-add-file-to-bib key file)
        (throw 'done key)))))

(defun jds~pdf-drop-build-ai-metadata-prompt (file title text metadata)
  "Build a structured metadata prompt for FILE using TITLE, TEXT, and METADATA."
  (format
   (concat
    "Identify this academic paper as precisely as possible.\n"
    "Prefer canonical identifiers explicitly supported by the evidence.\n"
    "If the manuscript appears on arXiv, return the arXiv identifier.\n"
    "Do not invent identifiers.\n\n"
    "Return ONLY compact JSON with these keys:\n"
    "{\"doi\": string|null, \"arxiv\": string|null, \"title\": string|null, "
    "\"authors\": [string], \"year\": string|null}\n\n"
    "Filename: %s\n"
    "%s"
    "%s\n\n"
    "PDF text from the first %d pages:\n---\n%s\n---")
   (file-name-nondirectory file)
   (if-let ((pdf-title (jds~pdf-drop-normalize-metadata-string title)))
       (format "PDF metadata title: %s\n" pdf-title)
     "")
   (let ((hints (jds~pdf-drop-metadata-queries metadata)))
     (if hints
         (format "Recovered local hints: %s\n"
                 (mapconcat #'identity hints " | "))
       ""))
   jds/pdf-drop-ai-metadata-pages
   text))

(defun jds~pdf-drop-build-ai-bibtex-prompt (title text metadata)
  "Build the last-resort BibTeX prompt from TITLE, TEXT, and METADATA."
  (let* ((doi (plist-get metadata :doi))
         (arxiv (plist-get metadata :arxiv))
         (resolved-title (or (plist-get metadata :title) title))
         (authors (plist-get metadata :authors))
         (year (plist-get metadata :year))
         (metadata-lines
          (delq nil
                (list (when doi (format "DOI: %s" doi))
                      (when arxiv (format "arXiv: %s" arxiv))
                      (when resolved-title (format "Title: %s" resolved-title))
                      (when authors
                        (format "Authors: %s" (mapconcat #'identity authors "; ")))
                      (when year (format "Year: %s" year))))))
    (format
     (concat
      "I need a BibTeX entry for this academic paper.\n"
      "Use the metadata below when supported by the evidence.\n"
      "If an arXiv identifier is known, prefer an arXiv-aware BibTeX entry over a generic draft note.\n"
      "Return ONLY a single complete BibTeX entry starting with @.\n"
      "No explanation, no markdown fences.\n\n"
      "%s\n\n"
      "Text from the first %d pages:\n---\n%s\n---")
     (if metadata-lines
         (mapconcat #'identity metadata-lines "\n")
       (if-let ((pdf-title (jds~pdf-drop-normalize-metadata-string title)))
           (format "PDF metadata title: %s" pdf-title)
         "No reliable metadata recovered."))
     jds/pdf-drop-ai-metadata-pages
     text)))

(use-package pdf-drop-mode
  :straight (pdf-drop-mode :type git :host github :repo "rougier/pdf-drop-mode")
  :config
  ;; Async callback: insert AI BibTeX into .bib file, then move FILE.
  (defun jds~pdf-ai-bibtex-callback (file response info)
    "Handle gptel RESPONSE for FILE by inserting BibTeX and moving the PDF."
    (if (not response)
        (message "AI BibTeX lookup failed for %s: %s"
                 (file-name-nondirectory file)
                 (plist-get info :status))
      (let* ((bib-start (and (string-match "@[a-zA-Z]+{" response)
                             (match-beginning 0)))
             (bibtex    (if bib-start (substring response bib-start) response))
             (bib-file  jds/references-bibliography))
        (condition-case err
            (let (key)
              (with-current-buffer (find-file-noselect bib-file)
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (let ((entry-start (point)))
                  (insert bibtex "\n")
                  (goto-char entry-start)
                  (bibtex-clean-entry t)
                  (setq key (cdr (assoc "=key=" (bibtex-parse-entry t))))
                  (bibtex-sort-buffer)
                  (save-buffer)))
              (if key
                  (progn
                    (message "AI BibTeX added: %s" key)
                    (jds/dired-add-file-to-bib key file))
                (message "AI BibTeX inserted for %s; could not determine key"
                         (file-name-nondirectory file))))
          (error (message "Error processing AI BibTeX: %s" (error-message-string err)))))))

  (defun jds~pdf-drop-request-ai-bibtex (file title text metadata)
    "Ask AI for a BibTeX entry for FILE using TITLE, TEXT, and METADATA."
    (message "Metadata lookup incomplete for %s; requesting final BibTeX..."
             (file-name-nondirectory file))
    (gptel-request
     (jds~pdf-drop-build-ai-bibtex-prompt title text metadata)
     :system (concat
              "You are a bibliographic assistant. "
              "Prefer canonical repository metadata over generic draft phrasing. "
              "Return only a single complete BibTeX entry.")
     :callback (lambda (response info)
                 (jds~pdf-ai-bibtex-callback file response info))))

  (defun jds~pdf-drop-ai-metadata-callback (file title text local-metadata response info)
    "Handle structured AI metadata RESPONSE for FILE."
    (if (not response)
        (progn
          (message "AI metadata lookup failed for %s: %s"
                   (file-name-nondirectory file)
                   (plist-get info :status))
          (jds~pdf-drop-request-ai-bibtex file title text local-metadata))
      (let* ((ai-metadata (jds~pdf-drop-parse-ai-metadata response))
             (metadata (jds~pdf-drop-merge-metadata local-metadata ai-metadata)))
        (if-let ((key (jds~pdf-drop-try-import-from-metadata file metadata)))
            (message "Imported bibliography entry for %s as %s"
                     (file-name-nondirectory file)
                     key)
          (jds~pdf-drop-request-ai-bibtex file title text metadata)))))

  ;; Called when DOI/arXiv lookup fails or when no identifier is found at all.
  (defun jds/pdf-drop-ai-bibtex-fallback (file title &optional seed-identifier)
    "Recover metadata for FILE, then create a BibTeX entry as a last resort.
TITLE is the PDF metadata title and SEED-IDENTIFIER is any earlier DOI/arXiv hint."
    (message "Identifier lookup failed for %s; recovering metadata..."
             (file-name-nondirectory file))
    (let* ((text (jds/pdf-extract-text-pages file jds/pdf-drop-ai-metadata-pages))
           (local-metadata (jds~pdf-drop-local-metadata file title text seed-identifier)))
      (if-let ((key (jds~pdf-drop-try-import-from-metadata file local-metadata)))
          (message "Imported bibliography entry for %s as %s"
                   (file-name-nondirectory file)
                   key)
        (gptel-request
         (jds~pdf-drop-build-ai-metadata-prompt file title text local-metadata)
         :system (concat
                  "You are a bibliographic identification assistant. "
                  "Extract reliable metadata from partial manuscript text. "
                  "Prefer explicit DOI/arXiv identifiers when present. "
                  "Return only compact JSON.")
         :callback (lambda (response info)
                     (jds~pdf-drop-ai-metadata-callback
                      file title text local-metadata response info))))))

  ;; When DOI found but zotra lookup fails, fall back to AI
  (defun jds/pdf-drop-mode-call-zotra (file identifier)
    "Call zotra for IDENTIFIER lookup; fall back to staged recovery if needed."
    (let ((identifier-string (jds~pdf-drop-clean-identifier
                              (if (listp identifier) (cdr identifier) identifier))))
      (or (jds~pdf-drop-try-zotra-query identifier-string)
          (jds/pdf-drop-ai-bibtex-fallback
           file
           (ignore-errors (pdf-drop-get-title-from-metadata file))
           identifier-string))))
  (setq pdf-drop-search-hook #'jds/pdf-drop-mode-call-zotra)
  (setq pdf-drop-search-methods '(doi/metadata doi/content arxiv/content)))

(defun jds~pdf-drop-find-identifier (file)
  "Return the first non-interactive identifier found for FILE.
The return value is a cons like `(doi . VALUE)' or `(arxiv . VALUE)'."
  (catch 'found
    (dolist (method pdf-drop-search-methods)
      (pcase method
        ('doi/metadata
         (when-let ((doi (pdf-drop-get-doi-from-metadata file)))
           (when (pdf-drop-validate-doi doi)
             (throw 'found `(doi . ,doi)))))
        ('doi/content
         (when-let ((doi (pdf-drop-get-doi-from-content file)))
           (when (pdf-drop-validate-doi doi)
             (throw 'found `(doi . ,doi)))))
        ('arxiv/content
         (when-let ((arxiv-id (pdf-drop-get-arxiv-id-from-content file)))
           (when (pdf-drop-validate-arxiv-id arxiv-id)
             (throw 'found `(arxiv . ,arxiv-id)))))))))

;;;###autoload
(defun jds/pdf-drop-process ()
  "Process PDF at point: import by identifier, or fall back to AI."
  (interactive)
  (let ((filename (dired-copy-filename-as-kill 0)))
    (if-let ((file-id (jds~pdf-drop-find-identifier filename)))
        (let ((key (jds~pdf-drop-try-zotra-query (cdr file-id))))
          (if key
              (jds/dired-add-file-to-bib key filename)
            (jds/pdf-drop-ai-bibtex-fallback
             filename
             (ignore-errors (pdf-drop-get-title-from-metadata filename))
             (cdr file-id))))
      (jds/pdf-drop-ai-bibtex-fallback
       filename
       (ignore-errors (pdf-drop-get-title-from-metadata filename))))))

;;;###autoload
(defun jds/dired-add-file-to-bib (key file)
    "Attach file to bibtex entry with KEY located in global
bibliography. Moves file into global file library. FILE should be
a string specifying full filepath."
    (interactive (list  (car (citar-select-refs :multiple nil))
			(dired-get-filename)))
    (let* ((destname (concat jds/references-library-path key (file-name-extension file t)))
	   (destfile (file-name-nondirectory destname)))
      (rename-file file destname)
      (save-excursion
	(citar-open-entry key)
	(bibtex-make-field `("file" "File to Attach" ,destfile) t))))

;;; AI-powered org-roam note creation -----------------------------------------

;;; gptel-reinforce integration for PDF summaries ----------------------------

(declare-function gptel-reinforce-register-database "gptel-reinforce" (&rest plist))
(declare-function gptel-reinforce-register-artifact "gptel-reinforce" (&rest plist))
(declare-function gptel-reinforce-set-active-database "gptel-reinforce" (database))
(declare-function gptel-reinforce-track-output-region "gptel-reinforce" (artifact start end &optional output-id))
(declare-function gptel-reinforce-tools--live-system "gptel-reinforce-tools" (artifact-name initial-system))

(defconst jds/pdf-ai-reinforce-artifact "academic-paper-summary"
  "Artifact name for the AI PDF summary system prompt.")

(defconst jds/pdf-ai-reinforce-system
  "You are a research assistant. Produce structured org-mode notes for academic papers."
  "Initial system prompt for AI PDF summaries.")

(defvar-local jds/pdf-ai-reinforce-citekey nil
  "Buffer-local citekey for gptel-reinforce tracking in PDF summary buffers.")

(defun jds/pdf-ai-reinforce--paper-meta (citekey)
  "Return a plist of paper metadata for CITEKEY from bibtex-completion, or nil."
  (when (and citekey (fboundp 'bibtex-completion-get-entry))
    (when-let ((entry (ignore-errors (bibtex-completion-get-entry citekey))))
      (let ((title   (bibtex-completion-get-value "title"   entry))
            (year    (bibtex-completion-get-value "year"    entry))
            (keywords (bibtex-completion-get-value "keywords" entry)))
        (when (or title year keywords)
          (list :paper-title title :year year :keywords keywords))))))

(defun jds/pdf-ai-reinforce-candidate ()
  "Return a gptel-reinforce candidate for the current PDF summary buffer."
  (when jds/pdf-ai-reinforce-citekey
    (let* ((meta (jds/pdf-ai-reinforce--paper-meta jds/pdf-ai-reinforce-citekey))
           (title (or (plist-get meta :paper-title) jds/pdf-ai-reinforce-citekey)))
      (list :context
            (list :item-key (format "academic-paper:%s" jds/pdf-ai-reinforce-citekey)
                  :title title
                  :meta meta)))))

(with-eval-after-load 'gptel-reinforce
  (gptel-reinforce-register-database
   :name "academic-papers"
   :candidate-fn #'jds/pdf-ai-reinforce-candidate)
  (gptel-reinforce-register-artifact
   :name jds/pdf-ai-reinforce-artifact
   :database "academic-papers"
   :initial-text jds/pdf-ai-reinforce-system
   :type "prompt"))

(defun jds~pdf-ai-find-pdf (citekey)
  "Return full path to PDF for CITEKEY, or nil if not found.
Tries citar-get-files first (which returns a hash-table keyed by citekey),
then constructs the expected path from `jds/references-library-path'."
  (or (when-let* ((tbl   (citar-get-files citekey))
                  (files (gethash citekey tbl)))
        (seq-find (lambda (f)
                    (string= "pdf" (downcase (or (file-name-extension f) ""))))
                  files))
      (let ((expected (expand-file-name (concat citekey ".pdf")
                                        jds/references-library-path)))
        (when (file-exists-p expected) expected))))

(define-derived-mode jds/pdf-ai-summary-mode org-mode "AI-PDF"
  "Read-only org buffer for AI-generated PDF summaries."
  (setq-local header-line-format "AI PDF Summary  |  q: close")
  (read-only-mode 1))

(defun jds~pdf-ai-summary-buffer-name (title)
  "Return a buffer name for PDF summary TITLE."
  (format "*AI PDF Summary: %s*"
          (truncate-string-to-width (or title "(untitled)") 50 nil nil t)))

(defun jds~pdf-ai-summary-title (file &optional citekey)
  "Return a title for FILE, using CITEKEY metadata when available."
  (or (when citekey
        (when-let ((entry (ignore-errors (citar-get-entry citekey))))
          (let ((title (citar-format--entry "${title}" entry)))
            (unless (string-empty-p (string-trim (or title "")))
              title))))
      (file-name-base file)))

(defun jds~pdf-ai-summary-authors (citekey)
  "Return author text for CITEKEY, or nil if unavailable."
  (when citekey
    (when-let ((entry (ignore-errors (citar-get-entry citekey))))
      (let ((authors (citar-format--entry "${author}" entry)))
        (unless (string-empty-p (string-trim (or authors "")))
          authors)))))

(defun jds~pdf-ai-build-summary-prompt (file &optional citekey extra-instructions)
  "Build the summary prompt for FILE.
Uses CITEKEY metadata when available, and appends EXTRA-INSTRUCTIONS when set."
  (let* ((title   (jds~pdf-ai-summary-title file citekey))
         (authors (or (jds~pdf-ai-summary-authors citekey) "Unknown"))
         (text    (jds/pdf-extract-text-pages file 8))
         (extra   (when (and extra-instructions
                             (not (string-empty-p (string-trim extra-instructions))))
                    (format "\n\nAdditional instructions: %s" extra-instructions))))
    (format "Write a structured research note for this academic paper in org-mode format.\n\nTitle: %s\nAuthors: %s\n\nInclude these sections as org headings:\n- Research Question\n- Methods\n- Key Findings\n- Contributions & Implications%s\n\nPaper text (first pages):\n---\n%s\n---\n\nReturn only org-mode text with * headings. No preamble or explanation."
            title authors (or extra "") text)))

(defun jds~pdf-ai-show-summary-buffer (file response &optional citekey)
  "Display AI summary RESPONSE for FILE in a read-only org buffer."
  (let ((title (jds~pdf-ai-summary-title file citekey))
        (buf (generate-new-buffer
              (jds~pdf-ai-summary-buffer-name
               (jds~pdf-ai-summary-title file citekey)))))
    (with-current-buffer buf
      (jds/pdf-ai-summary-mode)
      (read-only-mode -1)
      (setq-local jds/pdf-ai-reinforce-citekey (or citekey (file-name-base file)))
      (when (fboundp 'gptel-reinforce-set-active-database)
        (gptel-reinforce-set-active-database "academic-papers"))
      (insert (format "#+TITLE: PDF Summary: %s\n\n" title))
      (let ((start (point)))
        (insert (string-trim (or response "")))
        (insert "\n")
        (when (fboundp 'gptel-reinforce-track-output-region)
          (gptel-reinforce-track-output-region jds/pdf-ai-reinforce-artifact start (point))))
      (goto-char (point-min))
      (read-only-mode 1)
      (evil-local-set-key 'normal (kbd "q") #'bury-buffer))
    (pop-to-buffer buf)))

(defun jds~pdf-ai-request-summary (file callback &optional citekey extra-instructions)
  "Generate an AI summary for FILE and pass it to CALLBACK.
When provided, CITEKEY and EXTRA-INSTRUCTIONS are used in the prompt."
  (message "Generating AI summary for %s..." (file-name-nondirectory file))
  (let ((system (if (featurep 'gptel-reinforce)
                    (gptel-reinforce-tools--live-system
                     jds/pdf-ai-reinforce-artifact jds/pdf-ai-reinforce-system)
                  jds/pdf-ai-reinforce-system)))
    (gptel-request
     (jds~pdf-ai-build-summary-prompt file citekey extra-instructions)
     :system system
     :callback (lambda (response info)
                 (if (not response)
                     (message "AI summary failed: %s" (plist-get info :status))
                   (funcall callback response))))))

(defun jds~pdf-ai-summary-insert (citekey summary)
  "Create/open org-roam note for CITEKEY and append AI SUMMARY."
  (let* ((note-path (expand-file-name
                     (concat (if citar-org-roam-subdir
                                 (file-name-as-directory citar-org-roam-subdir)
                               "")
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
      (jds~pdf-ai-request-summary
       pdf
       (lambda (response)
         (jds~pdf-ai-summary-insert citekey response))
       citekey
       extra-instructions))))

;;;###autoload
(defun jds/pdf-ai-summarize-reference (citekey &optional extra-instructions)
  "Summarize the PDF for CITEKEY into a temporary read-only org buffer."
  (interactive
   (list (car (citar-select-refs :multiple nil))
         (when current-prefix-arg
           (read-string "Extra instructions for AI summary: "))))
  (if-let ((pdf (jds~pdf-ai-find-pdf citekey)))
      (jds~pdf-ai-request-summary
       pdf
       (lambda (response)
         (jds~pdf-ai-show-summary-buffer pdf response citekey))
       citekey
       extra-instructions)
    (user-error "No PDF found for %s" citekey)))

;;;###autoload
(defun jds/pdf-ai-summarize-file (file &optional extra-instructions)
  "Summarize PDF FILE into a temporary read-only org buffer.
With prefix argument \\[universal-argument], prompt for EXTRA-INSTRUCTIONS."
  (interactive
   (list (read-file-name "PDF: " nil nil t nil
                         (lambda (f)
                           (string= (downcase (or (file-name-extension f) ""))
                                    "pdf")))
         (when current-prefix-arg
           (read-string "Extra instructions for AI summary: "))))
  (unless (string= (downcase (or (file-name-extension file) "")) "pdf")
    (user-error "Not a PDF file: %s" file))
  (jds~pdf-ai-request-summary
   file
   (lambda (response)
     (jds~pdf-ai-show-summary-buffer file response))
   (file-name-base file)
   extra-instructions))

;;;###autoload
(defun jds/pdf-ai-summarize-file-to-roam (file &optional extra-instructions)
  "Summarize PDF FILE and append the result to the matching org-roam note."
  (interactive
   (list (read-file-name "PDF: " nil nil t nil
                         (lambda (f)
                           (string= (downcase (or (file-name-extension f) ""))
                                    "pdf")))
         (when current-prefix-arg
           (read-string "Extra instructions for AI summary: "))))
  (jds/pdf-ai-summarize-to-roam (file-name-base file) extra-instructions))

;;;###autoload
(defun jds/pdf-ai-summarize-at-point ()
  "AI-summarize the PDF at point in dired or pdf-view-mode."
  (interactive)
  (let* ((file (cond
                ((derived-mode-p 'dired-mode) (dired-get-filename))
                ((derived-mode-p 'pdf-view-mode) (buffer-file-name))
                (t (user-error "Not in dired or pdf-view-mode"))))
         (extra (when current-prefix-arg
                  (read-string "Extra instructions for AI summary: "))))
    (jds/pdf-ai-summarize-file file extra)))

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
  "s" #'jds/pdf-ai-summarize-at-point
  "S" #'jds/pdf-ai-summarize-to-roam-at-point)

(with-eval-after-load 'embark
  (define-key embark-file-map (kbd "m") #'jds/pdf-ai-summarize-file)
  (define-key embark-file-map (kbd "A") #'jds/pdf-ai-summarize-file-to-roam)
  (with-eval-after-load 'citar-embark
    (define-key citar-embark-map (kbd "m") #'jds/pdf-ai-summarize-reference)
    (define-key citar-embark-map (kbd "A") #'jds/pdf-ai-summarize-to-roam)
    (define-key citar-embark-citation-map (kbd "m") #'jds/pdf-ai-summarize-reference)
    (define-key citar-embark-citation-map (kbd "A") #'jds/pdf-ai-summarize-to-roam)))

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
 "u" #'zotra-add-entry-from-url
 "S" #'zotra-add-entry-from-search)

(jds/localleader-def
  :keymaps '(org-mode-map LaTeX-mode-map)
  "c" jds/citation-map)
