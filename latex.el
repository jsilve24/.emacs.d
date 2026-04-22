;;; latex.el --- latex related config -*- lexical-binding: t; -*-

(use-package auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook (LaTeX-mode . reftex-mode)
  :hook (TeX-mode . visual-line-mode)   ; Enable word wrapping
  :hook (Tex-mode . outline-minor-mode) ; Enable folding
  :ensure t
  :diminish reftex-mode
  :config
  (setq TeX-parse-self t ;; parse on load
	TeX-auto-save t	 ;; parse on save
	TeX-auto-local ".auctex-auto"
	TeX-style-local ".auctex-style"
	;; automatically insert braces after sub/superscript in math mode
	TeX-electric-sub-and-superscript t
	;; just save, dont ask me before each compilation
	TeX-save-query nil)
  ;; Finally, if you often use \include or \input, you should make AUCTeX aware of the multi-file
  ;; document structure. You can do this by inserting
  ;; however doing this makes it promp for master every time (doom sets this to t)
  (setq-default TeX-master nil)

  ;; set-up chktex -- from doom
  ;; (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")
  (add-hook 'TeX-mode-hook (lambda ()
			     (setq ispell-parser 'tex
				   fill-nobreak-predicate (cons #'texmathp fill-nobreak-predicate)))))

(with-eval-after-load 'tex
  (setq TeX-source-correlate-mode t
	;; don't start the emacs server when correlating sources
	TeX-source-correlate-start-server nil
	TeX-source-correlate-method 'synctex
	;; support more electric pair braces, e.g., \{...\}
	LaTeX-electric-left-right-brace t)
  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-source-correlate-start-server t)
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  (defcustom jds/latex-keep-synctex t
    "Keep Synctex files after successful LaTeX compilation.
When non-nil, automatic cleanup preserves the generated
`.synctex.gz' file so PDF/source navigation keeps working."
    :type 'boolean
    :group 'TeX-command)

  (defun jds/latex--cleanup-suffixes ()
    "Return the suffixes to remove after a successful LaTeX run."
    (let ((suffixes (copy-sequence LaTeX-clean-intermediate-suffixes)))
      (if jds/latex-keep-synctex
          (delete "\\.synctex\\.gz" suffixes)
        suffixes)))

  (defun jds/latex-clean-intermediates-after-success (output-file)
    "Delete LaTeX intermediates after OUTPUT-FILE is built successfully."
    (let* ((raw-master (or (when (and (boundp 'TeX-command-buffer)
                                      (buffer-live-p TeX-command-buffer))
                             (with-current-buffer TeX-command-buffer
                               (TeX-master-file (TeX-output-extension))))
                           output-file))
           (master (file-name-sans-extension raw-master))
           (directory (or (file-name-directory master) default-directory))
           (basename (file-name-nondirectory master))
           (suffixes (jds/latex--cleanup-suffixes))
           (regexp (concat "\\`" (regexp-quote basename)
                           "\\(" (mapconcat #'identity suffixes "\\|")
                           "\\)\\'")))
      (when (and (file-directory-p directory) suffixes)
        (dolist (file (directory-files directory nil regexp))
          (ignore-errors
            (delete-file (expand-file-name file directory)))))))

  (add-hook 'TeX-after-compilation-finished-functions
            #'jds/latex-clean-intermediates-after-success)

  ;; texmathp should detect align(*) environments
  (setq texmathp-tex-commands '(("align*" env-on) ("align" env-on)))
  (texmathp-compile))

;;; setup cdlatex

(use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :hook (org-mode . turn-on-org-cdlatex)
  ;; smartparens takes care of inserting closing delimiters, and if you
  ;; don't use smartparens you probably won't want these also.
  ;; also auctex takes care of inserting _ and ^
  ;; also auctex already provides `LaTeX-insert-item' so C-ret not needed
  :diminish cdlatex-mode
  :bind
  (:map cdlatex-mode-map
	;; ("$" . nil)
	("(" . nil)
	("{" . nil)
	("[" . nil)
	("|" . nil)
	("<" . nil)
	("_" . nil)
	("^" . nil)
	;; ("TAB" . cdlatex-tab)
	;; ([(control return)] . nil)
	)
  :config
  ;; I think there might be a bug in my config such that I need the following line: 
  (defalias 'cdlatex--texmathp 'texmathp)

  (setq cdlatex-math-symbol-alist
	'((?< ("\\leftarrow" "\\Leftarrow" "\\longleftarrow" "\\Longleftarrow"))
	  (?> ("\\rightarrow" "\\Rightarrow" "\\longrightarrow" "\\Longrightarrow"))
	  (?\\ ("\\parallel"))
	  (?| ("\\perp"))))
  (setq cdlatex-math-modify-alist '((?\p "\\proc" nil t nil nil)
				    ;; mathbb requires the amssymb library in latex
				    (?\B "\\mathbb" nil t nil nil)))
  ;; Keep cdlatex from taking backtick key (this functionality is now done by aas snippets)
  (define-key cdlatex-mode-map (kbd "`") nil)

  ;; also turn off backtick from org mode 
  ;; don't have cdlatex take over the backtick symbol (funcationality done by aas snippets now)
  (defun jds~org-cdlatex-hook-function ()
    (define-key org-cdlatex-mode-map (kbd "`") nil))
  (add-hook 'org-mode-hook 'jds~org-cdlatex-hook-function)


  ;;  give me back my ' key
  (defun jds/cdlatex-math-modify (&optional arg)
    "Just wraps cdlatex-math-modify and really makes sure its only active in texmathp"
    (interactive "P")
    (if (texmathp)
	(cdlatex-math-modify arg)
      (self-insert-command 1)))
  (define-key cdlatex-mode-map (kbd "'") 'jds/cdlatex-math-modify))

;;; setup latexmk
(use-package auctex-latexmk
  :ensure t
  :defer t
  :functions auctex-latexmk-setup
  :preface
  ;; solving temporary issue with tex-buf.el not being found
  ;; https://github.com/tom-tan/auctex-latexmk/issues/39
  (defun my-auctex-latexmk-advice (req feature &rest args)
    "Call REQ with FEATURE and ARGS, unless FEATURE is `tex-buf'."
    (unless (eq feature 'tex-buf)
      (apply req feature args)))
  :init
  (unwind-protect
      (progn (advice-add 'require :around #'my-auctex-latexmk-advice)
	     (auctex-latexmk-setup))
    (advice-remove 'require #'my-auctex-latexmk-advice))

  ;; Pass the -pdf flag when TeX-PDF-mode is active
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
:config
  ;; Set LatexMk as the default
  (defun jds/latex-set-latexmk-default ()
    "Prefer LatexMk as the default compile command in LaTeX buffers."
    (setq TeX-command-default "LatexMk"))
  (add-hook 'LaTeX-mode-hook #'jds/latex-set-latexmk-default)
  (add-hook 'latex-mode-hook #'jds/latex-set-latexmk-default))


;;; setup evil-tex

(use-package evil-tex
  :straight (evil-tex :type git :host github :repo "iyefrat/evil-tex"
		      :fork t)
  :hook (LaTeX-mode . evil-tex-mode)
  :hook (org-mode . evil-tex-mode)
  :init
  (setq evil-tex-toggle-override-m nil
	evil-tex-toggle-override-t nil)
  :config
  (define-key evil-tex-inner-text-objects-map "y" 'evil-tex-inner-superscript)
  (define-key evil-tex-outer-text-objects-map "y" 'evil-tex-a-superscript)
  (define-key evil-tex-inner-text-objects-map "u" 'evil-tex-inner-subscript)
  (define-key evil-tex-outer-text-objects-map "u" 'evil-tex-a-subscript)

  

  ;; place evil tex toggles on <localleader>z for relevant modes
  (jds/localleader-def
    :keymaps '(LaTeX-mode-map org-mode-map)
    "z" evil-tex-toggle-map))


;;; setup company-auctex and company-reftex (and perhaps company-math)

(use-package company-auctex
   :hook (LaTeX-mode . evil-tex-mode)
  :config
  (add-to-list 'company-backends
	     '(company-auctex-macros company-auctex-symbols company-auctex-environments))
  ;; (company-auctex-init)
  )

;; Nicely indent lines that have wrapped when visual line mode is activated
(use-package adaptive-wrap
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init (setq-default adaptive-wrap-extra-indent 0))




;;; setup reftex corfu interface
(use-package company-reftex)


(defun jds~setup-capf-latex ()
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
	       (cape-company-to-capf #'company-reftex-citations))
  (add-to-list 'completion-at-point-functions
	       (cape-company-to-capf #'company-reftex-labels)))

(add-hook 'LaTeX-mode-hook 'jds~setup-capf-latex)


;;;###autoload
(defun latex-word-count ()
  (interactive)
  (shell-command (concat "/sbin/texcount "
                         ; "uncomment then options go here "
                         (buffer-file-name))))

;;;###autoload
(defun jds/tex-command-run-all-and-save (&optional force-run-all)
  "Save the current buffer, then run the appropriate LaTeX compile command.

Prefer the current default TeX command, which is `LatexMk' in
LaTeX buffers, so `mm' saves and runs the compiler directly
without opening AUCTeX's command chooser.  This avoids layering
`TeX-command-run-all' on top of latexmk's own rerun logic.  With
prefix argument FORCE-RUN-ALL, always use `TeX-command-run-all'.
Without a prefix argument, open the compiled document after a
successful build."
  (interactive "P")
  (save-buffer)
  (if force-run-all
      (call-interactively #'TeX-command-run-all)
    (progn
      (TeX-master-file nil nil t)
      (TeX-command-sequence
       (list TeX-command-default TeX-command-Show) t #'TeX-master-file))))

;;; setup latexdiff

(declare-function latexdiff--check-if-installed "latexdiff")
(declare-function latexdiff-vc--latexdiff-sentinel "latexdiff")
(defvar latexdiff-runningp)
(defvar latexdiff-vc-args)

(use-package latexdiff
  :straight (:type git :host github :repo "galaunay/latexdiff.el")
  :defer t)

(defun jds/latex--buffer-file-directory ()
  "Return the directory of the current LaTeX buffer's file."
  (or (and buffer-file-name (file-name-directory buffer-file-name))
      (user-error "Current buffer is not visiting a file")))

(defun jds/latex--diff-process-environment (root)
  "Return search-path environment entries for ROOT-based LaTeX builds.

    TEXINPUTS is recursive so local class/style files and inputs under the
project tree remain visible when the diff is compiled from a generated
subdirectory."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (list (format "TEXINPUTS=.:%s//:" root)
          (format "BIBINPUTS=.:%s:" root)
          (format "BSTINPUTS=.:%s:" root))))

(defun jds/latex--diff-compile-command (root diff-dir file)
  "Return the shell command that compiles FILE from ROOT into DIFF-DIR.

Prefer `latexmk' when it is available so bibliography and rerun
dependencies are handled automatically."
  (let* ((diff-file
          (file-relative-name
           (expand-file-name (format "%s.tex" file) diff-dir)
           root))
         ;; Compile from the project root so relative assets like ./figures
         ;; and bibliography files resolve the same way as the main document.
         (compiler
          (if (executable-find "latexmk")
              (format "latexmk -pdf -interaction=nonstopmode -halt-on-error -f -outdir=%s %s"
                      (shell-quote-argument diff-dir)
                      (shell-quote-argument diff-file))
            (format "pdflatex -interaction=nonstopmode -halt-on-error -output-directory=%s %s"
                    (shell-quote-argument diff-dir)
                    (shell-quote-argument diff-file)))))
    (format "cd %s && %s >> %s 2>&1"
            (shell-quote-argument root)
            compiler
            (shell-quote-argument (expand-file-name "latexdiff.log" root)))))

(defun jds/latex--completion-table-with-metadata (collection metadata)
  "Return a completion table for COLLECTION with METADATA.

This is a small compatibility shim for Emacs builds that do not
provide `completion-table-with-metadata'."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata ,@metadata)
      (complete-with-action action collection string pred))))

(defun jds/latex--git-commit-candidates (directory file)
  "Return completion candidates for FILE's git history in DIRECTORY.

Candidates are ordered newest-first and include the commit timestamp,
committer name, short hash, and subject so same-day commits are easy to
distinguish."
  (let ((default-directory directory)
        (lines (condition-case nil
                   (process-lines
                    "git" "log" "--follow"
                    "--pretty=format:%H%x09%at%x09%an%x09%h%x09%s"
                    "--" file)
                 (error nil)))
        (entries nil))
    (dolist (line lines)
      (let* ((fields (split-string line "\t"))
             (hash (nth 0 fields))
             (time (nth 1 fields))
             (author (nth 2 fields))
             (short-hash (nth 3 fields))
             (subject (nth 4 fields)))
        (when hash
          (push (list :hash hash
                      :time (string-to-number time)
                      :author author
                      :short-hash short-hash
                      :subject subject
                      :date (format-time-string
                             "%Y-%m-%d %H:%M"
                             (seconds-to-time (string-to-number time))))
                entries))))
    (let* ((entries (sort entries (lambda (a b)
                                    (> (plist-get a :time)
                                       (plist-get b :time)))))
           (date-width (apply #'max 0 (mapcar (lambda (entry)
                                                (length (plist-get entry :date)))
                                              entries)))
           (author-width (apply #'max 0 (mapcar (lambda (entry)
                                                  (length (plist-get entry :author)))
                                                entries)))
           (hash-width (apply #'max 0 (mapcar (lambda (entry)
                                                (length (plist-get entry :short-hash)))
                                              entries))))
      (mapcar (lambda (entry)
                (cons (format (format "%%-%ds | %%-%ds | %%-%ds | %%s"
                                      date-width author-width hash-width)
                              (plist-get entry :date)
                              (plist-get entry :author)
                              (plist-get entry :short-hash)
                              (plist-get entry :subject))
                      (plist-get entry :hash)))
              entries))))

(defun jds/latexdiff-vc--compile-with-current (REV)
  "Generate diff of current file against REV and compile PDF inside the diff directory."
  (let* ((buffer-dir (jds/latex--buffer-file-directory))
         (file (file-name-base buffer-file-name))
         (diff-dir (expand-file-name (format "diff%s" REV) buffer-dir)))
    (latexdiff--check-if-installed)
    (setq latexdiff-runningp t)
    (message "[%s] Generating latex diff with %s" file REV)
    (let* ((process-environment
            (append (jds/latex--diff-process-environment buffer-dir)
                    process-environment))
           (process
            (start-process
             "latexdiff" " *latexdiff*" "/bin/sh" "-c"
             (format "cd %s && yes X | latexdiff-vc --dir --force --git %s -r %s %s.tex > latexdiff.log 2>&1 && %s"
                     (shell-quote-argument buffer-dir)
                     (mapconcat #'shell-quote-argument latexdiff-vc-args " ")
                     (shell-quote-argument REV)
                     (shell-quote-argument file)
                     (jds/latex--diff-compile-command buffer-dir diff-dir file)))))
      (process-put process 'diff-dir diff-dir)
      (process-put process 'file file)
      (process-put process 'rev1 "current")
      (process-put process 'rev2 REV)
      (set-process-sentinel process #'latexdiff-vc--latexdiff-sentinel)
      diff-dir)))

(defun jds/latexdiff-vc ()
  "Run latexdiff-vc, compiling the diff PDF inside the diff directory.
Sets TEXINPUTS so pdflatex can find class/style/input files from the project root."
  (interactive)
  (require 'latexdiff)
  (let* ((buffer-dir (jds/latex--buffer-file-directory))
         (process-environment
          (append (jds/latex--diff-process-environment buffer-dir)
                  process-environment))
         (git-file (file-relative-name buffer-file-name buffer-dir))
         (commits (jds/latex--git-commit-candidates buffer-dir git-file)))
    (unless commits
      (user-error "No git history found for %s" git-file))
    (let* ((commit-table (jds/latex--completion-table-with-metadata
                          commits
                          '((display-sort-function . identity)
                            (cycle-sort-function . identity))))
           (commit (completing-read "Choose a commit (newest first): "
                                    commit-table nil t nil nil (caar commits)))
           (commit-hash (cdr (assoc commit commits))))
      (jds/latexdiff-vc--compile-with-current commit-hash))))

;;; keybindings

(jds/localleader-def
  :keymaps 'LaTeX-mode-map
  "d" '(:ignore t :which-key "diff")
  "df" #'latexdiff
  "dv" #'jds/latexdiff-vc
  "m" #'jds/tex-command-run-all-and-save
  "e" #'LaTeX-environment
  "s" #'LaTeX-section
  "C" #'LaTeX-close-environment
  "n" #'TeX-next-error
  "N" #'TeX-previous-error
  ;; "i" #'LaTeX-insert-item
  "t" #'reftex-toc)

(jds/localleader-def
  :keymaps '(LaTeX-mode-map)
  ;; "c" jds/citation-map ; reserved
  "l" #'reftex-label)

(jds/localleader-def
  :keymaps 'bibtex-mode-map
  "c" '(:ignore t))


;; some key bindings that should be in org and latex
(general-define-key
 :keymaps '(LaTeX-mode-map org-mode-map org-capture-mode-map)
 :states 'normal
 "[m" #'jds/latex-previous-math-start
 "]m" #'jds/latex-next-math-start)

(jds/localleader-def
 :keymaps '(LaTeX-mode-map org-mode-map org-capture-mode-map)
 :states 'normal
 "g" '(:ignore t :which-key "goto") 
 "gm" #'jds/avy-latex-math)
