;;; lsp.el ---  Language Server Protocol setup -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'xref)

;; Eglot is built-in to Emacs 30+. Keybindings are in bindings.el under SPC e.
;; Prerequisites:
;;   Python: install pyright (npm i -g pyright) or pylsp (pip install python-lsp-server)
;;   R: install.packages("languageserver") in R
;;   LaTeX: install texlab (optional, uncomment below)

;; Keep the global clangd invocation small. For Rcpp work, clangd can still be
;; useful for symbol indexing and navigation even when the project doesn't ship
;; extra clang metadata. If a compilation database or `.clangd` file exists,
;; clangd will use it; this config doesn't require either one.
(defconst jds/clangd-rcpp-args
  '("--header-insertion=never")
  "Arguments passed to clangd for C/C++ buffers.

Keep this list minimal so project-local clang settings can win when present.")

(defvar jds/eglot--missing-clangd-warned nil
  "Non-nil after warning once that `clangd' is unavailable.")


(use-package eglot
  :straight (:type built-in)
  ;; Hook both python-mode and python-ts-mode since treesit-auto may remap;
  ;; only one fires per buffer.
  :hook ((python-mode . eglot-ensure)
	 (python-ts-mode . eglot-ensure)
	 (ess-r-mode . eglot-ensure)
	 (c-mode . eglot-ensure)
	 (c++-mode . eglot-ensure)
	 (c-ts-mode . eglot-ensure)
	 (c++-ts-mode . eglot-ensure)
	 (LaTeX-mode . eglot-ensure))
  :config
  (setq eldoc-echo-area-use-multiline-p 1)

  ;; R language server (requires R package: install.packages("languageserver"))
  (add-to-list 'eglot-server-programs
	       '(ess-r-mode . ("R" "--slave" "-e" "languageserver::run()")))

  ;; clangd handles both classic cc-mode buffers and tree-sitter C/C++ buffers.
  ;; Keep the baseline small and let project-local settings refine it when
  ;; available.
  (when (executable-find "clangd")
    (add-to-list 'eglot-server-programs
	         `(,(list 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode)
		   . ("clangd" ,@jds/clangd-rcpp-args))))

  ;; Python: pyright is auto-detected by eglot, no explicit entry needed.
  ;; If you prefer pylsp, uncomment:
  ;; (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pylsp")))

  ;; LaTeX: texlab (uncomment to enable; install texlab first)
  (add-to-list 'eglot-server-programs
               '((tex-mode context-mode texinfo-mode bibtex-mode LaTeX-mode latex-mode)
                 . ("texlab")))
  )

;; `consult-eglot-symbols` already queries all running Eglot servers for the
;; current project, so it remains the right command for project-wide symbol
;; lookup under SPC e e.
(use-package consult-eglot
  :after eglot
  :commands consult-eglot-symbols)

(defvar eglot-managed-mode)
(defvar eglot--cached-server)
(defvar eglot--servers-by-project)
(defvar-local jds/eglot-companion-buffer nil)
(defvar-local jds/eglot-project-setup-timer nil)
(defvar-local jds/eglot-project-setup-attempts 0)
(defvar jds/eglot--pending-companion-starts (make-hash-table :test #'equal)
  "In-flight companion server starts keyed by package root and server kind.")

(declare-function eglot-current-server "eglot")
(declare-function eglot "eglot")
(declare-function eglot-ensure "eglot")
(declare-function eglot-server-capable "eglot")
(declare-function eglot--guess-contact "eglot")
(declare-function eglot--major-modes "eglot")
(declare-function eglot--request "eglot")
(declare-function eglot-uri-to-path "eglot")
(declare-function project-current "project")

(defconst jds/eglot--rcpp-header-extensions
  '("h" "hh" "hpp" "hxx" "ipp" "tpp")
  "Header-like file extensions used by Rcpp package fallback lookup.")

(defconst jds/eglot--rcpp-source-extensions
  '("c" "cc" "cpp" "cxx" "m" "mm")
  "Source file extensions used by Rcpp package fallback lookup.")

(defconst jds/eglot--cpp-keywords
  '("alignas" "alignof" "and" "asm" "auto" "bitand" "bitor" "break" "case"
    "catch" "class" "compl" "concept" "const" "consteval" "constexpr"
    "constinit" "const_cast" "continue" "co_await" "co_return" "co_yield"
    "decltype" "default" "delete" "do" "dynamic_cast" "else" "enum"
    "explicit" "export" "extern" "false" "for" "friend" "goto" "if"
    "inline" "mutable" "namespace" "new" "noexcept" "not" "nullptr"
    "operator" "or" "private" "protected" "public" "register"
    "reinterpret_cast" "requires" "return" "sizeof" "static"
    "static_assert" "static_cast" "struct" "switch" "template" "this"
    "throw" "true" "try" "typedef" "typename" "union" "using" "virtual"
    "volatile" "while" "xor")
  "C++ keywords that should never be treated as symbol names.")

(defvar jds/eglot--rcpp-symbol-cache (make-hash-table :test #'equal)
  "Cache of textual symbol indexes for Rcpp package trees.")

(defcustom jds/eglot-auto-start-rcpp-companion-servers t
  "When non-nil, start both R and C++ Eglot servers for Rcpp packages.

If a project root contains `DESCRIPTION' and appears to have a C++ backend
under `src/', `include/', or `inst/include/', visiting either an R or C/C++
buffer will start the missing companion server in a buried project buffer."
  :type 'boolean
  :group 'eglot)

(defcustom jds/eglot-project-setup-retries 10
  "How many times to retry project Eglot setup after opening a buffer.

This covers the gap between visiting a file and Eglot attaching a server to the
buffer, which can happen asynchronously."
  :type 'integer
  :group 'eglot)

(defun jds/eglot--description-root (&optional dir)
  "Return the nearest parent directory of DIR containing `DESCRIPTION'."
  (when-let ((root (locate-dominating-file (or dir default-directory) "DESCRIPTION")))
    (expand-file-name root)))

(defun jds/eglot--first-file-matching (directory regexp)
  "Return the first file under DIRECTORY matching REGEXP."
  (when (file-directory-p directory)
    (car (directory-files-recursively directory regexp nil nil t))))

(defun jds/eglot--rcpp-directories (root &optional headers-only)
  "Return relevant C/C++ directories under the R package ROOT.

When HEADERS-ONLY is non-nil, omit `src/' so header fallback stays focused on
R-package header trees."
  (cl-remove-if-not
   #'file-directory-p
   (append
    (unless headers-only
      (list (expand-file-name "src" root)))
    (list (expand-file-name "inst/include" root)
          (expand-file-name "include" root)))))

(defun jds/eglot--rcpp-files (root &optional headers-only)
  "Return relevant C/C++ files under the R package ROOT.

When HEADERS-ONLY is non-nil, only include header-like files."
  (let ((regexp (format "\\.\\(%s\\)$"
                        (string-join
                         (if headers-only
                             jds/eglot--rcpp-header-extensions
                           (append jds/eglot--rcpp-source-extensions
                                   jds/eglot--rcpp-header-extensions))
                         "\\|"))))
    (cl-loop for directory in (jds/eglot--rcpp-directories root headers-only)
             append (directory-files-recursively directory regexp nil nil t))))

(defun jds/eglot--rcpp-backend-file (root)
  "Return one representative C/C++ file for the R package at ROOT."
  (or (jds/eglot--first-file-matching
       (expand-file-name "src" root)
       "\\.\\(cc\\|cpp\\|cxx\\|c\\)$")
      (jds/eglot--first-file-matching
       (expand-file-name "inst/include" root)
       "\\.\\(h\\|hh\\|hpp\\|hxx\\|ipp\\|tpp\\)$")
      (jds/eglot--first-file-matching
       (expand-file-name "include" root)
       "\\.\\(h\\|hh\\|hpp\\|hxx\\|ipp\\|tpp\\)$")
      (jds/eglot--first-file-matching
       (expand-file-name "src" root)
       "\\.\\(h\\|hh\\|hpp\\|hxx\\|ipp\\|tpp\\)$")))

(defun jds/eglot--r-package-file (root)
  "Return one representative R source file for the package at ROOT."
  (jds/eglot--first-file-matching
   (expand-file-name "R" root)
   "\\.[rR]$"))

(defun jds/eglot--companion-key (root kind)
  "Return a stable key for the companion server KIND at package ROOT."
  (list (expand-file-name root) kind))

(defun jds/eglot--pending-companion-start-p (root kind)
  "Return non-nil when companion server KIND for ROOT is already starting."
  (gethash (jds/eglot--companion-key root kind)
           jds/eglot--pending-companion-starts))

(defun jds/eglot--set-pending-companion-start (root kind pending)
  "Set companion server KIND pending state for ROOT to PENDING."
  (let ((key (jds/eglot--companion-key root kind)))
    (if pending
        (puthash key t jds/eglot--pending-companion-starts)
      (remhash key jds/eglot--pending-companion-starts))))

(defun jds/eglot--server-running-for-modes-p (modes)
  "Return non-nil when one current-project Eglot server manages MODES."
  (cl-some
   (lambda (server)
     (cl-intersection modes (eglot--major-modes server)))
   (jds/eglot--project-servers)))

(defun jds/eglot--warn-missing-clangd ()
  "Warn once that `clangd' is required for C/C++ Eglot support."
  (unless jds/eglot--missing-clangd-warned
    (setq jds/eglot--missing-clangd-warned t)
    (message "Rcpp companion server skipped: `clangd` is not installed")))

(defun jds/eglot--current-server ()
  "Return the current buffer's Eglot server, or nil when unmanaged."
  (ignore-errors (eglot-current-server)))

(defun jds/eglot--start-server-for-file (file root kind)
  "Start companion server KIND for FILE in package ROOT."
  (jds/eglot--set-pending-companion-start root kind t)
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (setq-local jds/eglot-companion-buffer t)
      (unless (jds/eglot--current-server)
        (condition-case err
            ;; `eglot-ensure' is not enough here: once an R server already
            ;; exists for the project, it may decide the buffer is already
            ;; "covered" and skip starting the C/C++ companion server. Force
            ;; the contact for the target file instead.
            (apply #'eglot (eglot--guess-contact))
          (error
           (jds/eglot--set-pending-companion-start root kind nil)
           (message "Eglot companion start failed for %s: %s"
                    (abbreviate-file-name file)
                    (error-message-string err))))))
    (bury-buffer buffer)))

(defun jds/eglot-maybe-start-rcpp-companion-server ()
  "Start the missing R or C++ Eglot server for the current Rcpp package."
  (when (and jds/eglot-auto-start-rcpp-companion-servers
             (jds/eglot--current-server)
             (not jds/eglot-companion-buffer)
             (derived-mode-p 'ess-r-mode 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode))
    (when-let* ((root (jds/eglot--description-root))
                (cpp-file (jds/eglot--rcpp-backend-file root)))
      (cond
       ((derived-mode-p 'ess-r-mode)
        (cond
         ((not (executable-find "clangd"))
          (jds/eglot--warn-missing-clangd))
         ((jds/eglot--server-running-for-modes-p
           '(c-mode c++-mode c-ts-mode c++-ts-mode))
          (jds/eglot--set-pending-companion-start root 'cpp nil))
         ((not (jds/eglot--pending-companion-start-p root 'cpp))
          (jds/eglot--start-server-for-file cpp-file root 'cpp))))
       ((derived-mode-p 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode)
        (cond
         ((jds/eglot--server-running-for-modes-p '(ess-r-mode))
          (jds/eglot--set-pending-companion-start root 'r nil))
         ((not (jds/eglot--pending-companion-start-p root 'r))
          (when-let ((r-file (jds/eglot--r-package-file root)))
            (jds/eglot--start-server-for-file r-file root 'r)))))))))

(defun jds/eglot-install-project-xref-backend ()
  "Install a project-aware Eglot xref backend in R and C/C++ buffers."
  (when (and (jds/eglot--current-server)
             (derived-mode-p 'ess-r-mode 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode))
    ;; Put the mixed-project backend first so `xref-find-definitions' reaches
    ;; it before backend-specific fallbacks such as dumb-jump.
    (setq-local xref-backend-functions
                (cons #'jds/eglot-project-xref-backend
                      (remq #'jds/eglot-project-xref-backend
                            xref-backend-functions)))))

(defun jds/eglot--relevant-rcpp-mode-p ()
  "Return non-nil when the current buffer should participate in mixed R/C++ LSP."
  (derived-mode-p 'ess-r-mode 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode))

(defun jds/eglot--finalize-project-setup (buffer)
  "Finish mixed-project Eglot setup for BUFFER once a server is available."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((not (jds/eglot--relevant-rcpp-mode-p))
        (setq-local jds/eglot-project-setup-timer nil
                    jds/eglot-project-setup-attempts 0))
       ((jds/eglot--current-server)
        (setq-local jds/eglot-project-setup-timer nil
                    jds/eglot-project-setup-attempts 0)
        (jds/eglot-install-project-xref-backend)
        (jds/eglot-maybe-start-rcpp-companion-server))
       ((> jds/eglot-project-setup-attempts 0)
        (cl-decf jds/eglot-project-setup-attempts)
        (setq-local jds/eglot-project-setup-timer
                    (run-at-time
                     0.5 nil #'jds/eglot--finalize-project-setup buffer)))
       (t
        (setq-local jds/eglot-project-setup-timer nil))))))

(defun jds/eglot-schedule-project-setup ()
  "Retry mixed-project Eglot setup until the current buffer has a server."
  (when (jds/eglot--relevant-rcpp-mode-p)
    (when (timerp jds/eglot-project-setup-timer)
      (cancel-timer jds/eglot-project-setup-timer))
    (setq-local jds/eglot-project-setup-attempts jds/eglot-project-setup-retries)
    (setq-local jds/eglot-project-setup-timer
                (run-at-time
                 0.5 nil #'jds/eglot--finalize-project-setup (current-buffer)))))

(defun jds/eglot-project-xref-backend ()
  "Return the multi-server Eglot xref backend for mixed R/C++ projects."
  'jds-eglot-project)

(defun jds/eglot--project-servers (&optional capability)
  "Return live Eglot servers for the current project.

When CAPABILITY is non-nil, only keep servers advertising that capability."
  (let* ((project (project-current nil))
         (servers (if-let ((project-servers (and project
                                                 (gethash project eglot--servers-by-project))))
                      project-servers
                    (list (eglot-current-server)))))
    (cl-delete-duplicates
     (cl-remove-if-not
      (lambda (server)
        (and server
             (let ((eglot--cached-server server))
               (or (null capability)
                   (eglot-server-capable capability)))))
      servers)
     :test #'eq)))

(defun jds/eglot--workspace-symbol-name-match-p (candidate identifier)
  "Return non-nil when CANDIDATE is an exact-ish match for IDENTIFIER."
  (or (string= candidate identifier)
      (string-suffix-p (concat "::" identifier) candidate)))

(defun jds/eglot--workspace-symbol-xref (symbol identifier)
  "Convert SYMBOL from `workspace/symbol' into an xref for IDENTIFIER."
  (let* ((name (plist-get symbol :name))
         (location (plist-get symbol :location))
         (range (and location (plist-get location :range)))
         (start (and range (plist-get range :start)))
         (uri (and location (plist-get location :uri))))
    (when (and name
               uri
               start
               (jds/eglot--workspace-symbol-name-match-p name identifier))
      (xref-make
       name
       (xref-make-file-location
        (eglot-uri-to-path uri)
        (1+ (plist-get start :line))
        (plist-get start :character))))))

(defun jds/eglot--workspace-symbol-definitions (identifier)
  "Return project-wide workspace-symbol xrefs for IDENTIFIER."
  (cl-loop
   for server in (jds/eglot--project-servers :workspaceSymbolProvider)
   append
   (mapcan
    (lambda (symbol)
      (when-let ((xref (jds/eglot--workspace-symbol-xref symbol identifier)))
        (list xref)))
    (append (ignore-errors
              (eglot--request server :workspace/symbol `(:query ,identifier)))
            nil))))

(defun jds/eglot--cpp-symbol-keyword-p (symbol)
  "Return non-nil when SYMBOL is a C++ keyword."
  (member symbol jds/eglot--cpp-keywords))

(defun jds/eglot--rcpp-symbol-kind-for-line (line)
  "Return an LSP-like symbol kind for LINE."
  (cond
   ((string-match-p "^[[:space:]]*namespace\\_>" line) 3)
   ((string-match-p "^[[:space:]]*struct\\_>" line) 23)
   ((string-match-p "^[[:space:]]*\\(?:enum\\(?:[[:space:]]+class\\)?\\)\\_>" line) 10)
   ((string-match-p "^[[:space:]]*class\\_>" line) 5)
   (t 12)))

(defun jds/eglot--candidate-symbol-from-line (line)
  "Extract one likely symbol name from LINE, or nil."
  (cond
   ((string-match
     "^[[:space:]]*\\(?:template\\_>.*\n[[:space:]]*\\)?\\(?:class\\|struct\\|namespace\\|enum\\(?:[[:space:]]+class\\)?\\)\\_>[[:space:]]+\\([[:alpha:]_][[:alnum:]_]*\\)"
     line)
    (match-string 1 line))
   ((string-match
     "^[[:space:]]*\\(?:template\\_>.*\n[[:space:]]*\\)?\\(?:[[[:space:][:word:]_:<>,*&~]]+[[:space:]]+\\)+\\([[:alpha:]_~][[:alnum:]_:~]*\\)[[:space:]]*("
     line)
    (match-string 1 line))
   (t nil)))

(defun jds/eglot--symbol-match-p (name query exact)
  "Return non-nil when NAME matches QUERY.

When EXACT is non-nil, require exact-ish symbol matching."
  (and name
       (not (jds/eglot--cpp-symbol-keyword-p name))
       (if exact
           (or (string= name query)
               (string-suffix-p (concat "::" query) name))
         (string-match-p (regexp-quote query) name))))

(defun jds/eglot--make-symbol-information (name kind file line column)
  "Build a SymbolInformation-like plist."
  `(:name ,name
    :kind ,kind
    :location (:uri ,(concat "file://" (expand-file-name file))
               :range (:start (:line ,line :character ,column)
                       :end (:line ,line :character ,column)))))

(defun jds/eglot--symbol-info-key (symbol)
  "Return a stable key for deduplicating SYMBOL."
  (let* ((location (plist-get symbol :location))
         (range (and location (plist-get location :range)))
         (start (and range (plist-get range :start))))
    (list (plist-get symbol :name)
          (plist-get location :uri)
          (plist-get start :line)
          (plist-get start :character))))

(defun jds/eglot--dedupe-symbol-infos (symbols)
  "Deduplicate SYMBOLS while preserving order."
  (let ((seen (make-hash-table :test #'equal)))
    (cl-loop for symbol in symbols
             for key = (jds/eglot--symbol-info-key symbol)
             unless (gethash key seen)
             do (puthash key t seen)
             collect symbol)))

(defun jds/eglot--index-rcpp-file-symbols (file)
  "Return likely textual symbol definitions from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let (symbols)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (unless (string-match-p "^[[:space:]]*\\(?:#\\|//\\)" line)
            (when-let ((name (jds/eglot--candidate-symbol-from-line line)))
              (push (jds/eglot--make-symbol-information
                     name
                     (jds/eglot--rcpp-symbol-kind-for-line line)
                     file
                     (1- (line-number-at-pos (line-beginning-position)))
                     (max 0 (or (string-match (regexp-quote name) line) 0)))
                    symbols))))
        (forward-line 1))
      (nreverse symbols))))

(defun jds/eglot--rcpp-symbol-cache-key (root headers-only)
  "Return the cache key for ROOT and HEADERS-ONLY."
  (list (expand-file-name root) headers-only))

(defun jds/eglot--rcpp-symbol-index (root &optional headers-only)
  "Return a cached textual symbol index for the R package ROOT."
  (let* ((files (jds/eglot--rcpp-files root headers-only))
         (stamp (cl-loop for file in files
                         maximize (float-time
                                   (file-attribute-modification-time
                                    (file-attributes file)))
                         into newest
                         finally return newest))
         (cache-key (jds/eglot--rcpp-symbol-cache-key root headers-only))
         (cached (gethash cache-key jds/eglot--rcpp-symbol-cache)))
    (if (and cached (= (car cached) stamp))
        (cdr cached)
      (let ((symbols (cl-loop for file in files
                              append (jds/eglot--index-rcpp-file-symbols file))))
        (puthash cache-key (cons stamp symbols) jds/eglot--rcpp-symbol-cache)
        symbols))))

(defun jds/eglot--rcpp-text-symbols (query &optional exact headers-only)
  "Return textual fallback symbols for QUERY in the current Rcpp package.

When EXACT is non-nil, only keep exact-ish symbol matches. When HEADERS-ONLY is
non-nil, only search header-like package files."
  (when-let ((root (jds/eglot--description-root)))
    (cl-loop for symbol in (jds/eglot--rcpp-symbol-index root headers-only)
             when (jds/eglot--symbol-match-p (plist-get symbol :name) query exact)
             collect symbol)))

(defun jds/eglot--text-symbol-xrefs (identifier)
  "Return textual fallback xrefs for IDENTIFIER in package headers."
  (mapcar (lambda (symbol)
            (jds/eglot--workspace-symbol-xref symbol identifier))
          (jds/eglot--rcpp-text-symbols identifier t t)))

(defun jds/eglot--xref-key (xref)
  "Return a stable key for deduplicating XREF."
  (let ((location (xref-item-location xref)))
    (if (xref-file-location-p location)
        (list (xref-file-location-file location)
              (xref-file-location-line location)
              (xref-file-location-column location))
      (list (xref-item-summary xref)))))

(defun jds/eglot--dedupe-xrefs (xrefs)
  "Deduplicate XREFS while preserving order."
  (let ((seen (make-hash-table :test #'equal)))
    (cl-loop for xref in xrefs
             for key = (jds/eglot--xref-key xref)
             unless (gethash key seen)
             do (puthash key t seen)
             collect xref)))

(cl-defmethod xref-backend-definitions ((_backend (eql jds-eglot-project)) identifier)
  "Find IDENTIFIER definitions by combining local and project-wide Eglot data."
  (jds/eglot--dedupe-xrefs
   (append (ignore-errors (xref-backend-definitions 'eglot identifier))
           (jds/eglot--workspace-symbol-definitions identifier)
           (delq nil (jds/eglot--text-symbol-xrefs identifier)))))

(cl-defmethod xref-backend-references ((_backend (eql jds-eglot-project)) identifier)
  "Defer IDENTIFIER references to the current buffer's Eglot server."
  (ignore-errors (xref-backend-references 'eglot identifier)))

(add-hook 'eglot-managed-mode-hook #'jds/eglot-install-project-xref-backend)
(dolist (hook '(ess-r-mode-hook
                c-mode-hook
                c++-mode-hook
                c-ts-mode-hook
                c++-ts-mode-hook))
  (add-hook hook #'jds/eglot-schedule-project-setup))

;; `consult-eglot-symbols' is still the right UI for project-wide symbol search,
;; but clangd often omits `inst/include/' declarations from `workspace/symbol'
;; in plain R package workflows. Supplement its async source with a small local
;; textual index so the command can still surface package headers.
(defun jds/consult-eglot--augment-async-source (orig-fn servers)
  "Wrap ORIG-FN so `consult-eglot-symbols' also sees Rcpp package headers."
  (let ((source (funcall orig-fn servers)))
    (lambda (async)
      (let ((base-results nil)
            (fallback-results nil)
            (controller nil))
        (setq controller
              (funcall
               source
               (lambda (payload)
                 (if (listp payload)
                     (progn
                       (setq base-results payload)
                       (funcall async
                                (jds/eglot--dedupe-symbol-infos
                                 (append base-results fallback-results))))
                   (funcall async payload)))))
        (lambda (action)
          (pcase action
            ((pred stringp)
             (setq fallback-results
                   (unless (string-empty-p action)
                     (jds/eglot--rcpp-text-symbols action nil t)))
             (funcall async 'flush)
             (funcall async
                      (jds/eglot--dedupe-symbol-infos
                       (append base-results fallback-results))))
            ('setup
             (setq fallback-results nil)))
          (funcall controller action))))))

(with-eval-after-load 'consult-eglot
  (advice-add 'consult-eglot--make-async-source
              :around #'jds/consult-eglot--augment-async-source))
