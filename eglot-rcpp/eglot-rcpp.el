;;; eglot-rcpp.el --- Mixed R/Rcpp/C++ package support for Eglot -*- lexical-binding: t; -*-

;; Author: Justin Silverman <jsilve24@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, tools, convenience
;; URL: https://github.com/jsilve24/eglot-rcpp

;;; Commentary:

;; `eglot-rcpp' is an Eglot-centered frontend for mixed R / Rcpp / C++
;; package development.  It activates only in R package projects, starts the
;; relevant Eglot servers for R and C/C++ buffers, merges project-aware xref
;; data, and uses generated Rcpp bridge files for R-side completion without
;; letting those generated files dominate definition jumps by default.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'jsonrpc)
(require 'project)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'xref)

(defgroup eglot-rcpp nil
  "Mixed R / Rcpp / C++ package support built on top of Eglot."
  :group 'eglot)

(defcustom eglot-rcpp-root-marker-file "DESCRIPTION"
  "Project marker used to recognize R package roots."
  :type 'string
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-r-modes '(ess-r-mode)
  "Major modes that should use the R language server."
  :type '(repeat symbol)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-cpp-modes '(c-mode c++-mode c-ts-mode c++-ts-mode)
  "Major modes that should use clangd inside mixed package projects."
  :type '(repeat symbol)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-source-directories '("src")
  "Directories searched for Rcpp and C/C++ source files."
  :type '(repeat string)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-header-directories '("include" "inst/include")
  "Directories searched for package header files."
  :type '(repeat string)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-source-extensions '("c" "cc" "cpp" "cxx" "m" "mm")
  "Source file extensions searched when indexing package symbols."
  :type '(repeat string)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-header-extensions '("h" "hh" "hpp" "hxx" "ipp" "tpp")
  "Header file extensions searched when indexing package symbols."
  :type '(repeat string)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-auto-start-companion-servers t
  "When non-nil, start the companion Eglot server for the current package."
  :type 'boolean
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-project-setup-retries 10
  "How many times to retry mixed-project setup after opening a buffer."
  :type 'integer
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-r-server-command
  '("R" "--slave" "-e" "languageserver::run()")
  "Command used to start the R language server."
  :type '(repeat string)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-clangd-command '("clangd" "--header-insertion=never")
  "Command used to start clangd for package C/C++ buffers."
  :type '(repeat string)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-enable-project-root-markers t
  "When non-nil, teach `project.el' and `projectile' about `DESCRIPTION'."
  :type 'boolean
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-restrict-symbol-search-to-project t
  "When non-nil, keep symbol results inside the current package root."
  :type 'boolean
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-generated-file-regexps
  '("[/\\]R[/\\]RcppExports\\.R\\'"
    "[/\\]src[/\\]RcppExports\\.cpp\\'")
  "Regexps matching generated bridge files inside an Rcpp package."
  :type '(repeat regexp)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-generated-definition-policy 'deprioritize
  "How generated-file definition hits should be treated.

`deprioritize' keeps generated hits but sorts real source first.
`omit' removes generated hits when at least one real-source hit exists.
`keep' preserves generated hits in normal order after deduplication."
  :type '(choice (const :tag "Real source first" deprioritize)
                 (const :tag "Drop generated when real source exists" omit)
                 (const :tag "Keep generated hits" keep))
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-enable-ess-keybindings nil
  "When non-nil, install optional convenience bindings in ESS R buffers."
  :type 'boolean
  :group 'eglot-rcpp)

(defvar eglot-managed-mode)
(defvar eglot-server-programs)
(defvar projectile-project-root-files)
(defvar project-vc-extra-root-markers)

(defvar eglot-rcpp--hooks-installed nil
  "Non-nil after `eglot-rcpp' has installed its global hooks.")

(defvar eglot-rcpp--missing-clangd-warned nil
  "Non-nil after warning once that clangd is unavailable.")

(defvar eglot-rcpp--missing-r-warned nil
  "Non-nil after warning once that R is unavailable.")

(defvar eglot-rcpp--pending-companion-starts (make-hash-table :test #'equal)
  "In-flight companion starts keyed by package root and language kind.")

(defvar eglot-rcpp--symbol-cache (make-hash-table :test #'equal)
  "Cache of textual symbol indexes keyed by project root.")

(defvar eglot-rcpp--bridge-cache (make-hash-table :test #'equal)
  "Cache of parsed `RcppExports.R' completion candidates keyed by project root.")

(defvar eglot-rcpp-ess-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") #'eglot-rcpp-use-rcpp)
    (define-key map (kbd "c") #'eglot-rcpp-compile-attributes)
    (define-key map (kbd "d") #'eglot-rcpp-check-r-dependencies)
    map)
  "Optional ESS bindings for `eglot-rcpp' commands.")

(defvar-local eglot-rcpp-companion-buffer nil)
(defvar-local eglot-rcpp-project-setup-attempts 0)
(defvar-local eglot-rcpp-project-setup-timer nil)

(declare-function eglot-current-server "eglot")
(declare-function eglot-ensure "eglot")
(declare-function eglot-server-capable "eglot")

(defun eglot-rcpp--normalize-path (path)
  "Return a canonical absolute path for PATH, or nil."
  (when path
    (ignore-errors
      (file-truename (expand-file-name path)))))

(defun eglot-rcpp--uri-to-path (uri)
  "Return a local file path for URI, or nil when it is not a local file URI."
  (when (and (stringp uri) (string-prefix-p "file:" uri))
    (let* ((parsed (url-generic-parse-url uri))
           (host (url-host parsed))
           (path (url-unhex-string (or (url-filename parsed) ""))))
      (when (member host '(nil "" "localhost"))
        (if (and (eq system-type 'windows-nt)
                 (string-match-p "\\`/[[:alpha:]]:" path))
            (substring path 1)
          path)))))

(defun eglot-rcpp--file-uri (file)
  "Return a canonical file URI for FILE."
  (concat "file://" (or (eglot-rcpp--normalize-path file)
                        (expand-file-name file))))

(defun eglot-rcpp--mode-hook (mode)
  "Return the hook symbol for MODE."
  (intern (format "%s-hook" mode)))

(defun eglot-rcpp--all-managed-modes ()
  "Return all major modes managed by this package."
  (append eglot-rcpp-r-modes eglot-rcpp-cpp-modes))

(defun eglot-rcpp--relevant-mode-p (&optional mode)
  "Return non-nil when MODE or the current major mode is managed here."
  (let ((mode (or mode major-mode)))
    (or (memq mode eglot-rcpp-r-modes)
        (memq mode eglot-rcpp-cpp-modes)
        (apply #'derived-mode-p (eglot-rcpp--all-managed-modes)))))

(defun eglot-rcpp--buffer-kind (&optional buffer)
  "Return the language kind for BUFFER as `r', `cpp', or nil."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((apply #'derived-mode-p eglot-rcpp-r-modes) 'r)
     ((apply #'derived-mode-p eglot-rcpp-cpp-modes) 'cpp)
     (t nil))))

(defun eglot-rcpp--description-root (&optional dir)
  "Return the nearest parent directory of DIR containing `DESCRIPTION'."
  (when-let ((root (locate-dominating-file (or dir default-directory)
                                           eglot-rcpp-root-marker-file)))
    (directory-file-name
     (or (eglot-rcpp--normalize-path root)
         (expand-file-name root)))))

(defun eglot-rcpp--project-root (&optional buffer)
  "Return the package root for BUFFER, or nil."
  (with-current-buffer (or buffer (current-buffer))
    (when buffer-file-name
      (eglot-rcpp--description-root (file-name-directory buffer-file-name)))))

(defun eglot-rcpp--eligible-buffer-p (&optional buffer)
  "Return non-nil when BUFFER should activate `eglot-rcpp'."
  (with-current-buffer (or buffer (current-buffer))
    (and buffer-file-name
         (eglot-rcpp--relevant-mode-p)
         (eglot-rcpp--project-root))))

(defun eglot-rcpp--within-root-p (path root)
  "Return non-nil when PATH is inside ROOT."
  (when (and path root)
    (let ((path (file-name-as-directory
                 (file-name-directory
                  (or (eglot-rcpp--normalize-path path)
                      (expand-file-name path)))))
          (root (file-name-as-directory
                 (or (eglot-rcpp--normalize-path root)
                     (expand-file-name root)))))
      (string-prefix-p root path))))

(defun eglot-rcpp--generated-file-p (path)
  "Return non-nil when PATH matches a generated bridge file."
  (let ((path (or (eglot-rcpp--normalize-path path)
                  (and path (expand-file-name path)))))
    (and path
         (cl-some (lambda (regexp)
                    (string-match-p regexp path))
                  eglot-rcpp-generated-file-regexps))))

(defun eglot-rcpp--current-server ()
  "Return the current buffer's Eglot server, or nil."
  (ignore-errors (eglot-current-server)))

(defun eglot-rcpp--first-file-matching (directory regexp)
  "Return the first file under DIRECTORY matching REGEXP."
  (when (file-directory-p directory)
    (car (directory-files-recursively directory regexp nil nil t))))

(defun eglot-rcpp--first-useful-file-matching (directory regexp)
  "Return the first non-generated file under DIRECTORY matching REGEXP."
  (when (file-directory-p directory)
    (cl-find-if-not #'eglot-rcpp--generated-file-p
                    (directory-files-recursively directory regexp nil nil t))))

(defun eglot-rcpp--directories (root directories)
  "Return existing subdirectories in ROOT listed by DIRECTORIES."
  (cl-remove-if-not
   #'file-directory-p
   (mapcar (lambda (directory)
             (expand-file-name directory root))
           directories)))

(defun eglot-rcpp--package-directories (root &optional headers-only)
  "Return relevant package directories under ROOT.

When HEADERS-ONLY is non-nil, omit source directories."
  (append (unless headers-only
            (eglot-rcpp--directories root eglot-rcpp-source-directories))
          (eglot-rcpp--directories root eglot-rcpp-header-directories)))

(defun eglot-rcpp--files (root &optional headers-only include-generated)
  "Return relevant source or header files under ROOT.

When HEADERS-ONLY is non-nil, omit source directories.
When INCLUDE-GENERATED is non-nil, keep generated bridge files."
  (let ((regexp (format "\\.\\(%s\\)\\'"
                        (string-join
                         (if headers-only
                             eglot-rcpp-header-extensions
                           (append eglot-rcpp-source-extensions
                                   eglot-rcpp-header-extensions))
                         "\\|"))))
    (cl-loop for directory in (eglot-rcpp--package-directories root headers-only)
             append (cl-loop for file in (directory-files-recursively directory regexp nil nil t)
                             unless (and (not include-generated)
                                         (eglot-rcpp--generated-file-p file))
                             collect (or (eglot-rcpp--normalize-path file) file)))))

(defun eglot-rcpp--backend-file (root)
  "Return one representative C/C++ file for the package at ROOT."
  (or (eglot-rcpp--first-useful-file-matching
       (expand-file-name "src" root)
       "\\.\\(cc\\|cpp\\|cxx\\|c\\)\\'")
      (eglot-rcpp--first-useful-file-matching
       (expand-file-name "inst/include" root)
       "\\.\\(h\\|hh\\|hpp\\|hxx\\|ipp\\|tpp\\)\\'")
      (eglot-rcpp--first-useful-file-matching
       (expand-file-name "include" root)
       "\\.\\(h\\|hh\\|hpp\\|hxx\\|ipp\\|tpp\\)\\'")
      (eglot-rcpp--first-useful-file-matching
       (expand-file-name "src" root)
       "\\.\\(h\\|hh\\|hpp\\|hxx\\|ipp\\|tpp\\)\\'")))

(defun eglot-rcpp--r-package-file (root)
  "Return one representative R source file for the package at ROOT."
  (eglot-rcpp--first-file-matching
   (expand-file-name "R" root)
   "\\.[rR]\\'"))

(defun eglot-rcpp--r-bridge-file (root)
  "Return the generated R bridge file for ROOT."
  (expand-file-name "R/RcppExports.R" root))

(defun eglot-rcpp--cpp-bridge-file (root)
  "Return the generated C++ bridge file for ROOT."
  (expand-file-name "src/RcppExports.cpp" root))

(defun eglot-rcpp--cpp-files-present-p (root)
  "Return non-nil when ROOT contains any relevant C/C++ file."
  (and root (eglot-rcpp--backend-file root)))

(defun eglot-rcpp--r-files-present-p (root)
  "Return non-nil when ROOT contains any R package file."
  (and root (eglot-rcpp--r-package-file root)))

(defun eglot-rcpp--r-executable ()
  "Return the configured R executable, or nil when unavailable."
  (when-let ((command (car-safe eglot-rcpp-r-server-command)))
    (executable-find command)))

(defun eglot-rcpp--command-available-p (command)
  "Return non-nil when COMMAND appears runnable."
  (and (listp command)
       (car command)
       (executable-find (car command))))

(defun eglot-rcpp--warn-missing-clangd ()
  "Warn once that clangd is required for package C/C++ support."
  (unless eglot-rcpp--missing-clangd-warned
    (setq eglot-rcpp--missing-clangd-warned t)
    (message "eglot-rcpp: companion clangd start skipped because `clangd' is not installed")))

(defun eglot-rcpp--warn-missing-r ()
  "Warn once that R is required for package R support."
  (unless eglot-rcpp--missing-r-warned
    (setq eglot-rcpp--missing-r-warned t)
    (message "eglot-rcpp: companion R start skipped because `R' is not installed")))

(defun eglot-rcpp--companion-key (root kind)
  "Return a stable key for KIND under package ROOT."
  (list (expand-file-name root) kind))

(defun eglot-rcpp--pending-companion-start-p (root kind)
  "Return non-nil when companion KIND for ROOT is already starting."
  (gethash (eglot-rcpp--companion-key root kind)
           eglot-rcpp--pending-companion-starts))

(defun eglot-rcpp--set-pending-companion-start (root kind pending)
  "Set companion KIND pending state for ROOT to PENDING."
  (let ((key (eglot-rcpp--companion-key root kind)))
    (if pending
        (puthash key t eglot-rcpp--pending-companion-starts)
      (remhash key eglot-rcpp--pending-companion-starts))))

(defun eglot-rcpp--project-buffers (&optional root)
  "Return live `eglot-rcpp' buffers for ROOT.

When ROOT is nil, use the current buffer's project root."
  (let ((root (or root (eglot-rcpp--project-root))))
    (cl-loop for buffer in (buffer-list)
             when (and (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (and (bound-and-true-p eglot-rcpp-mode)
                              (equal (eglot-rcpp--project-root) root))))
             collect buffer)))

(defun eglot-rcpp--project-server-buffers (&optional root)
  "Return one live managed buffer per Eglot server for ROOT."
  (let ((seen (make-hash-table :test #'eq)))
    (cl-loop for buffer in (eglot-rcpp--project-buffers root)
             for server = (with-current-buffer buffer
                            (and (bound-and-true-p eglot-managed-mode)
                                 (eglot-rcpp--current-server)))
             unless (or (null server) (gethash server seen))
             do (puthash server t seen)
             and collect buffer)))

(defun eglot-rcpp--server-running-p (root kind)
  "Return non-nil when ROOT already has a live server for KIND."
  (cl-some (lambda (buffer)
             (eq (with-current-buffer buffer
                   (eglot-rcpp--buffer-kind))
                 kind))
           (eglot-rcpp--project-server-buffers root)))

(defun eglot-rcpp--companion-start-spec (&optional root kind)
  "Return a plist describing the companion server to start.

The plist has keys `:kind', `:file', and optionally `:warning'.  Return nil
when no companion start is needed."
  (let* ((root (or root (eglot-rcpp--project-root)))
         (kind (or kind (eglot-rcpp--buffer-kind))))
    (pcase kind
      ('r
       (let ((file (and root (eglot-rcpp--backend-file root))))
         (cond
          ((not (and eglot-rcpp-auto-start-companion-servers root file)) nil)
          ((eglot-rcpp--server-running-p root 'cpp)
           (eglot-rcpp--set-pending-companion-start root 'cpp nil)
           nil)
          ((eglot-rcpp--pending-companion-start-p root 'cpp) nil)
          ((not (eglot-rcpp--command-available-p eglot-rcpp-clangd-command))
           '(:warning missing-clangd))
          (t (list :kind 'cpp :file file)))))
      ('cpp
       (let ((file (and root (eglot-rcpp--r-package-file root))))
         (cond
          ((not (and eglot-rcpp-auto-start-companion-servers root file)) nil)
          ((eglot-rcpp--server-running-p root 'r)
           (eglot-rcpp--set-pending-companion-start root 'r nil)
           nil)
          ((eglot-rcpp--pending-companion-start-p root 'r) nil)
          ((not (eglot-rcpp--r-executable))
           '(:warning missing-r))
          (t (list :kind 'r :file file)))))
      (_ nil))))

(defun eglot-rcpp--start-server-for-file (file root kind)
  "Start companion server KIND for FILE in package ROOT."
  (eglot-rcpp--set-pending-companion-start root kind t)
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (setq-local eglot-rcpp-companion-buffer t)
      (condition-case err
          (eglot-ensure)
        (error
         (eglot-rcpp--set-pending-companion-start root kind nil)
         (message "eglot-rcpp: companion start failed for %s: %s"
                  (abbreviate-file-name file)
                  (error-message-string err)))))
    (bury-buffer buffer)))

(defun eglot-rcpp-maybe-start-companion-server ()
  "Start the missing companion Eglot server for the current package."
  (when-let* ((root (eglot-rcpp--project-root))
              (kind (eglot-rcpp--buffer-kind))
              (spec (and (eglot-rcpp--current-server)
                         (not eglot-rcpp-companion-buffer)
                         (eglot-rcpp--companion-start-spec root kind))))
    (pcase (plist-get spec :warning)
      ('missing-clangd (eglot-rcpp--warn-missing-clangd))
      ('missing-r (eglot-rcpp--warn-missing-r))
      (_ (eglot-rcpp--start-server-for-file
           (plist-get spec :file)
           root
           (plist-get spec :kind))))))

(defun eglot-rcpp--cancel-project-setup ()
  "Cancel the current buffer's deferred setup timer."
  (when (timerp eglot-rcpp-project-setup-timer)
    (cancel-timer eglot-rcpp-project-setup-timer))
  (setq-local eglot-rcpp-project-setup-timer nil
              eglot-rcpp-project-setup-attempts 0))

(defun eglot-rcpp--finalize-project-setup (buffer)
  "Finish mixed-project setup for BUFFER once a server is available."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((not (bound-and-true-p eglot-rcpp-mode))
        (eglot-rcpp--cancel-project-setup))
       ((eglot-rcpp--current-server)
        (eglot-rcpp--cancel-project-setup)
        (eglot-rcpp-install-project-xref-backend)
        (eglot-rcpp-maybe-start-companion-server))
       ((> eglot-rcpp-project-setup-attempts 0)
        (cl-decf eglot-rcpp-project-setup-attempts)
        (setq-local eglot-rcpp-project-setup-timer
                    (run-at-time
                     0.5 nil #'eglot-rcpp--finalize-project-setup buffer)))
       (t
        (eglot-rcpp--cancel-project-setup))))))

(defun eglot-rcpp-schedule-project-setup ()
  "Retry mixed-project setup until the current buffer has a server."
  (when (bound-and-true-p eglot-rcpp-mode)
    (eglot-rcpp--cancel-project-setup)
    (setq-local eglot-rcpp-project-setup-attempts eglot-rcpp-project-setup-retries
                eglot-rcpp-project-setup-timer
                (run-at-time 0.5 nil
                             #'eglot-rcpp--finalize-project-setup
                             (current-buffer)))))

(defun eglot-rcpp--xref-location-path (xref)
  "Return the file path referenced by XREF, or nil."
  (let ((location (xref-item-location xref)))
    (when (xref-file-location-p location)
      (xref-file-location-file location))))

(defun eglot-rcpp--xref-in-root-p (xref root)
  "Return non-nil when XREF points inside ROOT."
  (if (not eglot-rcpp-restrict-symbol-search-to-project)
      t
    (when-let ((path (eglot-rcpp--xref-location-path xref)))
      (eglot-rcpp--within-root-p path root))))

(defun eglot-rcpp--xref-generated-p (xref)
  "Return non-nil when XREF points to a generated bridge file."
  (when-let ((path (eglot-rcpp--xref-location-path xref)))
    (eglot-rcpp--generated-file-p path)))

(defun eglot-rcpp--xref-key (xref)
  "Return a stable key for deduplicating XREF."
  (if-let ((location (ignore-errors (xref-item-location xref))))
      (list (prin1-to-string location))
    (list (xref-item-summary xref))))

(defun eglot-rcpp--dedupe-xrefs (xrefs)
  "Deduplicate XREFS while preserving order."
  (let ((seen (make-hash-table :test #'equal)))
    (cl-loop for xref in xrefs
             for key = (eglot-rcpp--xref-key xref)
             unless (gethash key seen)
             collect xref
             and do (puthash key t seen))))

(defun eglot-rcpp--apply-generated-definition-policy (xrefs)
  "Apply `eglot-rcpp-generated-definition-policy' to XREFS."
  (let ((real (cl-remove-if #'eglot-rcpp--xref-generated-p xrefs))
        (generated (cl-remove-if-not #'eglot-rcpp--xref-generated-p xrefs)))
    (pcase eglot-rcpp-generated-definition-policy
      ('keep xrefs)
      ('omit (if real real xrefs))
      (_ (append real generated)))))

(defun eglot-rcpp--workspace-symbol-name-match-p (candidate identifier)
  "Return non-nil when CANDIDATE is an exact-ish match for IDENTIFIER."
  (or (string= candidate identifier)
      (string-suffix-p (concat "::" identifier) candidate)))

(defun eglot-rcpp--make-workspace-symbol-xref (symbol identifier)
  "Convert SYMBOL into an xref for IDENTIFIER, or nil."
  (let* ((name (plist-get symbol :name))
         (location (plist-get symbol :location))
         (range (and location (plist-get location :range)))
         (start (and range (plist-get range :start)))
         (uri (and location (plist-get location :uri))))
    (when (and name uri start
               (eglot-rcpp--workspace-symbol-name-match-p name identifier))
      (xref-make
       name
       (xref-make-file-location
        (eglot-rcpp--uri-to-path uri)
        (1+ (plist-get start :line))
        (plist-get start :character))))))

(defun eglot-rcpp--server-workspace-symbols (buffer query)
  "Return `workspace/symbol' results for QUERY using BUFFER's server."
  (with-current-buffer buffer
    (when (and (eglot-rcpp--current-server)
               (ignore-errors (eglot-server-capable :workspaceSymbolProvider)))
      (append (ignore-errors
                (jsonrpc-request (eglot-rcpp--current-server)
                                 :workspace/symbol
                                 `(:query ,query)))
              nil))))

(defun eglot-rcpp--workspace-symbol-definitions (identifier &optional root)
  "Return project-wide `workspace/symbol' xrefs for IDENTIFIER."
  (cl-loop for buffer in (eglot-rcpp--project-server-buffers root)
           append (cl-loop for symbol in (eglot-rcpp--server-workspace-symbols
                                          buffer identifier)
                           for xref = (eglot-rcpp--make-workspace-symbol-xref
                                       symbol identifier)
                           when (and xref
                                     (eglot-rcpp--xref-in-root-p xref root))
                           collect xref)))

(defun eglot-rcpp--cpp-symbol-keyword-p (symbol)
  "Return non-nil when SYMBOL is a C++ keyword."
  (member symbol
          '("alignas" "alignof" "and" "asm" "auto" "bitand" "bitor" "break"
            "case" "catch" "class" "compl" "concept" "const" "consteval"
            "constexpr" "constinit" "const_cast" "continue" "co_await"
            "co_return" "co_yield" "decltype" "default" "delete" "do"
            "dynamic_cast" "else" "enum" "explicit" "export" "extern"
            "false" "for" "friend" "goto" "if" "inline" "mutable"
            "namespace" "new" "noexcept" "not" "nullptr" "operator" "or"
            "private" "protected" "public" "register" "reinterpret_cast"
            "requires" "return" "sizeof" "static" "static_assert"
            "static_cast" "struct" "switch" "template" "this" "throw"
            "true" "try" "typedef" "typename" "union" "using" "virtual"
            "volatile" "while" "xor")))

(defun eglot-rcpp--rcpp-symbol-kind-for-line (line)
  "Return an LSP-like symbol kind for LINE."
  (cond
   ((string-match-p "^[[:space:]]*namespace\\_>" line) 3)
   ((string-match-p "^[[:space:]]*struct\\_>" line) 23)
   ((string-match-p "^[[:space:]]*\\(?:enum\\(?:[[:space:]]+class\\)?\\)\\_>" line) 10)
   ((string-match-p "^[[:space:]]*class\\_>" line) 5)
   (t 12)))

(defun eglot-rcpp--candidate-symbol-from-line (line)
  "Extract one likely C++ symbol name from LINE, or nil."
  (cond
   ((string-match
     "^[[:space:]]*\\(?:class\\|struct\\|namespace\\|enum\\(?:[[:space:]]+class\\)?\\)\\_>[[:space:]]+\\([[:alpha:]_][[:alnum:]_]*\\)"
     line)
    (match-string 1 line))
   ((string-match
     "^[[:space:]]*\\(?:inline\\|static\\|virtual\\|constexpr\\|friend\\|explicit\\|extern\\)?[[:space:]]*\\(?:[[:word:]_:<>,*&~]+[[:space:]]+\\)+\\([[:alpha:]_~][[:alnum:]_:~]*\\)[[:space:]]*("
     line)
    (match-string 1 line))
   (t nil)))

(defun eglot-rcpp--symbol-match-p (name query exact)
  "Return non-nil when NAME matches QUERY.

When EXACT is non-nil, require exact-ish symbol matching."
  (and name
       (not (eglot-rcpp--cpp-symbol-keyword-p name))
       (if exact
           (or (string= name query)
               (string-suffix-p (concat "::" query) name))
         (string-match-p (regexp-quote query) name))))

(defun eglot-rcpp--make-symbol-information (name kind file line column)
  "Build a SymbolInformation-like plist."
  `(:name ,name
    :kind ,kind
    :location (:uri ,(eglot-rcpp--file-uri file)
               :range (:start (:line ,line :character ,column)
                       :end (:line ,line :character ,column)))))

(defun eglot-rcpp--symbol-info-key (symbol)
  "Return a stable key for deduplicating SYMBOL."
  (let* ((location (plist-get symbol :location))
         (range (and location (plist-get location :range)))
         (start (and range (plist-get range :start)))
         (uri (plist-get location :uri))
         (path (and uri
                    (string-prefix-p "file://" uri)
                    (ignore-errors (eglot-rcpp--uri-to-path uri)))))
    (list (plist-get symbol :name)
          (or (eglot-rcpp--normalize-path path) uri)
          (plist-get start :line)
          (plist-get start :character))))

(defun eglot-rcpp--dedupe-symbol-infos (symbols)
  "Deduplicate SYMBOLS while preserving order."
  (let ((seen (make-hash-table :test #'equal)))
    (cl-loop for symbol in symbols
             for key = (eglot-rcpp--symbol-info-key symbol)
             unless (gethash key seen)
             collect symbol
             and do (puthash key t seen))))

(defun eglot-rcpp--index-package-file-symbols (file)
  "Return likely textual C/C++ symbol definitions from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let (symbols)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (unless (string-match-p "^[[:space:]]*\\(?:#\\|//\\)" line)
            (when-let ((name (eglot-rcpp--candidate-symbol-from-line line)))
              (push (eglot-rcpp--make-symbol-information
                     name
                     (eglot-rcpp--rcpp-symbol-kind-for-line line)
                     file
                     (1- (line-number-at-pos (line-beginning-position)))
                     (max 0 (or (string-match (regexp-quote name) line) 0)))
                    symbols))))
        (forward-line 1))
      (nreverse symbols))))

(defun eglot-rcpp--cache-signature (files)
  "Return a cache signature for FILES."
  (mapcar (lambda (file)
            (cons (or (eglot-rcpp--normalize-path file) file)
                  (float-time
                   (file-attribute-modification-time
                    (file-attributes file)))))
          files))

(defun eglot-rcpp--symbol-cache-key (root headers-only)
  "Return the cache key for ROOT and HEADERS-ONLY."
  (list (expand-file-name root) headers-only))

(defun eglot-rcpp--symbol-index (root &optional headers-only)
  "Return a cached textual symbol index for package ROOT."
  (let* ((files (eglot-rcpp--files root headers-only nil))
         (signature (eglot-rcpp--cache-signature files))
         (cache-key (eglot-rcpp--symbol-cache-key root headers-only))
         (cached (gethash cache-key eglot-rcpp--symbol-cache)))
    (if (and cached (equal (car cached) signature))
        (cdr cached)
      (let ((symbols (cl-loop for file in files
                              append (eglot-rcpp--index-package-file-symbols file))))
        (puthash cache-key (cons signature symbols) eglot-rcpp--symbol-cache)
        symbols))))

(defun eglot-rcpp--text-symbols (query &optional exact headers-only root)
  "Return textual fallback symbols for QUERY in package ROOT."
  (when-let ((root (or root (eglot-rcpp--project-root))))
    (cl-loop for symbol in (eglot-rcpp--symbol-index root headers-only)
             when (eglot-rcpp--symbol-match-p (plist-get symbol :name) query exact)
             collect symbol)))

(defun eglot-rcpp--symbol-info-xref (symbol)
  "Convert SYMBOL into an `xref-item'."
  (let* ((location (plist-get symbol :location))
         (range (and location (plist-get location :range)))
         (start (and range (plist-get range :start)))
         (uri (and location (plist-get location :uri))))
    (when (and uri start)
      (xref-make
       (plist-get symbol :name)
       (xref-make-file-location
        (eglot-rcpp--uri-to-path uri)
        (1+ (plist-get start :line))
        (plist-get start :character))))))

(defun eglot-rcpp--text-symbol-xrefs (query &optional exact headers-only root)
  "Return textual fallback xrefs for QUERY in package ROOT."
  (cl-loop for symbol in (eglot-rcpp--text-symbols query exact headers-only root)
           for xref = (eglot-rcpp--symbol-info-xref symbol)
           when xref collect xref))

(defun eglot-rcpp--current-eglot-definitions (identifier)
  "Return current-buffer Eglot definitions for IDENTIFIER."
  (when (bound-and-true-p eglot-managed-mode)
    (ignore-errors (xref-backend-definitions 'eglot identifier))))

(defun eglot-rcpp--current-eglot-references (identifier)
  "Return current-buffer Eglot references for IDENTIFIER."
  (when (bound-and-true-p eglot-managed-mode)
    (ignore-errors (xref-backend-references 'eglot identifier))))

(defun eglot-rcpp--workspace-symbol-apropos (pattern &optional root)
  "Return project-wide workspace-symbol xrefs for PATTERN."
  (cl-loop for buffer in (eglot-rcpp--project-server-buffers root)
           append (cl-loop for symbol in (eglot-rcpp--server-workspace-symbols buffer pattern)
                           for name = (plist-get symbol :name)
                           for xref = (and (eglot-rcpp--symbol-match-p name pattern nil)
                                           (eglot-rcpp--symbol-info-xref symbol))
                           when (and xref
                                     (eglot-rcpp--xref-in-root-p xref root))
                           collect xref)))

(defun eglot-rcpp--bridge-cache-key (root)
  "Return the bridge cache key for ROOT."
  (expand-file-name root))

(defun eglot-rcpp--bridge-export-candidates (root)
  "Return exported Rcpp function names from ROOT's `RcppExports.R'."
  (let* ((file (eglot-rcpp--r-bridge-file root))
         (cache-key (eglot-rcpp--bridge-cache-key root))
         (signature (and (file-exists-p file)
                         (list (float-time
                                (file-attribute-modification-time
                                 (file-attributes file))))))
         (cached (gethash cache-key eglot-rcpp--bridge-cache)))
    (cond
     ((not signature)
      (remhash cache-key eglot-rcpp--bridge-cache)
      nil)
     ((and cached (equal (car cached) signature))
      (cdr cached))
     (t
      (with-temp-buffer
        (insert-file-contents file)
        (let (candidates)
          (goto-char (point-min))
          (while (re-search-forward
                  "^[[:space:]]*\\([.[:alpha:]_][.[:alnum:]_]*\\)[[:space:]]*<-[[:space:]]*function\\s-*("
                  nil t)
            (push (match-string-no-properties 1) candidates))
          (setq candidates (nreverse (delete-dups candidates)))
          (puthash cache-key (cons signature candidates) eglot-rcpp--bridge-cache)
          candidates))))))

(defun eglot-rcpp--run-next-capf ()
  "Return the next completion-at-point result after `eglot-rcpp'."
  (let ((completion-at-point-functions
         (remq #'eglot-rcpp-completion-at-point completion-at-point-functions)))
    (cl-loop for function in completion-at-point-functions
             thereis (and (functionp function)
                          (funcall function)))))

(defun eglot-rcpp--completion-metadata (base-table exports)
  "Return completion metadata for BASE-TABLE supplemented by EXPORTS."
  (let* ((base-metadata (and base-table (completion-metadata "" base-table nil)))
         (base-annotation (completion-metadata-get base-metadata 'annotation-function))
         (base-category (completion-metadata-get base-metadata 'category)))
    `(metadata
      (category . ,(or base-category 'symbol))
      (annotation-function
       . ,(lambda (candidate)
            (or (and (member candidate exports) " [Rcpp]")
                (and base-annotation
                     (funcall base-annotation candidate))))))))

(defun eglot-rcpp--merge-completion-table (base-table exports)
  "Return a completion table combining BASE-TABLE with EXPORTS."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        (eglot-rcpp--completion-metadata base-table exports)
      (let ((candidates
             (delete-dups
              (append (if base-table
                          (all-completions string base-table predicate)
                        nil)
                      (cl-remove-if-not
                       (lambda (candidate)
                         (string-prefix-p string candidate))
                       exports)))))
        (complete-with-action action candidates string predicate)))))

;;;###autoload
(defun eglot-rcpp-completion-at-point ()
  "Supplement R completion with names exported through `RcppExports.R'."
  (when-let* ((root (and (eq (eglot-rcpp--buffer-kind) 'r)
                         (eglot-rcpp--project-root)))
              (exports (eglot-rcpp--bridge-export-candidates root)))
    (let* ((base (eglot-rcpp--run-next-capf))
           (base-beg (nth 0 base))
           (base-end (nth 1 base))
           (base-table (nth 2 base))
           (bounds (bounds-of-thing-at-point 'symbol))
           (beg (or base-beg (car bounds)))
           (end (or base-end (cdr bounds))))
      (cond
       ((and base
             (or (null beg)
                 (null end)
                 (not (and (= beg base-beg) (= end base-end)))))
        base)
       ((and (null beg) (null base))
        nil)
       (t
        (append
         (list beg end (eglot-rcpp--merge-completion-table base-table exports))
         (nthcdr 3 base)))))))

(defun eglot-rcpp--cache-keys-for-root (root)
  "Return all cache keys that belong to ROOT."
  (list (eglot-rcpp--bridge-cache-key root)
        (eglot-rcpp--symbol-cache-key root nil)
        (eglot-rcpp--symbol-cache-key root t)))

;;;###autoload
(defun eglot-rcpp-invalidate-project-cache (&optional root)
  "Clear cached symbol and bridge data for ROOT or the current project."
  (interactive)
  (when-let ((root (or root (eglot-rcpp--project-root))))
    (remhash (eglot-rcpp--bridge-cache-key root) eglot-rcpp--bridge-cache)
    (remhash (eglot-rcpp--symbol-cache-key root nil) eglot-rcpp--symbol-cache)
    (remhash (eglot-rcpp--symbol-cache-key root t) eglot-rcpp--symbol-cache)))

(defun eglot-rcpp--after-save ()
  "Invalidate the current project's caches after a save."
  (eglot-rcpp-invalidate-project-cache))

(defun eglot-rcpp-install-project-xref-backend ()
  "Install the mixed-project xref backend in the current buffer."
  (when (bound-and-true-p eglot-rcpp-mode)
    (setq-local xref-backend-functions
                (cons #'eglot-rcpp-project-xref-backend
                      (remq #'eglot-rcpp-project-xref-backend
                            xref-backend-functions)))))

(defun eglot-rcpp-project-xref-backend ()
  "Return the mixed-project xref backend for `eglot-rcpp'."
  'eglot-rcpp-project)

(cl-defmethod xref-backend-definitions ((_backend (eql eglot-rcpp-project)) identifier)
  "Find IDENTIFIER definitions across the current mixed-language package."
  (let ((root (eglot-rcpp--project-root)))
    (eglot-rcpp--apply-generated-definition-policy
     (eglot-rcpp--dedupe-xrefs
      (cl-remove-if-not
       (lambda (xref) (eglot-rcpp--xref-in-root-p xref root))
       (append (eglot-rcpp--current-eglot-definitions identifier)
               (eglot-rcpp--workspace-symbol-definitions identifier root)
               ;; Fallback definitions should search real package sources too,
               ;; otherwise R buffers can miss Rcpp exports that only exist in
               ;; src/*.cpp until clangd has already been started separately.
               (eglot-rcpp--text-symbol-xrefs identifier t nil root)))))))

(cl-defmethod xref-backend-references ((_backend (eql eglot-rcpp-project)) identifier)
  "Find IDENTIFIER references using the current server, filtered to the package."
  (let ((root (eglot-rcpp--project-root)))
    (eglot-rcpp--dedupe-xrefs
     (cl-remove-if-not
      (lambda (xref) (eglot-rcpp--xref-in-root-p xref root))
      (append (eglot-rcpp--current-eglot-references identifier) nil)))))

(cl-defmethod xref-backend-apropos ((_backend (eql eglot-rcpp-project)) pattern)
  "Find package symbols matching PATTERN."
  (let ((root (eglot-rcpp--project-root)))
    (eglot-rcpp--dedupe-xrefs
     (cl-remove-if-not
      (lambda (xref) (eglot-rcpp--xref-in-root-p xref root))
      (append (eglot-rcpp--workspace-symbol-apropos pattern root)
              (eglot-rcpp--text-symbol-xrefs pattern nil t root)
              (eglot-rcpp--text-symbol-xrefs pattern nil nil root))))))

(defvar eglot-rcpp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e c") #'eglot-rcpp-compile-attributes)
    (define-key map (kbd "C-c C-e u") #'eglot-rcpp-use-rcpp)
    (define-key map (kbd "C-c C-e d") #'eglot-rcpp-check-r-dependencies)
    map)
  "Keymap for `eglot-rcpp-mode'.")

;;;###autoload
(define-minor-mode eglot-rcpp-mode
  "Buffer-local mixed-language support for R package development."
  :lighter ""
  :keymap eglot-rcpp-mode-map
  (if eglot-rcpp-mode
      (progn
        (eglot-rcpp-install-project-xref-backend)
        (add-hook 'after-save-hook #'eglot-rcpp--after-save nil t)
        (when (eq (eglot-rcpp--buffer-kind) 'r)
          (add-hook 'completion-at-point-functions
                    #'eglot-rcpp-completion-at-point nil t)))
    (eglot-rcpp--cancel-project-setup)
    (remove-hook 'after-save-hook #'eglot-rcpp--after-save t)
    (remove-hook 'completion-at-point-functions
                 #'eglot-rcpp-completion-at-point t)
    (setq-local xref-backend-functions
                (remq #'eglot-rcpp-project-xref-backend
                      xref-backend-functions))))

(defun eglot-rcpp--activate-current-buffer ()
  "Enable `eglot-rcpp-mode' in the current buffer when appropriate."
  (if (eglot-rcpp--eligible-buffer-p)
      (progn
        (unless (bound-and-true-p eglot-rcpp-mode)
          (eglot-rcpp-mode 1))
        (eglot-ensure)
        (eglot-rcpp-schedule-project-setup))
    (when (bound-and-true-p eglot-rcpp-mode)
      (eglot-rcpp-mode -1))))

(defun eglot-rcpp--install-project-root-markers ()
  "Teach project navigators about `DESCRIPTION'."
  (when eglot-rcpp-enable-project-root-markers
    (with-eval-after-load 'project
      (add-to-list 'project-vc-extra-root-markers eglot-rcpp-root-marker-file))
    (with-eval-after-load 'projectile
      (add-to-list 'projectile-project-root-files eglot-rcpp-root-marker-file))))

(defun eglot-rcpp--ensure-server-program (modes command)
  "Register COMMAND for MODES in `eglot-server-programs'."
  (let ((entry (cons modes command)))
    (unless (member entry eglot-server-programs)
      (add-to-list 'eglot-server-programs entry))))

(defun eglot-rcpp--install-eglot-server-programs ()
  "Install Eglot server entries for package R and C/C++ buffers."
  (with-eval-after-load 'eglot
    (when (eglot-rcpp--command-available-p eglot-rcpp-r-server-command)
      (eglot-rcpp--ensure-server-program
       eglot-rcpp-r-modes
       eglot-rcpp-r-server-command))
    (when (eglot-rcpp--command-available-p eglot-rcpp-clangd-command)
      (eglot-rcpp--ensure-server-program
       eglot-rcpp-cpp-modes
       eglot-rcpp-clangd-command))))

(defun eglot-rcpp--install-ess-support ()
  "Install optional ESS bindings when enabled."
  (when eglot-rcpp-enable-ess-keybindings
    (dolist (feature '(ess-r-mode ess-site))
      (with-eval-after-load feature
        (when (boundp 'ess-r-mode-map)
          (define-key ess-r-mode-map (kbd "C-c C-e") eglot-rcpp-ess-command-map))))))

(defun eglot-rcpp--eglot-managed-hook ()
  "Finalize `eglot-rcpp' setup when Eglot starts managing this buffer."
  (when (bound-and-true-p eglot-rcpp-mode)
    (eglot-rcpp-install-project-xref-backend)
    (when-let ((root (eglot-rcpp--project-root))
               (kind (eglot-rcpp--buffer-kind)))
      (eglot-rcpp--set-pending-companion-start root kind nil))
    (eglot-rcpp-maybe-start-companion-server)))

(defun eglot-rcpp--install-hooks ()
  "Install global `eglot-rcpp' hooks once."
  (unless eglot-rcpp--hooks-installed
    (setq eglot-rcpp--hooks-installed t)
    (dolist (mode (eglot-rcpp--all-managed-modes))
      (add-hook (eglot-rcpp--mode-hook mode) #'eglot-rcpp--activate-current-buffer))
    (add-hook 'eglot-managed-mode-hook #'eglot-rcpp--eglot-managed-hook)))

(defun eglot-rcpp--project-root-or-error ()
  "Return the current package root or signal a user error."
  (or (eglot-rcpp--project-root)
      (user-error "eglot-rcpp: current buffer is not in an R package project")))

(defun eglot-rcpp--r-command-line (expression)
  "Return a shell command line that evaluates EXPRESSION in R."
  (mapconcat #'shell-quote-argument
             (list (or (eglot-rcpp--r-executable)
                       (car-safe eglot-rcpp-r-server-command)
                       "R")
                   "--slave" "-e" expression)
             " "))

(defun eglot-rcpp--start-r-command (root expression buffer-name)
  "Run R EXPRESSION under ROOT in a compilation buffer named BUFFER-NAME."
  (let ((default-directory root))
    (compilation-start
     (eglot-rcpp--r-command-line expression)
     'compilation-mode
     (lambda (_mode) buffer-name))))

(defun eglot-rcpp--r-package-installed-p (package)
  "Return non-nil when R PACKAGE is installed."
  (when-let ((r-executable (eglot-rcpp--r-executable)))
    (with-temp-buffer
      (and (zerop (process-file
                   r-executable nil t nil
                   "--slave" "-e"
                   (format "cat(if (requireNamespace(%S, quietly = TRUE)) 'yes' else 'no')"
                           package)))
           (string= (string-trim (buffer-string)) "yes")))))

(defun eglot-rcpp--missing-r-packages (packages &optional predicate)
  "Return missing PACKAGES according to PREDICATE.

PREDICATE defaults to `eglot-rcpp--r-package-installed-p'."
  (let ((predicate (or predicate #'eglot-rcpp--r-package-installed-p)))
    (cl-remove-if predicate packages)))

(defun eglot-rcpp--ensure-r-package (package purpose)
  "Ensure R PACKAGE is installed for PURPOSE.

Return non-nil when the package is available after this check."
  (cond
   ((eglot-rcpp--r-package-installed-p package) t)
   ((not (eglot-rcpp--r-executable))
    (eglot-rcpp--warn-missing-r)
    nil)
   ((y-or-n-p (format "Install missing R package `%s' for %s? " package purpose))
    (eglot-rcpp--start-r-command
     (eglot-rcpp--project-root-or-error)
     (format "install.packages(%S)" package)
     (format "*eglot-rcpp install %s*" package))
    (message "eglot-rcpp: rerun the command after `%s' finishes installing" package)
    nil)
   (t nil)))

(defun eglot-rcpp--description-mentions-rcpp-p (root)
  "Return non-nil when ROOT's DESCRIPTION already mentions Rcpp."
  (let ((description (expand-file-name eglot-rcpp-root-marker-file root)))
    (and (file-readable-p description)
         (with-temp-buffer
           (insert-file-contents description)
           (re-search-forward "^\\(?:LinkingTo\\|Imports\\|Depends\\):.*\\bRcpp\\b" nil t)))))

(defun eglot-rcpp--rcpp-enabled-p (root)
  "Return non-nil when ROOT already looks like an Rcpp package."
  (or (file-exists-p (eglot-rcpp--r-bridge-file root))
      (file-exists-p (eglot-rcpp--cpp-bridge-file root))
      (eglot-rcpp--cpp-files-present-p root)
      (eglot-rcpp--description-mentions-rcpp-p root)))

;;;###autoload
(defun eglot-rcpp-check-r-dependencies (&optional prompt-install)
  "Check the package's R-side dependencies.

With prefix argument PROMPT-INSTALL, offer to install missing packages."
  (interactive "P")
  (let* ((packages '("languageserver" "Rcpp" "usethis"))
         (missing (eglot-rcpp--missing-r-packages packages)))
    (cond
     ((not (eglot-rcpp--r-executable))
      (eglot-rcpp--warn-missing-r))
     ((null missing)
      (message "eglot-rcpp: R dependencies available: %s"
               (string-join packages ", ")))
     (prompt-install
      (dolist (package missing)
        (eglot-rcpp--ensure-r-package package "eglot-rcpp")))
     (t
      (message "eglot-rcpp: missing R packages: %s"
               (string-join missing ", "))))))

;;;###autoload
(defun eglot-rcpp-use-rcpp ()
  "Enable Rcpp infrastructure in the current R package with `usethis::use_rcpp()'."
  (interactive)
  (let ((root (eglot-rcpp--project-root-or-error)))
    (when (and (eglot-rcpp--rcpp-enabled-p root)
               (not (y-or-n-p "Project already looks Rcpp-enabled. Run `usethis::use_rcpp()` anyway? ")))
      (user-error "eglot-rcpp: aborted"))
    (when (eglot-rcpp--ensure-r-package "usethis" "Rcpp package setup")
      (eglot-rcpp--start-r-command root "usethis::use_rcpp()" "*eglot-rcpp use_rcpp*")
      (message "eglot-rcpp: started `usethis::use_rcpp()` for %s"
               (abbreviate-file-name root)))))

;;;###autoload
(defun eglot-rcpp-compile-attributes ()
  "Regenerate Rcpp attribute bridge files with `Rcpp::compileAttributes()'."
  (interactive)
  (let ((root (eglot-rcpp--project-root-or-error)))
    (when (eglot-rcpp--ensure-r-package "Rcpp" "Rcpp attribute generation")
      (eglot-rcpp-invalidate-project-cache root)
      (eglot-rcpp--start-r-command
       root
       "Rcpp::compileAttributes()"
       "*eglot-rcpp compileAttributes*")
      (message "eglot-rcpp: regenerating `RcppExports` bridge files for %s"
               (abbreviate-file-name root)))))

;;;###autoload
(defun eglot-rcpp-find-symbol (pattern)
  "Find package symbols matching PATTERN using the mixed-project xref backend."
  (interactive (list (read-string "eglot-rcpp symbol: "
                                  (thing-at-point 'symbol t))))
  (let ((xref-backend-functions '(eglot-rcpp-project-xref-backend)))
    (xref-find-apropos pattern)))

;;;###autoload
(defun eglot-rcpp-setup ()
  "Install `eglot-rcpp' support for mixed R / Rcpp / C++ package projects."
  (interactive)
  (eglot-rcpp--install-project-root-markers)
  (eglot-rcpp--install-eglot-server-programs)
  (eglot-rcpp--install-ess-support)
  (eglot-rcpp--install-hooks))

(eglot-rcpp-setup)

(provide 'eglot-rcpp)

;;; eglot-rcpp.el ends here
