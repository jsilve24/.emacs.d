;; eglot-rcpp.el --- Mixed R/C++ Eglot support -*- lexical-binding: t; -*-

;; Author: Justin Silverman <jsilve24@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, languages, convenience

;;; Commentary:

;; `eglot-rcpp' keeps R package development workable when the package mixes
;; R code and C/C++ code under `src/' or `inst/include/'.  It can:
;; - start the companion R or clangd server when the current buffer only has
;;   one half of the pair attached,
;; - install a project-aware xref backend that merges Eglot results with
;;   package-wide symbol fallbacks, and
;; - augment `consult-eglot-symbols' when `consult-eglot' is available.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'xref)

(defgroup eglot-rcpp nil
  "Mixed R / C++ Eglot support for package development."
  :group 'eglot)

(defcustom eglot-rcpp-root-marker-file "DESCRIPTION"
  "Project marker used to recognize mixed R/C++ package roots."
  :type 'string
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-r-modes '(ess-r-mode)
  "Major modes that should use the R language server."
  :type '(repeat symbol)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-cpp-modes '(c-mode c++-mode c-ts-mode c++-ts-mode)
  "Major modes that should use clangd for mixed R/C++ projects."
  :type '(repeat symbol)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-source-directories '("src")
  "Directories searched for mixed project C/C++ source files."
  :type '(repeat string)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-header-directories '("include" "inst/include")
  "Directories searched for mixed project header files."
  :type '(repeat string)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-source-extensions '("c" "cc" "cpp" "cxx" "m" "mm")
  "Source file extensions searched when indexing mixed project symbols."
  :type '(repeat string)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-header-extensions '("h" "hh" "hpp" "hxx" "ipp" "tpp")
  "Header file extensions searched when indexing mixed project symbols."
  :type '(repeat string)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-auto-start-companion-servers t
  "When non-nil, start the missing R or C++ Eglot server automatically.

When visiting an R buffer inside a package root that also contains C/C++ code,
the package will try to start clangd in a companion buffer, and vice versa for
opening a C/C++ buffer first."
  :type 'boolean
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-project-setup-retries 10
  "How many times to retry project Eglot setup after opening a buffer."
  :type 'integer
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-r-server-command '("R" "--slave" "-e" "languageserver::run()")
  "Command used to start the R language server."
  :type '(repeat string)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-clangd-command '("clangd" "--header-insertion=never")
  "Command used to start clangd for mixed project C/C++ buffers."
  :type '(repeat string)
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-enable-project-root-markers t
  "When non-nil, teach `project.el' and `projectile' about `DESCRIPTION'."
  :type 'boolean
  :group 'eglot-rcpp)

(defcustom eglot-rcpp-restrict-symbol-search-to-project t
  "When non-nil, keep symbol search results inside the current package root."
  :type 'boolean
  :group 'eglot-rcpp)

(defvar eglot-managed-mode)
(defvar eglot--cached-server)
(defvar eglot--servers-by-project)
(defvar eglot-server-programs)
(defvar projectile-project-root-files)
(defvar project-vc-extra-root-markers)
(defvar consult-eglot-sort-results)

(defvar eglot-rcpp--missing-clangd-warned nil
  "Non-nil after warning once that clangd is unavailable.")

(defvar eglot-rcpp--missing-r-warned nil
  "Non-nil after warning once that the R server command is unavailable.")

(defvar eglot-rcpp--pending-companion-starts (make-hash-table :test #'equal)
  "In-flight companion server starts keyed by package root and server kind.")

(defvar-local eglot-rcpp-companion-buffer nil)
(defvar-local eglot-rcpp-project-setup-timer nil)
(defvar-local eglot-rcpp-project-setup-attempts 0)

(declare-function eglot-current-server "eglot")
(declare-function eglot "eglot")
(declare-function eglot-ensure "eglot")
(declare-function eglot-server-capable "eglot")
(declare-function eglot--guess-contact "eglot")
(declare-function eglot--project "eglot")
(declare-function eglot--major-modes "eglot")
(declare-function eglot--request "eglot")
(declare-function eglot-uri-to-path "eglot")
(declare-function project-current "project")
(declare-function jsonrpc-async-request "jsonrpc")

(defun eglot-rcpp--normalize-path (path)
  "Return a canonical absolute path for PATH, or nil."
  (when path
    (ignore-errors
      (file-truename (expand-file-name path)))))

(defun eglot-rcpp--file-uri (file)
  "Return a canonical file URI for FILE."
  (concat "file://" (or (eglot-rcpp--normalize-path file)
                        (expand-file-name file))))

(defun eglot-rcpp--mode-hook (mode)
  "Return the hook symbol for major mode MODE."
  (intern (format "%s-hook" mode)))

(defun eglot-rcpp--all-managed-modes ()
  "Return all major modes managed by this package."
  (append eglot-rcpp-r-modes eglot-rcpp-cpp-modes))

(defun eglot-rcpp--description-root (&optional dir)
  "Return the nearest parent directory of DIR containing `DESCRIPTION'."
  (when-let ((root (locate-dominating-file (or dir default-directory)
                                           eglot-rcpp-root-marker-file)))
    (or (eglot-rcpp--normalize-path root)
        (expand-file-name root))))

(defun eglot-rcpp--server-root (server)
  "Return the project root for SERVER, or nil."
  (when-let* ((project (ignore-errors (eglot--project server))))
    (ignore-errors (project-root project))))

(defun eglot-rcpp--server-in-current-root-p (server)
  "Return non-nil when SERVER belongs to the current package root."
  (let ((root (eglot-rcpp--description-root)))
    (if (not (and eglot-rcpp-restrict-symbol-search-to-project root))
        t
      (eglot-rcpp--within-root-p (eglot-rcpp--server-root server) root))))

(defun eglot-rcpp--server-relevant-to-current-language-p (server)
  "Return non-nil when SERVER matches the current buffer's language group."
  (cond
   ((apply #'derived-mode-p eglot-rcpp-r-modes)
    (cl-intersection eglot-rcpp-r-modes (eglot--major-modes server)))
   ((apply #'derived-mode-p eglot-rcpp-cpp-modes)
    (cl-intersection eglot-rcpp-cpp-modes (eglot--major-modes server)))
   (t t)))

(defun eglot-rcpp--workspace-symbol-servers ()
  "Return project servers suitable for symbol search in the current buffer."
  (cl-remove-if-not
   (lambda (server)
     (and (eglot-rcpp--server-in-current-root-p server)
          (eglot-rcpp--server-relevant-to-current-language-p server)))
   (eglot-rcpp--project-servers :workspaceSymbolProvider)))

(defun eglot-rcpp--within-root-p (path root)
  "Return non-nil when PATH is inside ROOT."
  (when (and path root)
    (let* ((path (file-truename (expand-file-name path)))
           (root (file-name-as-directory (file-truename (expand-file-name root)))))
      (string-prefix-p root path))))

(defun eglot-rcpp--symbol-location-path (symbol)
  "Return the file path for SYMBOL, or nil."
  (when-let* ((location (plist-get symbol :location))
              (uri (plist-get location :uri)))
    (ignore-errors (eglot-uri-to-path uri))))

(defun eglot-rcpp--symbol-info-in-root-p (symbol root)
  "Return non-nil when SYMBOL lives inside ROOT."
  (if (not eglot-rcpp-restrict-symbol-search-to-project)
      t
    (eglot-rcpp--within-root-p (eglot-rcpp--symbol-location-path symbol) root)))

(defun eglot-rcpp--xref-in-root-p (xref root)
  "Return non-nil when XREF points inside ROOT."
  (if (not eglot-rcpp-restrict-symbol-search-to-project)
      t
    (let ((location (xref-item-location xref)))
      (when (xref-file-location-p location)
        (eglot-rcpp--within-root-p (xref-file-location-file location) root)))))

(defun eglot-rcpp--first-file-matching (directory regexp)
  "Return the first file under DIRECTORY matching REGEXP."
  (when (file-directory-p directory)
    (car (directory-files-recursively directory regexp nil nil t))))

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

(defun eglot-rcpp--files (root &optional headers-only)
  "Return relevant source or header files under ROOT."
  (let ((regexp (format "\\.\\(%s\\)$"
                        (string-join
                         (if headers-only
                             eglot-rcpp-header-extensions
                           (append eglot-rcpp-source-extensions
                                   eglot-rcpp-header-extensions))
                         "\\|"))))
    (cl-delete-duplicates
     (cl-loop for directory in (eglot-rcpp--package-directories root headers-only)
              append (directory-files-recursively directory regexp nil nil t))
     :test (lambda (left right)
             (equal (or (eglot-rcpp--normalize-path left) left)
                    (or (eglot-rcpp--normalize-path right) right))))))

(defun eglot-rcpp--backend-file (root)
  "Return one representative C/C++ file for the package at ROOT."
  (or (eglot-rcpp--first-file-matching
       (expand-file-name "src" root)
       "\\.\\(cc\\|cpp\\|cxx\\|c\\)$")
      (eglot-rcpp--first-file-matching
       (expand-file-name "inst/include" root)
       "\\.\\(h\\|hh\\|hpp\\|hxx\\|ipp\\|tpp\\)$")
      (eglot-rcpp--first-file-matching
       (expand-file-name "include" root)
       "\\.\\(h\\|hh\\|hpp\\|hxx\\|ipp\\|tpp\\)$")
      (eglot-rcpp--first-file-matching
       (expand-file-name "src" root)
       "\\.\\(h\\|hh\\|hpp\\|hxx\\|ipp\\|tpp\\)$")))

(defun eglot-rcpp--r-package-file (root)
  "Return one representative R source file for the package at ROOT."
  (eglot-rcpp--first-file-matching
   (expand-file-name "R" root)
   "\\.[rR]$"))

(defun eglot-rcpp--companion-key (root kind)
  "Return a stable key for the companion server KIND at package ROOT."
  (list (expand-file-name root) kind))

(defun eglot-rcpp--pending-companion-start-p (root kind)
  "Return non-nil when companion server KIND for ROOT is already starting."
  (gethash (eglot-rcpp--companion-key root kind)
           eglot-rcpp--pending-companion-starts))

(defun eglot-rcpp--set-pending-companion-start (root kind pending)
  "Set companion server KIND pending state for ROOT to PENDING."
  (let ((key (eglot-rcpp--companion-key root kind)))
    (if pending
        (puthash key t eglot-rcpp--pending-companion-starts)
      (remhash key eglot-rcpp--pending-companion-starts))))

(defun eglot-rcpp--current-server ()
  "Return the current buffer's Eglot server, or nil when unmanaged."
  (ignore-errors (eglot-current-server)))

(defun eglot-rcpp--warn-missing-clangd ()
  "Warn once that clangd is required for C/C++ Eglot support."
  (unless eglot-rcpp--missing-clangd-warned
    (setq eglot-rcpp--missing-clangd-warned t)
    (message "Rcpp companion server skipped: `clangd' is not installed")))

(defun eglot-rcpp--warn-missing-r ()
  "Warn once that the R language server command is unavailable."
  (unless eglot-rcpp--missing-r-warned
    (setq eglot-rcpp--missing-r-warned t)
    (message "Rcpp companion server skipped: `R' is not installed")))

(defun eglot-rcpp--r-server-available-p ()
  "Return non-nil when the configured R server command looks runnable."
  (and eglot-rcpp-r-server-command
       (listp eglot-rcpp-r-server-command)
       (executable-find (car eglot-rcpp-r-server-command))))

(defun eglot-rcpp--start-server-for-file (file root kind)
  "Start companion server KIND for FILE in package ROOT."
  (eglot-rcpp--set-pending-companion-start root kind t)
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (setq-local eglot-rcpp-companion-buffer t)
      (unless (eglot-rcpp--current-server)
        (condition-case err
            ;; `eglot-ensure' is not enough here: once an R server already
            ;; exists for the project, it may decide the buffer is already
            ;; "covered" and skip starting the C/C++ companion server. Force
            ;; the contact for the target file instead.
            (apply #'eglot (eglot--guess-contact))
          (error
           (eglot-rcpp--set-pending-companion-start root kind nil)
           (message "Eglot companion start failed for %s: %s"
                    (abbreviate-file-name file)
                    (error-message-string err))))))
    (bury-buffer buffer)))

(defun eglot-rcpp--server-running-for-modes-p (modes)
  "Return non-nil when a current-project Eglot server manages MODES."
  (cl-some
   (lambda (server)
     (cl-intersection modes (eglot--major-modes server)))
   (eglot-rcpp--project-servers)))

(defun eglot-rcpp-maybe-start-companion-server ()
  "Start the missing R or C++ Eglot server for the current mixed package."
  (when (and eglot-rcpp-auto-start-companion-servers
             (eglot-rcpp--current-server)
             (not eglot-rcpp-companion-buffer)
             (apply #'derived-mode-p (eglot-rcpp--all-managed-modes)))
    (when-let* ((root (eglot-rcpp--description-root))
                (cpp-file (eglot-rcpp--backend-file root)))
      (cond
       ((apply #'derived-mode-p eglot-rcpp-r-modes)
        (cond
         ((not (and eglot-rcpp-clangd-command
                    (executable-find (car eglot-rcpp-clangd-command))))
          (eglot-rcpp--warn-missing-clangd))
         ((eglot-rcpp--server-running-for-modes-p eglot-rcpp-cpp-modes)
          (eglot-rcpp--set-pending-companion-start root 'cpp nil))
         ((not (eglot-rcpp--pending-companion-start-p root 'cpp))
          (eglot-rcpp--start-server-for-file cpp-file root 'cpp))))
       ((apply #'derived-mode-p eglot-rcpp-cpp-modes)
        (cond
         ((not (eglot-rcpp--r-server-available-p))
          (eglot-rcpp--warn-missing-r))
         ((eglot-rcpp--server-running-for-modes-p eglot-rcpp-r-modes)
          (eglot-rcpp--set-pending-companion-start root 'r nil))
         ((not (eglot-rcpp--pending-companion-start-p root 'r))
          (when-let ((r-file (eglot-rcpp--r-package-file root)))
            (eglot-rcpp--start-server-for-file r-file root 'r)))))))))

(defun eglot-rcpp-install-project-xref-backend ()
  "Install a project-aware Eglot xref backend in R and C/C++ buffers."
  (when (and (eglot-rcpp--current-server)
             (apply #'derived-mode-p (eglot-rcpp--all-managed-modes)))
    ;; Put the mixed-project backend first so `xref-find-definitions' reaches
    ;; it before backend-specific fallbacks such as dumb-jump.
    (setq-local xref-backend-functions
                (cons #'eglot-rcpp-project-xref-backend
                      (remq #'eglot-rcpp-project-xref-backend
                            xref-backend-functions)))))

(defun eglot-rcpp--relevant-mode-p ()
  "Return non-nil when the current buffer should participate in mixed Eglot."
  (apply #'derived-mode-p (eglot-rcpp--all-managed-modes)))

(defun eglot-rcpp--finalize-project-setup (buffer)
  "Finish mixed-project Eglot setup for BUFFER once a server is available."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((not (eglot-rcpp--relevant-mode-p))
        (setq-local eglot-rcpp-project-setup-timer nil
                    eglot-rcpp-project-setup-attempts 0))
       ((eglot-rcpp--current-server)
        (setq-local eglot-rcpp-project-setup-timer nil
                    eglot-rcpp-project-setup-attempts 0)
        (eglot-rcpp-install-project-xref-backend)
        (eglot-rcpp-maybe-start-companion-server))
       ((> eglot-rcpp-project-setup-attempts 0)
        (cl-decf eglot-rcpp-project-setup-attempts)
        (setq-local eglot-rcpp-project-setup-timer
                    (run-at-time
                     0.5 nil #'eglot-rcpp--finalize-project-setup buffer)))
       (t
        (setq-local eglot-rcpp-project-setup-timer nil))))))

(defun eglot-rcpp-schedule-project-setup ()
  "Retry mixed-project Eglot setup until the current buffer has a server."
  (when (eglot-rcpp--relevant-mode-p)
    (when (timerp eglot-rcpp-project-setup-timer)
      (cancel-timer eglot-rcpp-project-setup-timer))
    (setq-local eglot-rcpp-project-setup-attempts eglot-rcpp-project-setup-retries)
    (setq-local eglot-rcpp-project-setup-timer
                (run-at-time
                 0.5 nil #'eglot-rcpp--finalize-project-setup (current-buffer)))))

(defun eglot-rcpp-project-xref-backend ()
  "Return the multi-server Eglot xref backend for mixed R/C++ projects."
  'eglot-rcpp-project)

(defun eglot-rcpp--project-servers (&optional capability)
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

(defun eglot-rcpp--workspace-symbol-name-match-p (candidate identifier)
  "Return non-nil when CANDIDATE is an exact-ish match for IDENTIFIER."
  (or (string= candidate identifier)
      (string-suffix-p (concat "::" identifier) candidate)))

(defun eglot-rcpp--workspace-symbol-xref (symbol identifier)
  "Convert SYMBOL from `workspace/symbol' into an xref for IDENTIFIER."
  (let* ((name (plist-get symbol :name))
         (location (plist-get symbol :location))
         (range (and location (plist-get location :range)))
         (start (and range (plist-get range :start)))
         (uri (and location (plist-get location :uri))))
    (when (and name
               uri
               start
               (eglot-rcpp--workspace-symbol-name-match-p name identifier))
      (xref-make
       name
       (xref-make-file-location
        (eglot-uri-to-path uri)
        (1+ (plist-get start :line))
        (plist-get start :character))))))

(defun eglot-rcpp--workspace-symbol-definitions (identifier)
  "Return project-wide workspace-symbol xrefs for IDENTIFIER."
  (cl-loop
   for server in (eglot-rcpp--project-servers :workspaceSymbolProvider)
   append
   (mapcan
    (lambda (symbol)
      (when-let ((xref (eglot-rcpp--workspace-symbol-xref symbol identifier)))
        (list xref)))
    (append (ignore-errors
              (eglot--request server :workspace/symbol `(:query ,identifier)))
            nil))))

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
                    (ignore-errors
                      (eglot-uri-to-path uri)))))
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
             do (puthash key t seen)
             collect symbol)))

(defun eglot-rcpp--index-package-file-symbols (file)
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

(defun eglot-rcpp--symbol-cache-key (root headers-only)
  "Return the cache key for ROOT and HEADERS-ONLY."
  (list (expand-file-name root) headers-only))

(defvar eglot-rcpp--symbol-cache (make-hash-table :test #'equal)
  "Cache of textual symbol indexes for mixed project trees.")

(defun eglot-rcpp--symbol-index (root &optional headers-only)
  "Return a cached textual symbol index for the package ROOT."
  (let* ((files (eglot-rcpp--files root headers-only))
         (stamp (if files
                    (cl-loop for file in files
                             maximize (float-time
                                       (file-attribute-modification-time
                                        (file-attributes file)))
                             into newest
                             finally return newest)
                  0))
         (cache-key (eglot-rcpp--symbol-cache-key root headers-only))
         (cached (gethash cache-key eglot-rcpp--symbol-cache)))
    (if (and cached (= (car cached) stamp))
        (cdr cached)
      (let ((symbols (cl-loop for file in files
                              append (eglot-rcpp--index-package-file-symbols file))))
        (puthash cache-key (cons stamp symbols) eglot-rcpp--symbol-cache)
        symbols))))

(defun eglot-rcpp--text-symbols (query &optional exact headers-only)
  "Return textual fallback symbols for QUERY in the current mixed package."
  (when-let ((root (eglot-rcpp--description-root)))
    (cl-loop for symbol in (eglot-rcpp--symbol-index root headers-only)
             when (eglot-rcpp--symbol-match-p (plist-get symbol :name) query exact)
             collect symbol)))

(defun eglot-rcpp--text-symbol-xrefs (identifier)
  "Return textual fallback xrefs for IDENTIFIER in package headers."
  (mapcar (lambda (symbol)
            (eglot-rcpp--workspace-symbol-xref symbol identifier))
          (eglot-rcpp--text-symbols identifier t t)))

(defun eglot-rcpp--xref-key (xref)
  "Return a stable key for deduplicating XREF."
  (let ((location (xref-item-location xref)))
    (if (xref-file-location-p location)
        (list (xref-file-location-file location)
              (xref-file-location-line location)
              (xref-file-location-column location))
      (list (xref-item-summary xref)))))

(defun eglot-rcpp--dedupe-xrefs (xrefs)
  "Deduplicate XREFS while preserving order."
  (let ((seen (make-hash-table :test #'equal)))
    (cl-loop for xref in xrefs
             for key = (eglot-rcpp--xref-key xref)
             unless (gethash key seen)
             do (puthash key t seen)
             collect xref)))

(cl-defmethod xref-backend-definitions ((_backend (eql eglot-rcpp-project)) identifier)
  "Find IDENTIFIER definitions by combining local and project-wide Eglot data."
  (let ((root (eglot-rcpp--description-root)))
    (eglot-rcpp--dedupe-xrefs
     (cl-remove-if-not
      (lambda (xref) (eglot-rcpp--xref-in-root-p xref root))
      (append (ignore-errors (xref-backend-definitions 'eglot identifier))
              (eglot-rcpp--workspace-symbol-definitions identifier)
              (delq nil (eglot-rcpp--text-symbol-xrefs identifier)))))))

(cl-defmethod xref-backend-references ((_backend (eql eglot-rcpp-project)) identifier)
  "Defer IDENTIFIER references to the current buffer's Eglot server."
  (ignore-errors (xref-backend-references 'eglot identifier)))

(defun eglot-rcpp--install-project-root-markers ()
  "Teach project navigators about `DESCRIPTION'."
  (when eglot-rcpp-enable-project-root-markers
    (with-eval-after-load 'project
      (add-to-list 'project-vc-extra-root-markers eglot-rcpp-root-marker-file))
    (with-eval-after-load 'projectile
      (add-to-list 'projectile-project-root-files eglot-rcpp-root-marker-file))))

(defun eglot-rcpp--install-eglot-hooks ()
  "Install buffer hooks that should exist before Eglot loads."
  (dolist (mode (eglot-rcpp--all-managed-modes))
    (add-hook (eglot-rcpp--mode-hook mode) #'eglot-ensure)
    (add-hook (eglot-rcpp--mode-hook mode) #'eglot-rcpp-schedule-project-setup))
  (add-hook 'eglot-managed-mode-hook #'eglot-rcpp-install-project-xref-backend))

(defun eglot-rcpp--install-eglot-server-programs ()
  "Install server program entries after `eglot' loads."
  (with-eval-after-load 'eglot
    (when (eglot-rcpp--r-server-available-p)
      (add-to-list 'eglot-server-programs
                   `(,eglot-rcpp-r-modes . ,eglot-rcpp-r-server-command)))
    (when (and eglot-rcpp-clangd-command
               (listp eglot-rcpp-clangd-command)
               (executable-find (car eglot-rcpp-clangd-command)))
      (add-to-list 'eglot-server-programs
                   `(,eglot-rcpp-cpp-modes . ,eglot-rcpp-clangd-command)))))

(defun eglot-rcpp--augment-async-source (orig-fn servers)
  "Wrap ORIG-FN so `consult-eglot-symbols' also sees package headers."
  (ignore orig-fn)
  (lambda (async)
    (let ((generation 0)
          (base-results nil)
          (fallback-results nil))
      (lambda (action)
        (pcase action
          ('setup
           (setq generation 0
                 base-results nil
                 fallback-results nil)
           (funcall async 'setup))
          ((pred stringp)
           (cl-incf generation)
           (let ((query action)
                 (current-generation generation))
             (setq base-results nil
                   fallback-results
                   (unless (string-empty-p query)
                     (eglot-rcpp--text-symbols query nil t)))
             (cl-loop
              with responses = nil
              for server in servers
              do (jsonrpc-async-request
                  server :workspace/symbol
                  `(:query ,query)
                  :success-fn
                  (lambda (resp)
                    (when (= current-generation generation)
                      (setq responses (append responses resp nil))
                      (when consult-eglot-sort-results
                        (setq responses
                              (cl-sort responses #'>
                                       :key (lambda (candidate)
                                              (cl-getf candidate :score 0)))))
                      (setq base-results responses)
                      (funcall async 'flush)
                      (funcall async
                               (eglot-rcpp--dedupe-symbol-infos
                                (append base-results fallback-results)))))
                  :error-fn
                  (lambda (&rest _args)
                    (when (= current-generation generation)
                      (message "workspace/symbol request failed")))
                  :timeout-fn
                  (lambda ()
                    (when (= current-generation generation)
                      (message "error: request timed out")))))
             (funcall async action)
             (funcall async 'flush)
             (funcall async
                      (eglot-rcpp--dedupe-symbol-infos
                       (append base-results fallback-results)))))
          (_ (funcall async action)))))))

(defun eglot-rcpp--install-consult-support ()
  "Install the optional consult-eglot augmentation."
  (with-eval-after-load 'consult-eglot
    (unless (advice-member-p #'eglot-rcpp--augment-async-source
                             'consult-eglot--make-async-source)
      (advice-add 'consult-eglot--make-async-source
                  :around #'eglot-rcpp--augment-async-source))))

;;;###autoload
(defun eglot-rcpp-setup ()
  "Install all Eglot integration for mixed R/C++ packages."
  (interactive)
  (eglot-rcpp--install-project-root-markers)
  (eglot-rcpp--install-eglot-hooks)
  (eglot-rcpp--install-eglot-server-programs)
  (eglot-rcpp--install-consult-support))

(eglot-rcpp-setup)

(provide 'eglot-rcpp)

;;; eglot-rcpp.el ends here
