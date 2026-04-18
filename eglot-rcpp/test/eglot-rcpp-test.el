;;; eglot-rcpp-test.el --- Tests for eglot-rcpp -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'eglot-rcpp)

(define-derived-mode eglot-rcpp-test-r-mode prog-mode "RcppTestR")
(define-derived-mode eglot-rcpp-test-cpp-mode prog-mode "RcppTestCpp")

(defmacro eglot-rcpp-test--with-modes (&rest body)
  "Run BODY with test-local mode customizations."
  `(let ((eglot-rcpp-r-modes '(eglot-rcpp-test-r-mode))
         (eglot-rcpp-cpp-modes '(eglot-rcpp-test-cpp-mode)))
     ,@body))

(defun eglot-rcpp-test--write-file (path content)
  "Write CONTENT to PATH, creating parent directories as needed."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert content)))

(defun eglot-rcpp-test--read-file (path)
  "Return the contents of PATH as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun eglot-rcpp-test--make-project ()
  "Create a temporary R package project and return its root."
  (let ((root (make-temp-file "eglot-rcpp-" t)))
    (eglot-rcpp-test--write-file
     (expand-file-name "DESCRIPTION" root)
     "Package: testpkg\nVersion: 0.0.1\n")
    root))

(defun eglot-rcpp-test--xref (file line &optional column summary)
  "Build an xref for FILE at LINE and COLUMN."
  (xref-make (or summary (file-name-nondirectory file))
             (xref-make-file-location file line (or column 0))))

(ert-deftest eglot-rcpp-root-and-file-discovery ()
  (eglot-rcpp-test--with-modes
   (let* ((root (eglot-rcpp-test--make-project))
          (r-file (expand-file-name "R/testpkg.R" root))
          (cpp-file (expand-file-name "src/testpkg.cpp" root))
          (header (expand-file-name "inst/include/testpkg.hpp" root))
          (generated (expand-file-name "src/RcppExports.cpp" root)))
     (eglot-rcpp-test--write-file r-file "hello <- function() {}\n")
     (eglot-rcpp-test--write-file cpp-file "int meaning() { return 42; }\n")
     (eglot-rcpp-test--write-file header "int header_decl();\n")
     (eglot-rcpp-test--write-file generated "int generated_decl();\n")
     (should (equal (eglot-rcpp--description-root (file-name-directory r-file)) root))
     (should (equal (eglot-rcpp--backend-file root) cpp-file))
     (should (equal (eglot-rcpp--r-package-file root) r-file))
     (should (member header (eglot-rcpp--files root nil nil)))
     (should-not (member generated (eglot-rcpp--files root nil nil))))))

(ert-deftest eglot-rcpp-clangd-fallback-flags-include-package-and-linkingto-headers ()
  (let* ((root (eglot-rcpp-test--make-project))
         (makevars (expand-file-name "src/Makevars" root))
         (description (expand-file-name "DESCRIPTION" root))
         (inst-include (expand-file-name "inst/include" root))
         (flags nil))
    (eglot-rcpp-test--write-file
     description
     "Package: testpkg\nVersion: 0.0.1\nLinkingTo: Rcpp, RcppEigen\n")
    (make-directory inst-include t)
    (eglot-rcpp-test--write-file
     makevars
     "CXX_STD = CXX17\nPKG_CPPFLAGS = -I../inst/include -DTESTPKG\n")
    (cl-letf (((symbol-function 'eglot-rcpp--linkingto-include-directories)
               (lambda (_packages) '("/opt/R/include" "/opt/RcppEigen/include"))))
      (setq flags (eglot-rcpp--clangd-fallback-flags root)))
    (should (member "-xc++" flags))
    (should (member "-std=gnu++17" flags))
    (should (member (concat "-I" (expand-file-name "inst/include" root)) flags))
    (should (member "-I/opt/R/include" flags))
    (should (member "-I/opt/RcppEigen/include" flags))
    (should (member "-DTESTPKG" flags))))

(ert-deftest eglot-rcpp-linkingto-include-directories-query-packages-individually ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'eglot-rcpp--r-command-lines)
               (lambda (&rest args)
                 (push args calls)
                 (cond
                  ((string-match-p "RcppEigen" (car (last args)))
                   '("/r/include" "/r/RcppEigen/include"))
                  ((string-match-p "Rcpp" (car (last args)))
                   '("/r/include" "/r/Rcpp/include"))
                  (t nil)))))
      (should (equal (eglot-rcpp--linkingto-include-directories '("Rcpp" "RcppEigen"))
                     '("/r/include" "/r/Rcpp/include" "/r/RcppEigen/include")))
      (should (= (length calls) 2)))))

(ert-deftest eglot-rcpp-clangd-compile-database-includes-package-sources-and-headers ()
  (let* ((root (eglot-rcpp-test--make-project))
         (description (expand-file-name "DESCRIPTION" root))
         (cpp-file (expand-file-name "src/testpkg.cpp" root))
         (header (expand-file-name "inst/include/testpkg.hpp" root)))
    (eglot-rcpp-test--write-file
     description
     "Package: testpkg\nVersion: 0.0.1\n")
    (eglot-rcpp-test--write-file cpp-file "int meaning() { return 42; }\n")
    (eglot-rcpp-test--write-file header "int header_decl();\n")
    (cl-letf (((symbol-function 'eglot-rcpp--clangd-fallback-flags)
               (lambda (_root) '("-xc++" "-I/tmp/pkg/src" "-I/tmp/pkg/inst/include"))))
      (let* ((dir (eglot-rcpp--clangd-compile-commands-dir root))
             (file (expand-file-name "compile_commands.json" dir))
             (contents (eglot-rcpp-test--read-file file)))
        (should (file-readable-p file))
        (should (string-match-p "testpkg\\.cpp" contents))
        (should (string-match-p "testpkg\\.hpp" contents))
        (should (string-match-p "c\\+\\+-header" contents))
        (should (string-match-p "clang\\+\\+" contents))))))

(ert-deftest eglot-rcpp-clangd-compile-database-detection-honors-ancestor-database ()
  (let* ((parent (make-temp-file "eglot-rcpp-clangd-parent-" t))
         (root (expand-file-name "pkg" parent)))
    (make-directory root t)
    (eglot-rcpp-test--write-file
     (expand-file-name "compile_commands.json" parent)
     "[]\n")
    (should (eglot-rcpp--clangd-compile-commands-present-p root))))

(ert-deftest eglot-rcpp-cpp-server-contact-is-gated-by-package-root ()
  (let ((project 'dummy-project)
        (root "/tmp/pkg"))
    (cl-letf (((symbol-function 'eglot-rcpp--project-root-from-project)
               (lambda (proj) (and (eq proj project) root)))
              ((symbol-function 'eglot-rcpp--clangd-fallback-flags)
               (lambda (_root) '("-xc++" "-I/tmp/pkg/inst/include")))
              ((symbol-function 'eglot-rcpp--clangd-compile-commands-dir)
               (lambda (_root) "/tmp/pkg/.eglot-rcpp/clangd/test"))
              ((symbol-function 'eglot-rcpp--clangd-compile-commands-present-p)
               (lambda (_root) nil)))
      (let ((contact (eglot-rcpp--cpp-server-contact nil project)))
        (should (equal (seq-take contact (+ (length eglot-rcpp-clangd-command) 1))
                       (append eglot-rcpp-clangd-command
                               '("--compile-commands-dir=/tmp/pkg/.eglot-rcpp/clangd/test"))))
        (should (equal (plist-get (nthcdr (+ (length eglot-rcpp-clangd-command) 1) contact)
                                  :initializationOptions)
                       '(:fallbackFlags ["-xc++" "-I/tmp/pkg/inst/include"])))))
    (cl-letf (((symbol-function 'eglot-rcpp--project-root-from-project)
               (lambda (_proj) nil)))
      (should-not (eglot-rcpp--cpp-server-contact nil project)))))

(ert-deftest eglot-rcpp-eligible-buffer-is-project-scoped ()
  (eglot-rcpp-test--with-modes
   (let* ((root (eglot-rcpp-test--make-project))
          (file (expand-file-name "R/testpkg.R" root))
          (other (make-temp-file "eglot-rcpp-outside-" nil ".R")))
     (eglot-rcpp-test--write-file file "hello <- function() {}\n")
     (with-temp-buffer
       (setq buffer-file-name file)
       (eglot-rcpp-test-r-mode)
       (should (eglot-rcpp--eligible-buffer-p)))
     (with-temp-buffer
       (setq buffer-file-name other)
       (eglot-rcpp-test-r-mode)
       (should-not (eglot-rcpp--eligible-buffer-p))))))

(ert-deftest eglot-rcpp-definitions-deprioritize-generated-hits ()
  (let* ((root (eglot-rcpp-test--make-project))
         (generated (expand-file-name "R/RcppExports.R" root))
         (real (expand-file-name "src/testpkg.cpp" root))
         (generated-xref (eglot-rcpp-test--xref generated 10))
         (real-xref (eglot-rcpp-test--xref real 12)))
    (let ((eglot-rcpp-generated-definition-policy 'deprioritize))
      (cl-letf (((symbol-function 'eglot-rcpp--project-root) (lambda (&optional _buffer) root))
                ((symbol-function 'eglot-rcpp--current-eglot-definitions)
                 (lambda (_identifier) (list generated-xref)))
                ((symbol-function 'eglot-rcpp--workspace-symbol-definitions)
                 (lambda (_identifier _root) (list real-xref generated-xref)))
                ((symbol-function 'eglot-rcpp--text-symbol-xrefs)
                 (lambda (&rest _args) nil)))
        (should (equal (xref-backend-definitions 'eglot-rcpp-project "meaning")
                       (list real-xref generated-xref)))))))

(ert-deftest eglot-rcpp-definitions-omit-generated-when-real-source-exists ()
  (let* ((root (eglot-rcpp-test--make-project))
         (generated (expand-file-name "src/RcppExports.cpp" root))
         (real (expand-file-name "src/testpkg.cpp" root))
         (generated-xref (eglot-rcpp-test--xref generated 4))
         (real-xref (eglot-rcpp-test--xref real 7)))
    (let ((eglot-rcpp-generated-definition-policy 'omit))
      (cl-letf (((symbol-function 'eglot-rcpp--project-root) (lambda (&optional _buffer) root))
                ((symbol-function 'eglot-rcpp--current-eglot-definitions)
                 (lambda (_identifier) (list generated-xref)))
                ((symbol-function 'eglot-rcpp--workspace-symbol-definitions)
                 (lambda (_identifier _root) (list real-xref)))
                ((symbol-function 'eglot-rcpp--text-symbol-xrefs)
                 (lambda (&rest _args) nil)))
        (should (equal (xref-backend-definitions 'eglot-rcpp-project "meaning")
                       (list real-xref)))))))

(ert-deftest eglot-rcpp-definitions-keep-generated-when-requested ()
  (let* ((root (eglot-rcpp-test--make-project))
         (generated (expand-file-name "R/RcppExports.R" root))
         (real (expand-file-name "src/testpkg.cpp" root))
         (generated-xref (eglot-rcpp-test--xref generated 10))
         (real-xref (eglot-rcpp-test--xref real 12)))
    (let ((eglot-rcpp-generated-definition-policy 'keep))
      (cl-letf (((symbol-function 'eglot-rcpp--project-root) (lambda (&optional _buffer) root))
                ((symbol-function 'eglot-rcpp--current-eglot-definitions)
                 (lambda (_identifier) (list generated-xref)))
                ((symbol-function 'eglot-rcpp--workspace-symbol-definitions)
                 (lambda (_identifier _root) (list real-xref)))
                ((symbol-function 'eglot-rcpp--text-symbol-xrefs)
                 (lambda (&rest _args) nil)))
        (should (equal (xref-backend-definitions 'eglot-rcpp-project "meaning")
                       (list generated-xref real-xref)))))))

(ert-deftest eglot-rcpp-definitions-fall-back-to-src-cpp-when-no-server-result-exists ()
  (let* ((root (eglot-rcpp-test--make-project))
         (cpp-file (expand-file-name "src/PibbleCollapsed_Optim.cpp" root))
         (bridge (expand-file-name "R/RcppExports.R" root)))
    (eglot-rcpp-test--write-file
     cpp-file
     (mapconcat
      #'identity
      '("#include <RcppEigen.h>"
        ""
        "List optimPibbleCollapsed(const Eigen::ArrayXXd Y,"
        "                          const double upsilon) {"
        "  return Rcpp::List::create();"
        "}")
      "\n"))
    (eglot-rcpp-test--write-file
     bridge
     (mapconcat
      #'identity
      '("optimPibbleCollapsed <- function(Y, upsilon) {"
        "  .Call(`_testpkg_optimPibbleCollapsed`, Y, upsilon)"
        "}")
      "\n"))
    (cl-letf (((symbol-function 'eglot-rcpp--project-root) (lambda (&optional _buffer) root))
              ((symbol-function 'eglot-rcpp--current-eglot-definitions)
               (lambda (_identifier) nil))
              ((symbol-function 'eglot-rcpp--workspace-symbol-definitions)
               (lambda (_identifier _root) nil)))
      (let ((definitions (xref-backend-definitions
                          'eglot-rcpp-project
                          "optimPibbleCollapsed")))
        (should definitions)
        (should (equal (eglot-rcpp--xref-location-path (car definitions))
                       cpp-file))))))

(ert-deftest eglot-rcpp-definitions-filter-external-results-by-default ()
  (let* ((root (eglot-rcpp-test--make-project))
         (local (expand-file-name "src/testpkg.cpp" root))
         (external "/usr/include/c++/15.2.1/bits/regex.h")
         (local-xref (eglot-rcpp-test--xref local 12))
         (external-xref (eglot-rcpp-test--xref external 460)))
    (let ((eglot-rcpp-restrict-xref-results-to-project t))
      (cl-letf (((symbol-function 'eglot-rcpp--project-root) (lambda (&optional _buffer) root))
                ((symbol-function 'eglot-rcpp--workspace-symbol-definitions)
                 (lambda (_identifier _root) nil))
                ((symbol-function 'eglot-rcpp--text-symbol-xrefs)
                 (lambda (&rest _args) nil))
                ((symbol-function 'eglot-rcpp--current-eglot-definitions)
                 (lambda (_identifier) (list external-xref local-xref))))
        (should (equal (xref-backend-definitions 'eglot-rcpp-project "optimize")
                       (list local-xref)))))))

(ert-deftest eglot-rcpp-definitions-can-keep-external-results-when-requested ()
  (let* ((root (eglot-rcpp-test--make-project))
         (local (expand-file-name "src/testpkg.cpp" root))
         (external "/usr/include/c++/15.2.1/bits/regex.h")
         (local-xref (eglot-rcpp-test--xref local 12))
         (external-xref (eglot-rcpp-test--xref external 460)))
    (let ((eglot-rcpp-restrict-xref-results-to-project nil))
      (cl-letf (((symbol-function 'eglot-rcpp--project-root) (lambda (&optional _buffer) root))
                ((symbol-function 'eglot-rcpp--workspace-symbol-definitions)
                 (lambda (_identifier _root) nil))
                ((symbol-function 'eglot-rcpp--text-symbol-xrefs)
                 (lambda (&rest _args) nil))
                ((symbol-function 'eglot-rcpp--current-eglot-definitions)
                 (lambda (_identifier) (list external-xref local-xref))))
        (should (equal (xref-backend-definitions 'eglot-rcpp-project "optimize")
                       (list external-xref local-xref)))))))

(ert-deftest eglot-rcpp-references-fall-back-to-r-files-from-cpp-symbols ()
  (let* ((root (eglot-rcpp-test--make-project))
         (r-file (expand-file-name "R/testpkg.R" root))
         (bridge (expand-file-name "R/RcppExports.R" root))
         (cpp-file (expand-file-name "src/testpkg.cpp" root)))
    (eglot-rcpp-test--write-file
     r-file
     (mapconcat
      #'identity
      '("#' optimPibbleCollapsed should be skipped in comments"
        "fit <- optimPibbleCollapsed(Y, upsilon)")
      "\n"))
    (eglot-rcpp-test--write-file
     bridge
     (mapconcat
      #'identity
      '("optimPibbleCollapsed <- function(Y, upsilon) {"
        "  .Call(`_testpkg_optimPibbleCollapsed`, Y, upsilon)"
        "}")
      "\n"))
    (eglot-rcpp-test--write-file
     cpp-file
     "List optimPibbleCollapsed(SEXP Y, SEXP upsilon) { return R_NilValue; }\n")
    (cl-letf (((symbol-function 'eglot-rcpp--project-root) (lambda (&optional _buffer) root))
              ((symbol-function 'eglot-rcpp--current-eglot-references)
               (lambda (_identifier) nil)))
      (let ((references (xref-backend-references
                         'eglot-rcpp-project
                         "optimPibbleCollapsed")))
        (should (= (length references) 3))
        (should (equal (car (mapcar #'eglot-rcpp--xref-location-path references))
                       r-file))
        (should (equal (car (last (mapcar #'eglot-rcpp--xref-location-path references)))
                       bridge))
        (should (member cpp-file
                        (mapcar #'eglot-rcpp--xref-location-path references)))
        (should (= (xref-file-location-line
                    (xref-item-location (car references)))
                   2))))))

(ert-deftest eglot-rcpp-mode-does-not-remap-consult-eglot-symbols-by-default ()
  (should-not (command-remapping 'consult-eglot-symbols eglot-rcpp-mode-map)))

(ert-deftest eglot-rcpp-setup-is-explicit-and-consult-integration-is-opt-in ()
  (let ((eglot-rcpp--hooks-installed nil)
        (eglot-rcpp--consult-advice-installed nil)
        (eglot-rcpp--consult-load-hook-installed nil)
        (eglot-rcpp-enable-consult-integration nil)
        (events nil))
    (cl-letf (((symbol-function 'eglot-rcpp--install-project-root-markers)
               (lambda () (push 'roots events)))
              ((symbol-function 'eglot-rcpp--install-eglot-server-programs)
               (lambda () (push 'servers events)))
              ((symbol-function 'eglot-rcpp--install-ess-support)
               (lambda () (push 'ess events)))
              ((symbol-function 'eglot-rcpp--install-hooks)
               (lambda () (push 'hooks events))))
      (should-not eglot-rcpp--hooks-installed)
      (eglot-rcpp-setup)
      (should (equal events '(hooks ess servers roots))))))

(ert-deftest eglot-rcpp-consult-integration-is-opt-in-and-removable ()
  (let ((eglot-rcpp--consult-advice-installed nil)
        (eglot-rcpp--consult-load-hook-installed nil)
        (eglot-rcpp-enable-consult-integration nil)
        (installed nil)
        (removed nil)
        (load-hook nil))
    (cl-letf (((symbol-function 'eglot-rcpp--ensure-consult-load-hook)
               (lambda () (setq load-hook t)))
              ((symbol-function 'eglot-rcpp--install-consult-advice)
               (lambda () (setq installed t)
                 (setq eglot-rcpp--consult-advice-installed t)))
              ((symbol-function 'advice-remove)
               (lambda (&rest _args) (setq removed t)))
              ((symbol-function 'featurep)
               (lambda (feature &optional _sub)
                 (eq feature 'consult-eglot))))
      (eglot-rcpp--sync-consult-integration)
      (should-not installed)
      (should-not load-hook)
      (setq eglot-rcpp-enable-consult-integration t)
      (eglot-rcpp--sync-consult-integration)
      (should load-hook)
      (should installed)
      (setq installed nil
            removed nil
            eglot-rcpp-enable-consult-integration nil)
      (eglot-rcpp--sync-consult-integration)
      (should removed))))

(ert-deftest eglot-rcpp-consult-eglot-dispatch-routes-only-package-buffers ()
  (let (called)
    (cl-letf (((symbol-function 'eglot-rcpp-consult-symbols)
               (lambda (&rest args)
                 (setq called (cons 'rcpp args))))
              ((symbol-function 'consult-eglot-symbols)
               (lambda (&rest args)
                 (setq called (cons 'consult args)))))
      (with-temp-buffer
        (eglot-rcpp-mode 1)
        (apply #'eglot-rcpp--consult-eglot-dispatch
               #'consult-eglot-symbols
               nil)
        (should (equal called '(rcpp))))
      (with-temp-buffer
        (setq called nil)
        (eglot-rcpp-mode 1)
        (apply #'eglot-rcpp--consult-eglot-dispatch
               #'consult-eglot-symbols
               '("foo"))
        (should (equal called '(rcpp "foo"))))
      (with-temp-buffer
        (setq called nil)
        (apply #'eglot-rcpp--consult-eglot-dispatch
               #'consult-eglot-symbols
               '("bar"))
        (should (equal called '(consult "bar")))))))

(ert-deftest eglot-rcpp-consult-source-skips-empty-query-requests ()
  (let (requests)
    (cl-letf (((symbol-function 'jsonrpc-async-request)
               (lambda (&rest args)
                 (push args requests)))
              ((symbol-function 'eglot-rcpp--consult-text-symbol-xrefs)
               (lambda (_pattern _root) nil)))
      (let* ((sink (lambda (_action) nil))
             (source (funcall (eglot-rcpp--consult-symbol-source '(dummy-server) "/tmp/pkg")
                              sink)))
        (funcall source 'setup)
        (funcall source "")
        (should-not requests)
        (funcall source "op")
        (should (= (length requests) 1))))))

(ert-deftest eglot-rcpp-consult-candidate-carries-kind-grouping ()
  (cl-letf (((symbol-function 'consult--format-file-line-match)
             (lambda (&rest _args) "example.cpp:7")))
    (let* ((file "/tmp/example.cpp")
           (xref (xref-make
                  (propertize "meaning" 'eglot-rcpp-kind 12)
                  (xref-make-file-location file 7 0)))
           (candidate (eglot-rcpp--consult-symbol-candidate xref "/tmp")))
      (should (eq (get-text-property 0 'consult--type candidate) ?f)))))

(ert-deftest eglot-rcpp-xrefs-deduplicate-cleanly ()
  (let* ((file "/tmp/example.cpp")
         (first (eglot-rcpp-test--xref file 3 0 "first"))
         (duplicate (eglot-rcpp-test--xref file 3 0 "duplicate"))
         (second (eglot-rcpp-test--xref file 8 2 "second")))
    (should (equal (eglot-rcpp--dedupe-xrefs (list first duplicate second))
                   (list first second)))))

(ert-deftest eglot-rcpp-symbol-search-backend-is-consult-free-and-deduplicated ()
  (let* ((root "/tmp/pkg")
         (first (eglot-rcpp-test--xref "/tmp/pkg/src/a.cpp" 3 0 "first"))
         (duplicate (eglot-rcpp-test--xref "/tmp/pkg/src/a.cpp" 3 0 "duplicate"))
         (second (eglot-rcpp-test--xref "/tmp/pkg/R/b.R" 7 0 "second")))
    (cl-letf (((symbol-function 'eglot-rcpp--project-root)
               (lambda (&optional _buffer) root))
              ((symbol-function 'eglot-rcpp--workspace-symbol-apropos)
               (lambda (_pattern _root) (list first duplicate)))
              ((symbol-function 'eglot-rcpp--text-symbol-xrefs)
               (lambda (&rest _args) (list duplicate second))))
      (should (equal (xref-backend-apropos 'eglot-rcpp-project "mean")
                     (list first second))))))

(ert-deftest eglot-rcpp-completion-merges-base-capf-with-rcpp-exports ()
  (eglot-rcpp-test--with-modes
   (let* ((root (eglot-rcpp-test--make-project))
          (bridge (expand-file-name "R/RcppExports.R" root))
          (file (expand-file-name "R/testpkg.R" root)))
     (eglot-rcpp-test--write-file
      bridge
      "meaning <- function() {\n  .Call(`_testpkg_meaning`)\n}\n")
     (eglot-rcpp-test--write-file file "mea")
     (with-temp-buffer
       (insert "mea")
       (setq buffer-file-name file)
       (goto-char (point-max))
       (eglot-rcpp-test-r-mode)
       (let* ((base-capf (lambda () (list 1 4 '("measure"))))
              (completion-at-point-functions
               (list #'eglot-rcpp-completion-at-point base-capf))
              (capf (eglot-rcpp-completion-at-point))
              (table (nth 2 capf)))
         (should (member "meaning" (all-completions "mea" table nil)))
         (should (member "measure" (all-completions "mea" table nil))))))))

(ert-deftest eglot-rcpp-companion-start-spec-prefers-missing-side ()
  (let ((root "/tmp/pkg"))
    (cl-letf (((symbol-function 'eglot-rcpp--backend-file)
               (lambda (_root) "/tmp/pkg/src/test.cpp"))
              ((symbol-function 'eglot-rcpp--r-package-file)
               (lambda (_root) "/tmp/pkg/R/test.R"))
              ((symbol-function 'eglot-rcpp--server-running-p)
               (lambda (_root _kind) nil))
              ((symbol-function 'eglot-rcpp--pending-companion-start-p)
               (lambda (_root _kind) nil))
              ((symbol-function 'eglot-rcpp--command-available-p)
               (lambda (_command) t))
              ((symbol-function 'eglot-rcpp--r-executable)
               (lambda () "/usr/bin/R")))
      (should (equal (eglot-rcpp--companion-start-spec root 'r)
                     '(:kind cpp :file "/tmp/pkg/src/test.cpp")))
      (should (equal (eglot-rcpp--companion-start-spec root 'cpp)
                     '(:kind r :file "/tmp/pkg/R/test.R"))))))

(ert-deftest eglot-rcpp-companion-start-spec-reports-missing-tools ()
  (let ((root "/tmp/pkg"))
    (cl-letf (((symbol-function 'eglot-rcpp--backend-file)
               (lambda (_root) "/tmp/pkg/src/test.cpp"))
              ((symbol-function 'eglot-rcpp--r-package-file)
               (lambda (_root) "/tmp/pkg/R/test.R"))
              ((symbol-function 'eglot-rcpp--server-running-p)
               (lambda (_root _kind) nil))
              ((symbol-function 'eglot-rcpp--pending-companion-start-p)
               (lambda (_root _kind) nil))
              ((symbol-function 'eglot-rcpp--command-available-p)
               (lambda (_command) nil))
              ((symbol-function 'eglot-rcpp--r-executable)
               (lambda () nil)))
      (should (equal (eglot-rcpp--companion-start-spec root 'r)
                     '(:warning missing-clangd)))
      (should (equal (eglot-rcpp--companion-start-spec root 'cpp)
                     '(:warning missing-r))))))

(ert-deftest eglot-rcpp-textual-symbol-extraction-is-line-based ()
  (should (equal (eglot-rcpp--candidate-symbol-from-line "namespace pkg {")
                 "pkg"))
  (should (equal (eglot-rcpp--candidate-symbol-from-line "class Widget {")
                 "Widget"))
  (should (equal (eglot-rcpp--candidate-symbol-from-line "inline int meaning(int x) {")
                 "meaning"))
  (should-not (eglot-rcpp--candidate-symbol-from-line "template <typename T>")))

(ert-deftest eglot-rcpp-cache-invalidation-clears-symbol-and-bridge-caches ()
  (let* ((root (eglot-rcpp-test--make-project))
         (cpp-file (expand-file-name "src/testpkg.cpp" root))
         (bridge (expand-file-name "R/RcppExports.R" root)))
    (eglot-rcpp-test--write-file cpp-file "int meaning() { return 42; }\n")
    (eglot-rcpp-test--write-file bridge "meaning <- function() {}\n")
    (eglot-rcpp--symbol-index root)
    (eglot-rcpp--bridge-export-candidates root)
    (puthash (eglot-rcpp--clangd-cache-key root)
             '(dummy-signature . ("-xc++"))
             eglot-rcpp--clangd-flags-cache)
    (should (gethash (eglot-rcpp--symbol-cache-key root nil) eglot-rcpp--symbol-cache))
    (should (gethash (eglot-rcpp--bridge-cache-key root) eglot-rcpp--bridge-cache))
    (should (gethash (eglot-rcpp--clangd-cache-key root) eglot-rcpp--clangd-flags-cache))
    (eglot-rcpp-invalidate-project-cache root)
    (should-not (gethash (eglot-rcpp--symbol-cache-key root nil) eglot-rcpp--symbol-cache))
    (should-not (gethash (eglot-rcpp--bridge-cache-key root) eglot-rcpp--bridge-cache))
    (should-not (gethash (eglot-rcpp--clangd-cache-key root) eglot-rcpp--clangd-flags-cache))))

(ert-deftest eglot-rcpp-missing-r-packages-helper-is-testable ()
  (let ((packages '("languageserver" "Rcpp" "usethis")))
    (should (equal (eglot-rcpp--missing-r-packages
                    packages
                    (lambda (package) (member package '("languageserver" "usethis"))))
                   '("Rcpp")))))

(provide 'eglot-rcpp-test)

;;; eglot-rcpp-test.el ends here
