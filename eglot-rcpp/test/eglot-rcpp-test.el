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

(ert-deftest eglot-rcpp-mode-remaps-consult-eglot-symbols ()
  (should (eq (lookup-key eglot-rcpp-mode-map [remap consult-eglot-symbols])
              #'eglot-rcpp-consult-symbols)))

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
    (should (gethash (eglot-rcpp--symbol-cache-key root nil) eglot-rcpp--symbol-cache))
    (should (gethash (eglot-rcpp--bridge-cache-key root) eglot-rcpp--bridge-cache))
    (eglot-rcpp-invalidate-project-cache root)
    (should-not (gethash (eglot-rcpp--symbol-cache-key root nil) eglot-rcpp--symbol-cache))
    (should-not (gethash (eglot-rcpp--bridge-cache-key root) eglot-rcpp--bridge-cache))))

(ert-deftest eglot-rcpp-missing-r-packages-helper-is-testable ()
  (let ((packages '("languageserver" "Rcpp" "usethis")))
    (should (equal (eglot-rcpp--missing-r-packages
                    packages
                    (lambda (package) (member package '("languageserver" "usethis"))))
                   '("Rcpp")))))

(provide 'eglot-rcpp-test)

;;; eglot-rcpp-test.el ends here
