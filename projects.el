;;; projects.el --- projectile related config -*- lexical-binding: t; -*-

(defcustom jds/project-templates
  '(("Journal article"
     :gitignore ("LaTeX" "R" "renv" "Emacs" "macOS")
     :files (("README.org" . "#+title: Journal Article\n\n* Scope\n\n* Submission target\n\n* Tasks\n")
             ("manuscript.org" . "#+title: Manuscript Draft\n#+startup: overview\n\n* Title\n\n* Abstract\n\n* Introduction\n\n* Methods\n\n* Results\n\n* Discussion\n\n* References\n")
             ("refs.bib" . "")
             ("Makefile" . "all:\n\tlatexmk -pdf manuscript.tex\n")
             ("figures/.gitkeep" . "")
             ("tables/.gitkeep" . "")
             ("data/.gitkeep" . "")
             ("notes/.gitkeep" . "")))
    ("R package"
     :gitignore ("R" "renv" "Emacs" "macOS")
     :files (("README.Rmd" . "---\ntitle: \"Package README\"\noutput: github_document\n---\n\n")
             ("DESCRIPTION" . "Package: changeme\nTitle: What the Package Does\nVersion: 0.0.0.9000\nAuthors@R: person(\"First\", \"Last\", email = \"you@example.com\", role = c(\"aut\", \"cre\"))\nDescription: Short package description.\nLicense: MIT + file LICENSE\nEncoding: UTF-8\nLazyData: true\nRoxygen: list(markdown = TRUE)\nRoxygenNote: 7.3.2\n")
             ("NAMESPACE" . "")
             (".Rbuildignore" . "^.*\\.Rproj$\n^\\.Rproj\\.user$\n^README\\.Rmd$\n^README\\.md$\n^LICENSE\\.md$\n")
             ("R/.gitkeep" . "")
             ("man/.gitkeep" . "")
             ("tests/testthat/.gitkeep" . "")
             ("data-raw/.gitkeep" . "")
             ("inst/extdata/.gitkeep" . "")))
    ("NIH grant"
     :gitignore ("LaTeX" "Emacs" "macOS")
     :files (("README.org" . "#+title: NIH Grant\n\n* Opportunity\n\n* Internal deadlines\n\n* Team\n")
             ("specific-aims.org" . "#+title: Specific Aims\n\n* Overall objective\n\n* Aim 1\n\n* Aim 2\n\n* Expected impact\n")
             ("research-strategy.org" . "#+title: Research Strategy\n\n* Significance\n\n* Innovation\n\n* Approach\n")
             ("biosketch.org" . "#+title: Biosketch Notes\n\n* Personal statement\n\n* Positions and honors\n\n* Contributions to science\n")
             ("budget-justification.org" . "#+title: Budget Justification\n\n* Personnel\n\n* Equipment\n\n* Travel\n\n* Other direct costs\n")
             ("facilities-and-resources.org" . "#+title: Facilities and Resources\n\n")
             ("refs.bib" . "")
             ("figures/.gitkeep" . "")
             ("attachments/.gitkeep" . "")))
    ("NSF grant"
     :gitignore ("LaTeX" "Emacs" "macOS")
     :files (("README.org" . "#+title: NSF Grant\n\n* Solicitation\n\n* Internal deadlines\n\n* Team\n")
             ("project-summary.org" . "#+title: Project Summary\n\n* Overview\n\n* Intellectual Merit\n\n* Broader Impacts\n")
             ("project-description.org" . "#+title: Project Description\n\n* Introduction\n\n* Prior work\n\n* Research plan\n\n* Broader impacts\n")
             ("budget-justification.org" . "#+title: Budget Justification\n\n* Personnel\n\n* Equipment\n\n* Travel\n\n* Participant support\n")
             ("current-and-pending.org" . "#+title: Current and Pending Support\n\n")
             ("results-from-prior-support.org" . "#+title: Results from Prior NSF Support\n\n")
             ("references.org" . "#+title: References Cited\n\n")
             ("figures/.gitkeep" . "")
             ("attachments/.gitkeep" . ""))))
  "Starter project scaffolds keyed by template name.

Each template may define `:gitignore' fragments and starter `:files'."
  :type '(repeat
          (list :tag "Project template"
                (string :tag "Name")
                (plist :tag "Template properties")))
  :group 'tools)

(defun jds/project-template-names ()
  "Return names of available `jds/project-templates'."
  (mapcar #'car jds/project-templates))

(defun jds/project-template-get (name)
  "Return the project template plist for NAME."
  (cdr (assoc-string name jds/project-templates t)))

(defun jds/project--write-file-if-missing (root relative-path content)
  "Create RELATIVE-PATH under ROOT with CONTENT when absent."
  (let ((target (expand-file-name relative-path root)))
    (make-directory (file-name-directory target) t)
    (unless (file-exists-p target)
      (with-temp-file target
        (insert content))
      target)))

;;;###autoload
(defun jds/project-scaffold (template directory)
  "Scaffold DIRECTORY using project TEMPLATE."
  (interactive
   (list
    (completing-read "Project template: "
                     (jds/project-template-names)
                     nil t)
    (read-directory-name "Project directory: "
                         (or (ignore-errors (projectile-project-root))
                             default-directory)
                         nil nil)))
  (let* ((root (file-name-as-directory (expand-file-name directory)))
         (spec (jds/project-template-get template))
         (gitignore-templates (plist-get spec :gitignore))
         (files (plist-get spec :files))
         created)
    (make-directory root t)
    (dolist (file files)
      (when-let ((path (jds/project--write-file-if-missing root (car file) (cdr file))))
        (push path created)))
    (when gitignore-templates
      (jds/gitignore-apply-templates gitignore-templates root))
    (dired root)
    (message "Scaffolded %s in %s (%d new files)"
             template
             (abbreviate-file-name root)
             (length created))))

(use-package projectile
  ;; :disabled t
  :diminish projectile-mode
  :config
  (projectile-mode +1)

  (setq projectile-indexing-method 'hybrid
	projectile-globally-ignored-file-suffixes '("pygtex" "pygstyle" "fls" "aux" "synctex.gz" "fdb_latexmk" "bbl")
	projectile-project-compilation-cmd "make -k "))


(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master")
  :config
  ;; consult-projectile--file calls projectile without setting default-directory,
  ;; so projectile-project-root falls back to a nil default-directory and
  ;; file-remote-p crashes. Bind default-directory to the project dir first.
  (advice-add 'consult-projectile--file :around
    (lambda (orig-fn dir &rest args)
      (let ((default-directory (expand-file-name dir)))
	(apply orig-fn dir args)))))


;;; better switch to other file (simpler at least) -----------------------------

(use-package ruled-switch-buffer
  :straight (ruled-switch-buffer :type git :host github :repo "kzkn/ruled-switch-buffer")
  :config

  (defun ruled-switch-buffer-other-window ()
    "ruled-switch-buffer but in other-window"
    (interactive)
    (let* ((buffer (current-buffer))
	   (window (selected-window)))
      (ruled-switch-buffer)
      (let* ((new-buffer (current-buffer)))
	(other-window 1)
	(switch-to-buffer new-buffer))
      (other-window 1)
      (switch-to-buffer buffer)
      (other-window 1)))

  ;; pdf
  (ruled-switch-buffer-define pdf-to
    :matcher (lambda (fn) (string-match ".pdf$" fn))
    :mappers ((lambda (fn) (replace-regexp-in-string "\\.pdf$" ".org" fn))
	      (lambda (fn) (replace-regexp-in-string "\\.pdf$" ".tex" fn))))

  ;; latex
  (ruled-switch-buffer-define tex-to-pdf
    :matcher (lambda (fn) (string-match ".tex$" fn))
    :mappers (lambda (fn) (replace-regexp-in-string "\\.tex$" ".pdf" fn)))

  ;; txt
  (ruled-switch-buffer-define txt-to-pdf
    :matcher (lambda (fn) (string-match ".txt$" fn))
    :mappers (lambda (fn) (replace-regexp-in-string "\\.txt$" ".org" fn)))

  ;; rmd
  (ruled-switch-buffer-define rmd-to
    :matcher (lambda (fn) (string-match ".[rR]md$" fn))
    :mappers ((lambda (fn) (replace-regexp-in-string "\\.[rR]md$" ".pdf" fn))
	      (lambda (fn) (replace-regexp-in-string "\\.[rR]md$" ".html" fn))))

  ;; org
  (ruled-switch-buffer-define org-to
    :matcher (lambda (fn) (string-match ".org$" fn))
    :mappers ((lambda (fn) (replace-regexp-in-string "\\.org$" ".pdf" fn))
	      (lambda (fn) (replace-regexp-in-string "\\.org$" ".html" fn))
	      (lambda (fn) (replace-regexp-in-string "\\.org$" ".txt" fn))
	      (lambda (fn) (replace-regexp-in-string "\\.org$" ".tex" fn))))

  ;; rules for c/c++
  (ruled-switch-buffer-define h-to-c
    :matcher (lambda (fn) (string-match ".h$" fn))
    :mappers ((lambda (fn) (replace-regexp-in-string "\\.h$" ".c" fn))
	      (lambda (fn) (replace-regexp-in-string "\\.h$" ".cc" fn))
	      (lambda (fn) (replace-regexp-in-string "\\.h$" ".cxx" fn))))

  (ruled-switch-buffer-define c-to-h
    :matcher (lambda (fn) (string-match ".c\\(c\\|xx\\)?$" fn))
    :mappers (lambda (fn) (replace-regexp-in-string "\\.c.*$" ".h" fn))))


(provide 'config-projects)
;;; projects.el ends here
