;;; templates.el --- built-in auto-insert setup -*- lexical-binding: t; -*-

(require 'autoinsert)

(defun jds/auto-insert-project-title ()
  "Return a human-readable title derived from the current project."
  (let* ((root (or (when-let ((project (project-current nil)))
                     (project-root project))
                   default-directory))
         (name (file-name-nondirectory (directory-file-name root))))
    (replace-regexp-in-string "[-_]+" " " name)))

(defun jds/auto-insert-fill-defaults ()
  "Replace common template tokens in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "__TITLE__" nil t)
      (replace-match (jds/auto-insert-project-title) t t))))

(defun jds/auto-insert-gitignore ()
  "Populate a new `.gitignore' from local template fragments."
  (when-let ((templates (jds/gitignore--read-templates)))
    (jds/gitignore-insert-templates templates)))

(defun jds/auto-insert--snippet-body (snippet-file)
  "Return the snippet body from SNIPPET-FILE."
  (with-temp-buffer
    (insert-file-contents snippet-file)
    (goto-char (point-min))
    (unless (search-forward "# --\n" nil t)
      (user-error "Missing snippet separator in %s" snippet-file))
    (string-trim (buffer-substring-no-properties (point) (point-max)))))

(defun jds/auto-insert--expand-snippet-file (snippet-name snippet-file)
  "Expand SNIPPET-NAME, falling back to SNIPPET-FILE if needed."
  (let ((snippet (and (require 'yasnippet nil t)
                      (yas-lookup-snippet snippet-name))))
    (cond
     (snippet
      (yas-expand-snippet snippet))
     ((file-readable-p snippet-file)
      (let ((body (jds/auto-insert--snippet-body snippet-file)))
        (if (fboundp 'yas-expand-snippet)
            (yas-expand-snippet body)
          (insert body))))
     (t
      (user-error "Missing template %s" snippet-file)))))

(defun jds/auto-insert-tex-article-template ()
  "Populate a new `.tex' file from the `article-template' yasnippet."
  (jds/auto-insert--expand-snippet-file
   "article-template"
   (expand-file-name "snippets/latex-mode/article-template"
                     user-emacs-directory)))

(defun jds/auto-insert-emacs-lisp-package-header ()
  "Populate a new `.el' file from the `package-header' yasnippet."
  (jds/auto-insert--expand-snippet-file
   "package-header"
   (expand-file-name "snippets/emacs-lisp-mode/package-header"
                     user-emacs-directory)))

(defun jds/auto-insert-r-script-title-block ()
  "Populate a new `.R' file from the `script-title-block' yasnippet."
  (jds/auto-insert--expand-snippet-file
   "script-title-block"
   (expand-file-name "snippets/ess-r-mode/r-script-title-block"
                     user-emacs-directory)))

(defun jds/auto-insert-markdown-template ()
  "Populate a new Markdown file with a small project-oriented scaffold."
  (insert "---\n"
          "title: \"__TITLE__\"\n"
          "---\n\n"
          "# __TITLE__\n\n")
  (jds/auto-insert-fill-defaults))

(defun jds/auto-insert-rmarkdown-template ()
  "Populate a new R Markdown file with a simple analysis scaffold."
  (insert "---\n"
          "title: \"__TITLE__\"\n"
          "output: html_document\n"
          "---\n\n"
          "```{r setup, include=FALSE}\n"
          "knitr::opts_chunk$set(echo = TRUE)\n"
          "```\n\n"
          "# Introduction\n\n")
  (jds/auto-insert-fill-defaults))

(defun jds/auto-insert-quarto-template ()
  "Populate a new Quarto file with a simple analysis scaffold."
  (insert "---\n"
          "title: \"__TITLE__\"\n"
          "format: html\n"
          "---\n\n"
          "```{r}\n"
          "#| echo: true\n"
          "```\n\n"
          "# Introduction\n\n")
  (jds/auto-insert-fill-defaults))

(defun jds/auto-insert-sql-template ()
  "Populate a new SQL file with a small starter comment."
  (insert "-- __TITLE__\n\n"
          "SELECT 1;\n")
  (jds/auto-insert-fill-defaults))

(use-package autoinsert
  :straight nil
  :config
  (setq auto-insert-query nil)
  (auto-insert-mode 1)

  ;; Doom-style empty-file templates, expanded automatically on first visit.
  (define-auto-insert "\\.gitignore\\'" #'jds/auto-insert-gitignore)
  (define-auto-insert "\\.tex\\'" #'jds/auto-insert-tex-article-template)
  (define-auto-insert "\\.el\\'" #'jds/auto-insert-emacs-lisp-package-header)
  (define-auto-insert "\\.[Rr]\\'" #'jds/auto-insert-r-script-title-block)
  (define-auto-insert "\\.[Rr]md\\'" #'jds/auto-insert-rmarkdown-template)
  (define-auto-insert "\\.[Qq]md\\'" #'jds/auto-insert-quarto-template)
  (define-auto-insert "\\.md\\'" #'jds/auto-insert-markdown-template)
  (define-auto-insert "\\.sql\\'" #'jds/auto-insert-sql-template))

(provide 'templates)
