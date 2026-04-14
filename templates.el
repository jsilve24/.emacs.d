;;; templates.el --- built-in auto-insert setup -*- lexical-binding: t; -*-

(require 'autoinsert)

(defcustom jds/auto-insert-directory
  (expand-file-name "auto-insert/" user-emacs-directory)
  "Directory containing file templates for `auto-insert-mode'."
  :type 'directory
  :group 'tools)

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

(use-package autoinsert
  :straight nil
  :config
  (setq auto-insert-query nil
        auto-insert-directory jds/auto-insert-directory)
  (auto-insert-mode 1)

  ;; Named files are a better fit than a custom project scaffolder when the
  ;; main task is just boilerplate for a new file.
  (define-auto-insert "\\.gitignore\\'" #'jds/auto-insert-gitignore)
  (define-auto-insert "README\\.org\\'" '["README.org" jds/auto-insert-fill-defaults])
  (define-auto-insert "manuscript\\.org\\'" "manuscript.org")
  (define-auto-insert "specific-aims\\.org\\'" "specific-aims.org")
  (define-auto-insert "research-strategy\\.org\\'" "research-strategy.org")
  (define-auto-insert "biosketch\\.org\\'" "biosketch.org")
  (define-auto-insert "budget-justification\\.org\\'" "budget-justification.org")
  (define-auto-insert "facilities-and-resources\\.org\\'" "facilities-and-resources.org")
  (define-auto-insert "project-summary\\.org\\'" "project-summary.org")
  (define-auto-insert "project-description\\.org\\'" "project-description.org")
  (define-auto-insert "current-and-pending\\.org\\'" "current-and-pending.org")
  (define-auto-insert "results-from-prior-support\\.org\\'" "results-from-prior-support.org")
  (define-auto-insert "references\\.org\\'" "references.org"))

(provide 'templates)
