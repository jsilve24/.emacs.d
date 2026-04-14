;;; git-gitignore.el --- gitignore helpers -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'seq)

(defcustom jds/gitignore-template-directory
  (expand-file-name "gitignore/" user-emacs-directory)
  "Directory containing local `.gitignore' template fragments.

Each template is stored as a `NAME.gitignore' file. This keeps the
catalog editable as ordinary files and makes it easy to sync from
external sources such as `github/gitignore'."
  :type 'directory
  :group 'tools)

(defun jds/gitignore--project-root ()
  "Return the current project root, falling back to `default-directory'."
  (or (when-let ((project (project-current nil)))
        (project-root project))
      (when (fboundp 'projectile-project-root)
        (ignore-errors (projectile-project-root)))
      default-directory))

(defun jds/gitignore--template-names ()
  "Return available gitignore template names."
  (when (file-directory-p jds/gitignore-template-directory)
    (mapcar #'file-name-base
            (sort (directory-files jds/gitignore-template-directory nil "\\.gitignore\\'")
                  #'string-lessp))))

(defun jds/gitignore--read-templates ()
  "Prompt for one or more gitignore templates."
  (completing-read-multiple
   "Gitignore template(s): "
   (jds/gitignore--template-names)
   nil t))

(defun jds/gitignore--render-template (name)
  "Render template NAME as a gitignore fragment."
  (when-let ((path (expand-file-name (format "%s.gitignore" name)
                                     jds/gitignore-template-directory)))
    (when (file-readable-p path)
      (concat "# " name "\n"
              (with-temp-buffer
                (insert-file-contents path)
                (string-trim-right (buffer-string)))
              "\n"))))

(defun jds/gitignore-insert-templates (templates)
  "Insert missing gitignore TEMPLATES into the current buffer."
  (let ((selected (delete-dups (seq-filter #'identity templates))))
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (dolist (name selected)
      (when-let ((fragment (jds/gitignore--render-template name)))
        (unless (save-excursion
                  (goto-char (point-min))
                  (search-forward fragment nil t))
          (unless (or (bobp)
                      (save-excursion
                        (forward-line -1)
                        (looking-at-p "^$")))
            (insert "\n"))
          (insert fragment))))))

(defun jds/gitignore-apply-templates (templates &optional root)
  "Append missing gitignore TEMPLATES to `.gitignore' under ROOT.

When ROOT is nil, use the current project root."
  (let* ((root (file-name-as-directory (or root (jds/gitignore--project-root))))
         (target (expand-file-name ".gitignore" root)))
    (find-file target)
    (jds/gitignore-insert-templates templates)
    (save-buffer)
    target))

;;;###autoload
(defun jds/project-insert-gitignore (templates)
  "Insert gitignore TEMPLATES into the current project's `.gitignore'.

When called interactively, prompt for one or more template names and
append any missing fragments to the project's `.gitignore' file."
  (interactive (list (jds/gitignore--read-templates)))
  (let ((target (jds/gitignore-apply-templates templates)))
    (message "Updated %s with %s"
             (abbreviate-file-name target)
             (string-join templates ", "))))

(provide 'git-gitignore)
