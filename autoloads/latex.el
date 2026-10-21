;;; latex.el --- On-demand LaTeX helpers -*- lexical-binding: t; -*-

(require 'project)
(require 'files-x)
(require 'seq)

(defvar projectile-require-project-root)

;;;###autoload
(defun jds/latex-set-project-master ()
  "Choose a master file for the current LaTeX project.
Use Projectile's root when available, falling back to project.el.
Save an absolute `TeX-master' path in the root's .dir-locals.el,
preserving other settings.  Also update open LaTeX buffers beneath
that root so compilation uses the master immediately.  Explicit
file-local or nested directory-local settings can override this
project default when a buffer is subsequently reopened."
  (interactive)
  (unless (derived-mode-p 'latex-mode 'LaTeX-mode)
    (user-error "This command requires a LaTeX buffer"))
  (let* ((root (or (and (fboundp 'projectile-project-root)
                        (let ((projectile-require-project-root nil))
                          (projectile-project-root)))
                   (when-let* ((project (project-current nil)))
                     (project-root project))
                   (user-error "No Projectile or project.el project found")))
         (root (file-name-as-directory (expand-file-name root)))
         (master (expand-file-name
                  (read-file-name "Master LaTeX file: " root nil t)))
         (locals (expand-file-name ".dir-locals.el" root)))
    (unless (and (file-regular-p master)
                 (file-in-directory-p master root)
                 (string-equal (downcase (or (file-name-extension master) ""))
                               "tex"))
      (user-error "Choose an existing .tex file inside the project"))
    (unless enable-local-variables
      (user-error "Directory-local variables are disabled"))
    (when-let* ((buffer (get-file-buffer locals)))
      (when (buffer-modified-p buffer)
        (user-error "Save existing edits to %s first" locals)))
    ;; The built-in editor ignores read errors; reject malformed input first.
    (when (file-exists-p locals)
      (with-temp-buffer
        (insert-file-contents locals)
        (emacs-lisp-mode)
        (goto-char (point-min))
        (forward-comment (point-max))
        (unless (eobp)
          (let ((value (read (current-buffer))))
            (unless (and (listp value)
                         (seq-every-p
                          (lambda (entry)
                            (and (consp entry) (listp (cdr entry))))
                          value))
              (user-error "Invalid directory-local settings in %s" locals)))
          (forward-comment (point-max))
          (unless (eobp)
            (user-error "Unexpected trailing data in %s" locals)))))
    (save-window-excursion
      (save-excursion
        (add-dir-local-variable nil 'TeX-master master locals)
        (save-buffer)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and buffer-file-name
                   (derived-mode-p 'latex-mode 'LaTeX-mode)
                   (file-in-directory-p buffer-file-name root))
          (setq-local TeX-master master))))
    (message "Project LaTeX master: %s" (file-relative-name master root))))

;;; latex.el ends here
