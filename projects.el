;;; projects.el --- projectile related config -*- lexical-binding: t; -*-

(use-package projectile
  ;; :disabled t
  :diminish projectile-mode
  :config
  (projectile-mode +1)

  (setq projectile-indexing-method 'hybrid
	projectile-globally-ignored-file-suffixes '("pygtex" "pygstyle" "fls" "aux" "synctex.gz" "fdb_latexmk" "bbl")))


(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))


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
  (ruled-switch-buffer-define tex-to-pdf
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
