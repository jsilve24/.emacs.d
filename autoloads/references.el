;;; references.el --- summary -*- lexical-binding: t -*-

;;;###autoload
(defun jds~citar-prioritize-local-bib (fun)
  "Run command FUN interactively setting citar-bibliography to local-bibliography if present."
  (let ((local-latex (citar-latex-local-bib-files))
	(local-org (citar-org-local-bib-files)))
    (if (or local-latex local-org)
	(let ((citar-bibliography (if local-latex
				      local-latex
				    local-org)))
	  (call-interactively fun))
      (call-interactively fun))))

;;;###autoload
(defun jds~citar-prioritize-global-bib (fun)
  "Run command FUN interactively setting citardybibliography to local-bibliography if present. C-u passed to fun."
  ;; use temp-buffer to ensure citar-bibliography does not include local bibs.
  (let ((global-bib (with-temp-buffer citar-bibliography)))
    (if global-bib
	(let ((citar-bibliography global-bib))
	  (call-interactively fun))
      (call-interactively fun))))


;;;###autoload
(defun jds/citar-insert-cite-prioritize-local-bib ()
  "Run citar-insert-cite but pioritize local bibliographies if present, otherwise use global."
  (interactive)
  (jds~citar-prioritize-local-bib #'citar-insert-citation))

;;;###autoload
(defun jds/citar-open-prioritize-global-bib ()
  "Run citar-open but prioritize global bibliographies if present, otherwise use local."
  (interactive)
  (jds~citar-prioritize-global-bib #'citar-open))
