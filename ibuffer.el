;;; ibuffer.el -- config for ibuffer -*- lexical-binding: t; -*-

(use-package ibuffer
  :straight (:type built-in))

(use-package ibuffer-projectile
  :after ibuffer
  ;; :disabled t
  :ensure t
  :config
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (ibuffer-projectile-set-filter-groups)
	      (unless (eq ibuffer-sorting-mode 'alphabetic)
		(ibuffer-do-sort-by-alphabetic)))))

;;; Autoloads

;;;###autoload
(defun jds~avy-ibuffer-cands ()
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (window-start) (window-end (selected-window) t))
      (goto-char (point-min))
      (setq pt (point))
      (text-property-search-forward 'ibuffer-properties)
      (beginning-of-line)
      (let ((candidates (list (cons (point) (selected-window)))))
	(while (not (equal pt (point)))
	  (setq pt (point))
	  (end-of-line)
	  (text-property-search-forward 'ibuffer-properties)
	  (beginning-of-line)
	  (push (cons (point) (selected-window)) candidates))
	(nreverse candidates)))))

;;;###autoload
(defun jds/avy-ibuffer ()
  "Goto a visible buffer listing in ibuffer."
  (interactive)
  (avy-action-goto (avy-with jds/avy-ibuffer
		     (avy-process (jds~avy-ibuffer-cands)))))

(defun jds/avy-ibuffer-and-go ()
  "Jump to visible buffer in ibuffer mode with hining."
  (interactive)
  (jds/avy-ibuffer)
  (ibuffer-visit-buffer))


(general-define-key
 :keymaps 'ibuffer-mode-map
 :states 'n
 "f" #'jds/avy-ibuffer
 "F" #'jds/avy-ibuffer-and-go)
