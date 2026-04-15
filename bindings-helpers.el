;;; bindings-helpers.el --- Shared helpers for top-level bindings -*- lexical-binding: t; -*-

;;;###autoload
(defun jds~kill-whole-line ()
  "Kill line, with prefix-arg kill entire line forwards and backwards to start of line."
  (interactive)
  (evil-delete-back-to-indentation))

;;;###autoload
(defun jds/evil-paste-from-clipboard ()
  "Paste from the GUI clipboard register."
  (interactive)
  (evil-paste-from-register ?\"))

;;;###autoload
(defun jds~blank-line-p ()
  "Return non-nil when point is currently on a blank line."
  (looking-at-p "^[[:space:]]*$"))

;;;###autoload
(defun jds/jump-delim ()
  "Move past a closing delimiter when point is on one."
  (interactive)
  (if (looking-at (rx (or (literal ")")
                          (literal "]")
                          (literal "}"))))
      (progn
        (forward-char)
        t)
    nil))

;;;###autoload
(defun jds/tab-dwim ()
  "Perform the most useful tab action for the current context."
  (interactive)
  (cond
   ((and (texmathp) (or (bound-and-true-p cdlatex-mode) org-cdlatex-mode))
    (cdlatex-tab))
   ((org-table-p)
    (org-table-next-field))
   ((and (or (org-at-drawer-p)
             (org-at-heading-p))
         (or (string= major-mode "org-mode")
             (string= major-mode "org-msg-edit-mode")))
    (org-cycle))
   ((and (bound-and-true-p corfu--candidates) (fboundp 'corfu-insert))
    (corfu-insert))
   ((yas-expand)
    nil)
   ((yas-active-snippets)
    (yas-next-field))
   ((and (looking-back "[^ \t\n]" 1)
         (completion-at-point))
    nil)
   ((and (texmathp) (jds/jump-delim))
    nil)
   ((and (string= major-mode "ess-r-mode") (looking-back "[^\s]"))
    (ess-indent-command))
   (t
    (indent-for-tab-command nil))))

;;;###autoload
(defun jds/completion-popup-visible-p ()
  "Return non-nil when the Corfu completion popup is active."
  (and (bound-and-true-p corfu-mode)
       (bound-and-true-p corfu--candidates)))

;;;###autoload
(defun jds/completion-accept-dwim ()
  "Accept the current Corfu candidate, or fall back to `jds/tab-dwim'."
  (interactive)
  (if (jds/completion-popup-visible-p)
      (corfu-insert)
    (jds/tab-dwim)))

;;;###autoload
(defun jds/completion-next-dwim ()
  "Move to the next Corfu candidate, or the next line otherwise."
  (interactive)
  (if (jds/completion-popup-visible-p)
      (corfu-next)
    (next-line)))

;;;###autoload
(defun jds/completion-previous-dwim ()
  "Move to the previous Corfu candidate, or the previous line otherwise."
  (interactive)
  (if (jds/completion-popup-visible-p)
      (corfu-previous)
    (previous-line)))

;;;###autoload
(defun jds/completion-abort-dwim ()
  "Dismiss the Corfu popup, or return to normal state otherwise."
  (interactive)
  (if (jds/completion-popup-visible-p)
      (corfu-quit)
    (evil-normal-state)))

;;;###autoload
(defun jds/completion-keys ()
  "Install local insert-state completion bindings for the current buffer."
  (evil-local-set-key 'insert (kbd "<tab>") #'jds/completion-accept-dwim)
  (evil-local-set-key 'insert (kbd "<down>") #'jds/completion-next-dwim)
  (evil-local-set-key 'insert (kbd "<up>") #'jds/completion-previous-dwim)
  (evil-local-set-key 'insert (kbd "<escape>") #'jds/completion-abort-dwim)
  (evil-local-set-key 'normal (kbd "<tab>") #'jds/tab-dwim))

(provide 'bindings-helpers)
;;; bindings-helpers.el ends here
