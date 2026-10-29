;;; ai-status.el --- Local agent usage popup -*- lexical-binding: t; -*-

(defconst jds/llm-status-script
  (expand-file-name "ai-status.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Helper used by `jds/llm-status'.")

(defface jds/llm-status-agent
  '((t (:inherit (bold font-lock-keyword-face))))
  "Face for agent headings in the status popup.")

(defvar jds/llm-status-font-lock-keywords
  '(("^LLM agent status$" . 'bold)
    ("^\\(?:Claude\\|Codex\\|Antigravity\\)$" . 'jds/llm-status-agent)
    ("\\([0-9]+\\(?:\\.[0-9]+\\)?\\)% used"
     (0 (let ((used (string-to-number (match-string 1))))
          (cond ((>= used 95) 'error)
                ((>= used 80) 'warning)
                (t 'success)))))
    ("^  live quota$" . 'success)
    ("^  cached .*" . 'shadow)
    ("resets .*$" . 'shadow)
    ("^  .*\\(?:unavailable\\|unreachable\\|not installed\\|not logged in\\).*$" . 'warning)
    ("^g:.*$" . 'shadow))
  "Highlight quota usage: green below 80%, amber from 80%, red from 95%.")

(define-derived-mode jds/llm-status-mode special-mode "LLM Status"
  "Read-only agent status with quota highlighting."
  (setq-local font-lock-defaults '(jds/llm-status-font-lock-keywords t))
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key (kbd "g") #'revert-buffer)
  (local-set-key (kbd "q") #'quit-window)
  (setq-local revert-buffer-function (lambda (&rest _) (jds/llm-status)))
  ;; Keep the popup's g/q keys above the global Evil g prefix.
  (when (fboundp 'evil-emacs-state) (evil-emacs-state)))

(defun jds/llm-status ()
  "Show Claude, Codex and Antigravity usage without blocking Emacs."
  (interactive)
  (let* ((python (executable-find "python3"))
         (buffer (get-buffer-create "*LLM Status*")))
    (unless python (user-error "Agent status needs python3"))
    (unless (get-buffer-process buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)) (erase-buffer))
        (jds/llm-status-mode))
      (make-process :name "llm-status" :buffer buffer
                    :command (list python "-u" jds/llm-status-script)
                    :noquery t :sentinel #'ignore))
    (display-buffer buffer)))

(defun jds/llm-status-toggle ()
  "Dismiss the current Popper popup, or show agent status, like s-n."
  (interactive)
  (if (and (bound-and-true-p popper-mode)
           (bound-and-true-p popper-open-popup-alist)
           (fboundp 'popper-toggle-latest))
      (popper-toggle-latest)
    (jds/llm-status)))

(global-set-key (kbd "s-a") #'jds/llm-status-toggle)
;; EXWM must also grab this key when focus is in an X application.
(with-eval-after-load 'exwm
  (exwm-input-set-key (kbd "s-a") #'jds/llm-status-toggle))

(provide 'ai-status)
;;; ai-status.el ends here
