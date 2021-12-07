;;; debugging.el -- tools for debugging elisp/config -level bindings -*- lexical-binding: t; -*-

;; (use-package explain-pause-mode
;;   :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
;;   :config
;;   (explain-pause-mode))

;; (use-package bug-hunter
;;   :straight (bug-hunter :type git :host github :repo "Malabarba/elisp-bug-hunter"))

(with-eval-after-load 'debug
  (defun debugger-setup-buffer (debugger-args)
    "Initialize the `*Backtrace*' buffer for entry to the debugger.
That buffer should be current already."
    (setq buffer-read-only nil)
    (erase-buffer)
    (set-buffer-multibyte t)        ;Why was it nil ?  -stef
    (setq buffer-undo-list t)
    (let ((standard-output (current-buffer))
          (print-escape-newlines t)
          (print-level 8)
          (print-length 50))
      (backtrace))
    (goto-char (point-min))
    (delete-region (point)
                   (progn
                     (search-forward "\n  debug(")
                     (forward-line (if (eq (car debugger-args) 'debug)
                                       2    ; Remove implement-debug-on-entry frame.
                                     1))
                     (point)))
    (insert "Debugger entered")
    ;; lambda is for debug-on-call when a function call is next.
    ;; debug is for debug-on-entry function called.
    (pcase (car debugger-args)
      ((or `lambda `debug)
       (insert "--entering a function:\n"))
      ;; Exiting a function.
      (`exit
       (insert "--returning value: ")
       (setq debugger-value (nth 1 debugger-args))
       (prin1 debugger-value (current-buffer))
       (insert ?\n)
       (delete-char 1)
       (insert ? )
       (beginning-of-line))
      ;; Debugger entered for an error.
      (`error
       (insert "--Lisp error: ")
       (prin1 (nth 1 debugger-args) (current-buffer))
       (insert ?\n))
      ;; debug-on-call, when the next thing is an eval.
      (`t
       (insert "--beginning evaluation of function call form:\n"))
      ;; User calls debug directly.
      (_
       (insert ": ")
       (prin1 (if (eq (car debugger-args) 'nil)
                  (cdr debugger-args) debugger-args)
              (current-buffer))
       (insert ?\n)))
    ;; After any frame that uses eval-buffer,
    ;; insert a line that states the buffer position it's reading at.
    (save-excursion
      (let ((tem eval-buffer-list))
        (while (and tem
                    (re-search-forward "^  eval-\\(buffer\\|region\\)(" nil t))
          (beginning-of-line)
          (insert (format "Error at line %d in %s: "
                          (with-current-buffer (car tem)
                            (line-number-at-pos (point)))
                          (with-current-buffer (car tem)
                            (buffer-name))))
          (pop tem))))
    (debugger-make-xrefs)))

