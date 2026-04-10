;;; org-roam-ai.el --- AI helpers for org-roam notes -*- lexical-binding: t; -*-

(require 'subr-x)

(defgroup jds/org-roam-ai nil
  "AI-assisted workflows for org-roam."
  :group 'org-roam)

(defcustom jds/org-roam-ai-model nil
  "Optional model override for org-roam AI requests.

When nil, use the active `gptel-model'."
  :type '(choice (const :tag "Use gptel default" nil)
                 symbol)
  :group 'jds/org-roam-ai)

(defcustom jds/org-roam-ai-cleanup-system-prompt
  (concat
   "You are an editor for org-roam notes. Improve clarity and organization while preserving factual content. "
   "Do not invent citations or claims. Preserve valid Org syntax, links, properties, and drawers. "
   "Return only revised Org text.")
  "System prompt used when cleaning org-roam entries."
  :type 'string
  :group 'jds/org-roam-ai)

(defcustom jds/org-roam-ai-draft-system-prompt
  (concat
   "You draft concise, high-signal org-roam notes. Output valid Org content only. "
   "Use short sections and include a :RELATED: drawer with links when appropriate.")
  "System prompt used when drafting org-roam entries."
  :type 'string
  :group 'jds/org-roam-ai)

(defcustom jds/org-roam-ai-linking-system-prompt
  (concat
   "You suggest concept links for an org-roam note. "
   "Output only Org list items in this exact form: - [[id:ID][TITLE]] :: rationale")
  "System prompt used when proposing concept links."
  :type 'string
  :group 'jds/org-roam-ai)

(defcustom jds/org-roam-ai-cleanup-default-directive
  "Improve clarity, structure, and grammar while preserving meaning."
  "Default user directive used for cleanup when no prefix arg is supplied."
  :type 'string
  :group 'jds/org-roam-ai)

(defcustom jds/org-roam-ai-linking-default-directive
  "Prefer high-signal conceptual links and avoid weak or redundant links."
  "Default user directive used for link suggestion when no prefix arg is supplied."
  :type 'string
  :group 'jds/org-roam-ai)

(defun jds/org-roam-ai--in-roam-file-p ()
  "Return non-nil when current buffer is in `org-roam-directory'."
  (and (buffer-file-name)
       (boundp 'org-roam-directory)
       org-roam-directory
       (file-in-directory-p (file-truename (buffer-file-name))
                            (file-truename (expand-file-name org-roam-directory)))))

(defun jds/org-roam-ai--node-context ()
  "Build a compact context string for the current org-roam node."
  (let* ((node (org-roam-node-at-point))
         (title (or (and node (org-roam-node-title node)) ""))
         (tags (if node
                   (string-join (org-roam-node-tags node) ", ")
                 "")))
    (format "Title: %s\nTags: %s\n" title tags)))

(defun jds/org-roam-ai--request (prompt system callback)
  "Send PROMPT and SYSTEM to gptel using CALLBACK."
  (unless (fboundp 'gptel-request)
    (user-error "gptel is not available"))
  (let ((gptel-model (or jds/org-roam-ai-model gptel-model)))
    (gptel-request prompt
                   :system system
                   :buffer (current-buffer)
                   :callback callback)))

(defun jds/org-roam-ai--read-directive (prompt default)
  "Read a user directive with PROMPT, seeded by DEFAULT."
  (string-trim (read-string (format "%s (empty for default): " prompt) nil nil default)))

;;;###autoload
(defun jds/org-roam-ai-cleanup-entry (&optional ask-directive)
  "Clean up the current org-roam entry using AI and replace the buffer."
  (interactive "P")
  (unless (jds/org-roam-ai--in-roam-file-p)
    (user-error "This command is intended for org-roam files"))
  (let* ((buf (current-buffer))
         (directive (if ask-directive
                        (jds/org-roam-ai--read-directive
                         "Cleanup directive"
                         jds/org-roam-ai-cleanup-default-directive)
                      jds/org-roam-ai-cleanup-default-directive))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (prompt (format "%s\n\nDirective: %s\n\nPlease clean this note:\n\n%s"
                         (jds/org-roam-ai--node-context)
                         directive
                         content)))
    (jds/org-roam-ai--request
     prompt
     jds/org-roam-ai-cleanup-system-prompt
     (lambda (response info)
       (if (not response)
           (message "org-roam-ai cleanup error: %s" (plist-get info :status))
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert response)
             (goto-char (point-min))
             (save-buffer))
           (message "org-roam-ai cleanup complete")))))))

;;;###autoload
(defun jds/org-roam-ai-draft-entry (instruction &optional ask-directive)
  "Draft or expand the current entry from INSTRUCTION.

With an empty file, this writes a first pass note. With existing content,
it appends a new section at point."
  (interactive (list (read-string "What should this note cover? ")
                     current-prefix-arg))
  (unless (jds/org-roam-ai--in-roam-file-p)
    (user-error "This command is intended for org-roam files"))
  (let* ((buf (current-buffer))
         (directive (when ask-directive
                      (jds/org-roam-ai--read-directive
                       "Draft style/direction"
                       "Use concise structure and add actionable next links.")))
         (existing (buffer-substring-no-properties (point-min) (point-max)))
         (prompt (format "%s\nInstruction: %s\n%s\nCurrent note:\n%s"
                         (jds/org-roam-ai--node-context)
                         instruction
                         (if directive
                             (format "Additional directive: %s\n\n" directive)
                           "\n")
                         existing))
         (insert-point (point-marker)))
    (jds/org-roam-ai--request
     prompt
     jds/org-roam-ai-draft-system-prompt
     (lambda (response info)
       (if (not response)
           (message "org-roam-ai draft error: %s" (plist-get info :status))
         (with-current-buffer buf
           (goto-char insert-point)
           (unless (bolp) (insert "\n"))
           (insert response)
           (set-marker insert-point nil)
           (save-buffer)
           (message "org-roam-ai draft inserted")))))))

(defun jds/org-roam-ai--node-candidates-for-linking ()
  "Return a compact list of node candidates as strings for prompting."
  (mapcar (lambda (n)
            (format "- %s | %s"
                    (org-roam-node-id n)
                    (org-roam-node-title n)))
          (org-roam-node-list)))

;;;###autoload
(defun jds/org-roam-ai-suggest-links (&optional ask-directive)
  "Suggest concept links for current note and insert suggestions at point."
  (interactive "P")
  (unless (jds/org-roam-ai--in-roam-file-p)
    (user-error "This command is intended for org-roam files"))
  (let* ((buf (current-buffer))
         (directive (if ask-directive
                        (jds/org-roam-ai--read-directive
                         "Linking directive"
                         jds/org-roam-ai-linking-default-directive)
                      jds/org-roam-ai-linking-default-directive))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (candidates (string-join (jds/org-roam-ai--node-candidates-for-linking) "\n"))
         (prompt (format
                  "%s\n\nDirective: %s\n\nCurrent note:\n%s\n\nAvailable nodes (id | title):\n%s\n\nPick the most relevant links and return only Org list items."
                  (jds/org-roam-ai--node-context)
                  directive
                  content
                  candidates))
         (insert-point (point-marker)))
    (jds/org-roam-ai--request
     prompt
     jds/org-roam-ai-linking-system-prompt
     (lambda (response info)
       (if (not response)
           (message "org-roam-ai linking error: %s" (plist-get info :status))
         (with-current-buffer buf
           (goto-char insert-point)
           (insert "\n* AI link suggestions\n" response "\n")
           (set-marker insert-point nil)
           (save-buffer)
           (message "org-roam-ai link suggestions inserted")))))))

(defvar jds/org-roam-ai-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c n a c") #'jds/org-roam-ai-cleanup-entry)
    (define-key map (kbd "C-c n a d") #'jds/org-roam-ai-draft-entry)
    (define-key map (kbd "C-c n a l") #'jds/org-roam-ai-suggest-links)
    map)
  "Keymap for `jds/org-roam-ai-mode'.")

;;;###autoload
(define-minor-mode jds/org-roam-ai-mode
  "Minor mode for AI org-roam note helpers."
  :lighter " RoamAI"
  :keymap jds/org-roam-ai-mode-map)

;;;###autoload
(defun jds/org-roam-ai-maybe-enable ()
  "Enable `jds/org-roam-ai-mode' in org-roam note buffers."
  (if (jds/org-roam-ai--in-roam-file-p)
      (jds/org-roam-ai-mode 1)
    (jds/org-roam-ai-mode -1)))

;;;###autoload
(add-hook 'org-mode-hook #'jds/org-roam-ai-maybe-enable)

(provide 'org-roam-ai)
;;; org-roam-ai.el ends here
