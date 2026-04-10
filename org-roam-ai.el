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
   "Prefer adding links to existing phrases already present in the note. "
   "Be sparing and only choose high-value links.")
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

(defcustom jds/org-roam-ai-max-inline-links 4
  "Maximum number of inline links to add per suggestion run."
  :type 'integer
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

(defun jds/org-roam-ai--point-in-org-link-p (pos)
  "Return non-nil when POS is inside an Org link."
  (save-excursion
    (goto-char pos)
    (let ((context (org-element-context)))
      (eq (org-element-type context) 'link))))

(defun jds/org-roam-ai--insert-inline-link-once (phrase id)
  "Link one unlinked occurrence of PHRASE to org-roam node ID.
Return non-nil if insertion happened."
  (save-excursion
    (goto-char (point-min))
    (let ((linked nil)
          (case-fold-search t))
      (while (and (not linked)
                  (search-forward phrase nil t))
        (let* ((end (point))
               (beg (- end (length phrase)))
               (matched-text (buffer-substring-no-properties beg end)))
          (unless (or (jds/org-roam-ai--point-in-org-link-p beg)
                      (jds/org-roam-ai--point-in-org-link-p end))
            (delete-region beg end)
            (goto-char beg)
            (insert (format "[[id:%s][%s]]" id matched-text))
            (setq linked t))))
      linked)))

(defun jds/org-roam-ai--parse-link-plan (response)
  "Parse RESPONSE into a list of (PHRASE ID TITLE RATIONALE).
Expected format per line: phrase<TAB>id<TAB>title<TAB>rationale."
  (let ((lines (split-string response "\n" t))
        items)
    (dolist (line lines (nreverse items))
      (let* ((clean-line (string-trim line))
             (parts (split-string clean-line "\t"))
             (parts (if (>= (length parts) 3)
                        parts
                      (split-string clean-line "|" t "[ \t]+"))))
        (when (>= (length parts) 3)
          (push (list (string-trim (nth 0 parts))
                      (string-trim (nth 1 parts))
                      (string-trim (nth 2 parts))
                      (string-trim (or (nth 3 parts) "")))
                items))))))

(defun jds/org-roam-ai--extract-org-id-links (response)
  "Extract (ID TITLE) pairs from org links in RESPONSE."
  (let ((start 0)
        links)
    (while (string-match "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" response start)
      (push (list (match-string 1 response)
                  (match-string 2 response))
            links)
      (setq start (match-end 0)))
    (nreverse links)))

(defun jds/org-roam-ai--response-link-candidates (response)
  "Return linking candidates parsed from RESPONSE.
Prefer explicit tab-separated plans, but fall back to org-link titles so
inline insertion still runs when the model replies with raw links."
  (let ((plan (jds/org-roam-ai--parse-link-plan response))
        (fallback-links (jds/org-roam-ai--extract-org-id-links response)))
    (if plan
        plan
      (mapcar (lambda (item)
                (pcase-let ((`(,id ,title) item))
                  (list title id title "")))
              fallback-links))))

(defun jds/org-roam-ai--append-related-links (links)
  "Append LINKS to a RELATED drawer.
LINKS should be a list of (ID TITLE)."
  (when links
    (save-excursion
      (goto-char (point-min))
      (let* ((case-fold-search t)
             (drawer-beg-regexp "^[ \t]*:RELATED:[ \t]*$")
             (drawer-end-regexp "^[ \t]*:END:[ \t]*$")
             (meta-bound (save-excursion
                           (if (search-forward-regexp org-heading-regexp nil t)
                               (line-beginning-position)
                             (point-max))))
             (inserted 0)
             (seen-ids (make-hash-table :test #'equal))
             beg end)
        ;; find existing RELATED drawer within metadata area
        (when (search-forward-regexp drawer-beg-regexp meta-bound t)
          (setq beg (line-beginning-position))
          (goto-char beg)
          (when (search-forward-regexp drawer-end-regexp meta-bound t)
            (setq end (line-beginning-position))))
        ;; create drawer if missing
        (unless (and beg end)
          (goto-char (point-min))
          (org-roam-end-of-meta-data)
          (unless (bolp) (insert "\n"))
          (insert ":RELATED:\n:END:\n")
          (setq beg (save-excursion
                      (search-backward ":RELATED:")
                      (line-beginning-position)))
          (goto-char beg)
          (search-forward-regexp drawer-end-regexp nil t)
          (setq end (line-beginning-position)))
        (save-excursion
          (goto-char beg)
          (while (re-search-forward "\\[\\[id:\\([^]]+\\)\\]\\[" end t)
            (puthash (match-string 1) t seen-ids)))
        ;; insert list items immediately before :END:, skipping duplicates
        (goto-char end)
        (dolist (item links inserted)
          (pcase-let ((`(,id ,title) item))
            (unless (gethash id seen-ids)
              (insert (format "- [[id:%s][%s]]\n" id title))
              (puthash id t seen-ids)
              (setq inserted (1+ inserted)))))))))

;;;###autoload
(defun jds/org-roam-ai-suggest-links (&optional ask-directive)
  "Suggest and insert sparse, high-value inline links in the current note."
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
                  (concat
                   "%s\n\nDirective: %s\n\nCurrent note:\n%s\n\nAvailable nodes (id | title):\n%s\n\n"
                   "Task:\n"
                   "1) Choose at most %d helpful links.\n"
                   "2) Prioritize phrases that already appear verbatim in the note body.\n"
                   "3) Avoid overwhelming the text; skip weak links.\n"
                   "4) Return ONLY tab-separated lines in this format:\n"
                   "phrase<TAB>id<TAB>title<TAB>brief rationale\n")
                  (jds/org-roam-ai--node-context)
                  directive
                  content
                  candidates
                  jds/org-roam-ai-max-inline-links)))
    (jds/org-roam-ai--request
     prompt
     jds/org-roam-ai-linking-system-prompt
     (lambda (response info)
       (if (not response)
           (message "org-roam-ai linking error: %s" (plist-get info :status))
         (with-current-buffer buf
           (let* ((plan (jds/org-roam-ai--response-link-candidates response))
                  (applied 0)
                  (related-added 0)
                  (unmatched nil))
             (dolist (entry plan)
               (pcase-let ((`(,phrase ,id ,_title ,_why) entry))
                 (cond
                  ((or (>= applied jds/org-roam-ai-max-inline-links)
                       (string-empty-p id))
                   nil)
                  ((or (and (not (string-empty-p phrase))
                            (jds/org-roam-ai--insert-inline-link-once phrase id))
                       (and (not (string-empty-p _title))
                            (jds/org-roam-ai--insert-inline-link-once _title id)))
                   (setq applied (1+ applied)))
                  (t
                   (push (list id _title) unmatched)))))
             (when (< applied jds/org-roam-ai-max-inline-links)
               (let ((remaining (- jds/org-roam-ai-max-inline-links applied))
                     (to-append nil))
                 (dolist (item (nreverse unmatched))
                   (when (> remaining 0)
                     (pcase-let ((`(,id ,title) item))
                       (push (list id title) to-append)
                       (setq remaining (1- remaining)))))
                 (setq to-append (nreverse to-append))
                 (setq related-added
                       (+ related-added
                          (jds/org-roam-ai--append-related-links to-append)))))
             (save-buffer)
             (message "org-roam-ai added %d inline link(s), %d related link(s)"
                      applied related-added))))))))

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
