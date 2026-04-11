;;; gptel-reinforce-tools.el --- Tool definition macro for gptel-reinforce -*- lexical-binding: t; -*-

;; Author: Justin Silverman <justinsilverman@psu.edu>
;; Maintainer: Justin Silverman <justinsilverman@psu.edu>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9.9.4"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Provides `gptel-reinforce-define-tool', a macro that simultaneously
;; defines an interactive gptel command and registers its system prompt as
;; a gptel-reinforce artifact.  This closes the loop between writing a
;; custom AI tool and tracking / evolving its system prompt over time via
;; the gptel-reinforce feedback cycle (like/dislike → summarize → update).

;;; Code:

(require 'cl-lib)
(require 'gptel-reinforce-core)
(require 'gptel-reinforce-org)

(declare-function gptel-request "gptel" (prompt &rest args))


;;; Internal helpers -------------------------------------------------------

(defun gptel-reinforce-tools--live-system (artifact-name initial-system)
  "Return current artifact text for ARTIFACT-NAME, or INITIAL-SYSTEM when unset.
Reads from the artifact's current.org on disk; falls back to INITIAL-SYSTEM
when the artifact text is empty (e.g. first run before any updates)."
  (let* ((artifact (gptel-reinforce-get-artifact artifact-name))
         (text (when artifact
                 (plist-get (gptel-reinforce-org-read-current artifact) :text))))
    (if (string-empty-p (string-trim (or text "")))
        initial-system
      text)))

(defun gptel-reinforce-tools--replace-region-callback (artifact-name start end)
  "Return a callback that replaces START..END and annotates with ARTIFACT-NAME.
On success: deletes the region, inserts the response, then calls
`gptel-reinforce-track-output-region' so feedback commands work."
  (let ((buf (current-buffer)))
    (lambda (response info)
      (if (not response)
          (message "gptel error: %s" (plist-get info :status))
        (with-current-buffer buf
          (delete-region start end)
          (goto-char start)
          (let ((ins (point-marker)))
            (insert response)
            (gptel-reinforce-track-output-region artifact-name ins (point))
            (set-marker ins nil)
            (set-marker start nil)
            (set-marker end nil)))))))

(defun gptel-reinforce-tools--insert-at-point-callback (artifact-name pos)
  "Return a callback that inserts at POS and annotates with ARTIFACT-NAME.
On success: moves to POS, inserts the response, then calls
`gptel-reinforce-track-output-region' so feedback commands work."
  (let ((buf (current-buffer)))
    (lambda (response info)
      (if (not response)
          (message "gptel error: %s" (plist-get info :status))
        (with-current-buffer buf
          (goto-char pos)
          (let ((ins (point-marker)))
            (insert response)
            (gptel-reinforce-track-output-region artifact-name ins (point))
            (set-marker ins nil)
            (set-marker pos nil)))))))


;;; Public macro -----------------------------------------------------------

;;;###autoload
(defmacro gptel-reinforce-define-tool (name docstring &rest plist)
  "Define NAME as an interactive gptel command backed by a gptel-reinforce artifact.

The macro emits a `progn' that:

  1. Optionally registers a database (when :database is omitted).
  2. Calls `gptel-reinforce-register-artifact' at load time, seeding the
     artifact with the :system string on first load; subsequent loads
     preserve any updates made via the feedback cycle.
  3. Defines an interactive `defun' NAME that reads the *live* artifact
     text at call time (falling back to :system) and makes a
     `gptel-request' whose response is automatically annotated with
     `gptel-reinforce-track-output-region'.

Required plist keys:
  :system         Initial system prompt string.  Becomes the artifact's
                  initial text.  Also used as a fallback when the artifact
                  has not yet been updated.
  :prompt-fn      A lambda of the same arity as :args that returns the
                  user-prompt string passed to `gptel-request'.  May
                  freely call region / buffer functions — they execute
                  synchronously before the async request is sent.
  :callback       How to handle the LLM response.  One of:
                    :replace-region   Delete the active region, insert the
                                      response, annotate for feedback.
                    :insert-at-point  Insert at (point), annotate.
                    FUNCTION          A custom callback (response info);
                                      no automatic annotation.

Optional plist keys — command shape:
  :args           Lambda list for the generated defun.  Default: ().
  :interactive    Interactive spec string or form.  Default: \"\".
  :requires-region  Non-nil → guard with (unless (use-region-p) ...).
  :stream         Passed as :stream to `gptel-request'.  Default: nil.

Optional plist keys — artifact / database registration:
  :database       Name of an already-registered database.  When omitted
                  the macro auto-creates a database named
                  ARTIFACT-NAME + \"-db\" with a buffer-tracking
                  candidate-fn (item-key is buffer-file-name or buffer-name).
                  Re-registering with the same name is a no-op, so
                  reloading is safe.
  :artifact-name  String name for the artifact.
                  Default: (symbol-name NAME).
  :candidate-fn   Override the auto-database candidate-fn.  Ignored when
                  :database is supplied.
  :type           Artifact type string: \"prompt\", \"code\", \"rules\", etc.
  :auto-update    Non-nil → skip review step during artifact updates.
  :summarizer-system-prompt  Full system prompt for summarization.
  :summarizer-user-prompt    Task-specific guidance for the summarizer.
  :updater-system-prompt     Full system prompt for artifact updates.
  :updater-user-prompt       Task-specific guidance for the updater.
  :pre-update-hook           Function or list run before applying an update.
  :post-update-hook          Function or list run after applying an update.

Example — minimal, auto-creates database:

  (gptel-reinforce-define-tool my/rewrite
    \"Rewrite selection according to DIRECTIVE.\"
    :args (directive)
    :interactive \"sDirective: \"
    :requires-region t
    :system \"You are a writing assistant. Rewrite as directed.\"
    :prompt-fn (lambda (directive)
                 (format \"Directive: %s\\n\\nText:\\n\\n%s\"
                         directive
                         (buffer-substring-no-properties
                          (region-beginning) (region-end))))
    :callback :replace-region)

Example — sharing a pre-registered database:

  (gptel-reinforce-register-database
    :name \"writing\"
    :candidate-fn (lambda ()
                    (list :context
                          (list :item-key (or buffer-file-name (buffer-name))
                                :title (buffer-name)))))

  (gptel-reinforce-define-tool my/draft
    \"Insert a draft at point.\"
    :args (prompt)
    :interactive \"sDraft: \"
    :database \"writing\"
    :system \"You are a writing assistant.\"
    :prompt-fn (lambda (prompt) (format \"Draft: %s\" prompt))
    :callback :insert-at-point)"
  (declare (doc-string 2) (indent 2))
  (let* ((args          (or (plist-get plist :args) '()))
         (ispec         (or (plist-get plist :interactive) ""))
         (requires-rgn  (plist-get plist :requires-region))
         (system        (plist-get plist :system))
         (artifact-name (or (plist-get plist :artifact-name) (symbol-name name)))
         (prompt-fn     (plist-get plist :prompt-fn))
         (callback-spec (plist-get plist :callback))
         (stream        (plist-get plist :stream))
         ;; :database defaults to ARTIFACT-NAME + "-db"
         (database      (or (plist-get plist :database)
                            (concat artifact-name "-db")))
         (explicit-db   (not (null (plist-get plist :database))))
         (candidate-fn  (plist-get plist :candidate-fn))
         ;; Collect optional artifact registration keys
         (art-keys
          (cl-loop for k in '(:type :auto-update
                              :summarizer-system-prompt :summarizer-user-prompt
                              :updater-system-prompt    :updater-user-prompt
                              :pre-update-hook          :post-update-hook)
                   when (plist-member plist k)
                   append (list k (plist-get plist k))))
         ;; Gensyms for marker hygiene — avoid capture in prompt-fn closures
         (g-start (gensym "gr-start-"))
         (g-end   (gensym "gr-end-"))
         (g-pos   (gensym "gr-pos-"))
         (g-sys   (gensym "gr-sys-"))
         (g-prom  (gensym "gr-prom-")))
    `(progn
       ;; ---- 1. Auto-register database when :database was not supplied ----
       ,@(unless explicit-db
           `((unless (gptel-reinforce-get-database ,database)
               (gptel-reinforce-register-database
                :name ,database
                :candidate-fn ,(or candidate-fn
                                   `(lambda ()
                                      (list :context
                                            (list :item-key
                                                  (or buffer-file-name
                                                      (buffer-name))
                                                  :title
                                                  (buffer-name)))))))))

       ;; ---- 2. Register the artifact (idempotent after first load) -------
       (gptel-reinforce-register-artifact
        :name ,artifact-name
        :database ,database
        :initial-text ,system
        ,@art-keys)

       ;; ---- 3. Define the interactive command ----------------------------
       (defun ,name ,args
         ,docstring
         (interactive ,ispec)
         ,@(when requires-rgn
             '((unless (use-region-p) (user-error "Select a region first"))))
         ,(pcase callback-spec
            (':replace-region
             `(let* ((,g-start (copy-marker (region-beginning)))
                     (,g-end   (copy-marker (region-end) t))
                     (,g-sys   (gptel-reinforce-tools--live-system
                                ,artifact-name ,system))
                     (,g-prom  (funcall ,prompt-fn ,@args)))
                (gptel-request ,g-prom
                  :system ,g-sys
                  :buffer (current-buffer)
                  :stream ,stream
                  :callback (gptel-reinforce-tools--replace-region-callback
                             ,artifact-name ,g-start ,g-end))))
            (':insert-at-point
             `(let* ((,g-pos  (copy-marker (point)))
                     (,g-sys  (gptel-reinforce-tools--live-system
                               ,artifact-name ,system))
                     (,g-prom (funcall ,prompt-fn ,@args)))
                (gptel-request ,g-prom
                  :system ,g-sys
                  :buffer (current-buffer)
                  :stream ,stream
                  :callback (gptel-reinforce-tools--insert-at-point-callback
                             ,artifact-name ,g-pos))))
            (_
             `(let* ((,g-sys  (gptel-reinforce-tools--live-system
                               ,artifact-name ,system))
                     (,g-prom (funcall ,prompt-fn ,@args)))
                (gptel-request ,g-prom
                  :system ,g-sys
                  :buffer (current-buffer)
                  :stream ,stream
                  :callback ,callback-spec))))))))

(provide 'gptel-reinforce-tools)

;;; gptel-reinforce-tools.el ends here
