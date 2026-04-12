;;; ai.el --- summary -*- lexical-binding: t -*-

(use-package gptel
  :init
  (setq gptel-model 'gpt-5-mini)
  :config
  ;; --- OpenAI backend ---
  (setq gptel-backend (gptel-make-openai "OpenAI"
                        :stream t
                        :key (auth-source-pick-first-password :host "api.openai.com" :user "apikey")
                        :request-params '(:reasoning_effort "high")))

  ;; --- Claude backend ---
  (gptel-make-anthropic "Claude"
    :stream t
    :key (auth-source-pick-first-password :host "api.anthropic.com" :user "apikey"))
  
  ;; --- Gemini backend ---
  (gptel-make-gemini "Gemini"
    :stream t
    :key (auth-source-pick-first-password :host "generativelanguage.googleapis.com" :user "apikey"))
  
  (setq gptel-highlight-methods '(face))
  (gptel-highlight-mode 1)
  (setq gptel-default-mode 'org-mode
	gptel-org-branching-context nil)
  
  ;; --- LaTeX writing system prompt ---
  (defvar jds/gptel-latex-system
    "You are an expert scientific writing assistant. Rules:
1. Preserve ALL LaTeX commands, environments, and macros exactly as written.
2. Return ONLY the requested text — no preamble, no explanation, no markdown code fences.
3. Use formal, precise academic prose appropriate for peer-reviewed journals.
4. Match the style and register of the surrounding text.")

)

;;; gptel-reinforce ------------------------------------------------------------
(defun jds/gptel-reinforce-stale-build-p (dir)
  "Return non-nil when gptel-reinforce sources in DIR need recompilation."
  (catch 'stale
    (dolist (source (directory-files dir t "\\.el\\'"))
      (let ((bytecode (concat source "c")))
        (when (or (not (file-exists-p bytecode))
                  (file-newer-than-file-p source bytecode))
          (throw 'stale t))))
    nil))

(defun jds/gptel-reinforce-clear-build-artifacts (dir)
  "Delete stale byte-compiled and native-compiled artifacts for DIR."
  (dolist (bytecode (directory-files dir t "\\.elc\\'"))
    (delete-file bytecode))
  (when-let* ((eln-cache-dir (expand-file-name "eln-cache" user-emacs-directory))
              ((file-directory-p eln-cache-dir)))
    (dolist (eln (directory-files-recursively eln-cache-dir "gptel-reinforce.*\\.eln\\'"))
      (delete-file eln))))

(let ((gptel-reinforce-dir
       (expand-file-name "gptel-reinforce" user-emacs-directory)))
  ;; Local package development can leave dependents compiled against an older
  ;; struct layout, which breaks startup before recompilation.
  (when (jds/gptel-reinforce-stale-build-p gptel-reinforce-dir)
    (jds/gptel-reinforce-clear-build-artifacts gptel-reinforce-dir))
  (add-to-list 'load-path gptel-reinforce-dir))
(require 'gptel-reinforce)
(require 'gptel-reinforce-elfeed)
(setq gptel-reinforce-summary-review-mode 'edit
      gptel-reinforce-update-review-mode 'edit)

(defconst jds/gptel-reinforce-transient-context-ttl 120
  "Seconds that transient item-feedback context remains valid.")

(defun jds/gptel-reinforce--context-fresh-p (context)
  "Return non-nil when CONTEXT is recent enough for transient feedback routing."
  (let ((timestamp (plist-get context :timestamp)))
    (and timestamp
         (< (- (float-time) timestamp)
            jds/gptel-reinforce-transient-context-ttl))))

(defun jds/gptel-reinforce--clear-transient-context (database-name)
  "Clear transient feedback-routing state for DATABASE-NAME in the current buffer."
  (pcase database-name
    ("tab-dwim" (kill-local-variable 'jds/tab-dwim--last-context))
    ("aas-snippets" (kill-local-variable 'jds/aas-snippets--last-context))))

(with-eval-after-load 'gptel-reinforce-ui
  (advice-add
   'gptel-reinforce--record-item-feedback :around
   (lambda (orig score prefix &optional database)
     (let* ((resolved (gptel-reinforce-resolve-database-and-context database nil))
            (db (car resolved))
            (database-name (and db (gptel-reinforce-database-name db))))
       (prog1 (funcall orig score prefix database)
         (jds/gptel-reinforce--clear-transient-context database-name))))))

;;; tab-dwim artifact ---------------------------------------------------------

(defconst jds/tab-dwim-artifact "tab-dwim"
  "Artifact name for the jds/tab-dwim function block.")

(defconst jds/tab-dwim-bindings-file
  (expand-file-name "bindings.el" user-emacs-directory)
  "File containing the tab-dwim definitions.")

(defun jds/tab-dwim-artifact--read ()
  "Extract the tab-dwim block from `bindings.el'."
  (with-temp-buffer
    (insert-file-contents jds/tab-dwim-bindings-file)
    (goto-char (point-min))
    (when (search-forward ";;; tab-dwim-begin\n" nil t)
      (let ((start (point)))
        (when (search-forward "\n;;; tab-dwim-end" nil t)
          (string-trim
           (buffer-substring-no-properties start (match-beginning 0))))))))

(defun jds/tab-dwim-artifact--write-back (text)
  "Replace the tab-dwim block in `bindings.el' with TEXT and re-eval."
  (with-current-buffer (find-file-noselect jds/tab-dwim-bindings-file)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward ";;; tab-dwim-begin\n" nil t)
        (let ((start (point)))
          (when (search-forward "\n;;; tab-dwim-end" nil t)
            (delete-region start (match-beginning 0))
            (goto-char start)
            (insert (string-trim text))))))
    (save-buffer))
  (condition-case err
      (eval (read (format "(progn %s)" text)) t)
    (error (message "tab-dwim update error: %s" (error-message-string err)))))

(defvar-local jds/tab-dwim--last-context nil
  "Transient context for routing feedback about the most recent `jds/tab-dwim'.")

(with-eval-after-load "bindings"
  (advice-add 'jds/tab-dwim :after
              (lambda (&rest _)
                (setq-local jds/tab-dwim--last-context
                            (list :mode major-mode
                                  :timestamp (float-time))))))

(gptel-reinforce-register-database
 :name "tab-dwim"
 :candidate-fn (lambda ()
                 (when (jds/gptel-reinforce--context-fresh-p
                        jds/tab-dwim--last-context)
                   (list :context
                         (list :item-key (format "tab-dwim:%s" major-mode)
                               :title    (format "tab-dwim in %s" major-mode)
                               :meta     (list :mode (symbol-name major-mode)))))))

(gptel-reinforce-register-artifact
 :name jds/tab-dwim-artifact
 :database "tab-dwim"
 :type "code"
 :initial-text (or (jds/tab-dwim-artifact--read) "")
 :post-update-hook #'jds/tab-dwim-artifact--write-back)

;;; aas snippets artifact -----------------------------------------------------

(defconst jds/aas-snippets-artifact "aas-snippets"
  "Artifact name for the aas snippet registration block.")

(defconst jds/aas-snippets-file
  (expand-file-name "snippets.el" user-emacs-directory)
  "File containing the aas snippet definitions.")

(defun jds/aas-snippets--read ()
  "Extract the aas snippet registration block from `snippets.el'."
  (with-temp-buffer
    (insert-file-contents jds/aas-snippets-file)
    (goto-char (point-min))
    (when (search-forward ";;; aas-snippets-begin\n" nil t)
      (let ((start (point)))
        (when (search-forward "\n  ;;; aas-snippets-end" nil t)
          (string-trim
           (buffer-substring-no-properties start (match-beginning 0))))))))

(defun jds/aas-snippets--write-back (text)
  "Replace the aas snippet block in `snippets.el' with TEXT and re-eval."
  (with-current-buffer (find-file-noselect jds/aas-snippets-file)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward ";;; aas-snippets-begin\n" nil t)
        (let ((start (point)))
          (when (search-forward "\n  ;;; aas-snippets-end" nil t)
            (let ((end (match-beginning 0)))
              (delete-region start end)
              (goto-char start)
              (insert (string-trim text)))))))
    (save-buffer))
  (condition-case err
      (eval (read (format "(progn %s)" text)) t)
    (error (message "aas-snippets update error: %s" (error-message-string err)))))

(defvar-local jds/aas-snippets--last-context nil
  "Plist :mode :before captured at the last aas pre-expansion hook.")

(defun jds/aas-snippets--pre-expand-hook ()
  "Record mode + surrounding text before aas expands a snippet."
  (setq jds/aas-snippets--last-context
        (list :mode major-mode
              :timestamp (float-time)
              ;; ~60 chars before point: shows what was typed + trigger key
              :before (buffer-substring-no-properties
                       (max (point-min) (- (point) 60))
                       (point))
              ;; a little after point: shows what the snippet will land in
              :after  (buffer-substring-no-properties
                       (point)
                       (min (point-max) (+ (point) 40))))))

(with-eval-after-load 'aas
  (add-hook 'aas-pre-snippet-expand-hook #'jds/aas-snippets--pre-expand-hook))

(gptel-reinforce-register-database
 :name "aas-snippets"
 :candidate-fn (lambda ()
                 (when (jds/gptel-reinforce--context-fresh-p
                        jds/aas-snippets--last-context)
                   (let ((mode   (plist-get jds/aas-snippets--last-context :mode))
                         (before (plist-get jds/aas-snippets--last-context :before))
                         (after  (plist-get jds/aas-snippets--last-context :after)))
                     (list :context
                           (list :item-key (format "aas:%s" mode)
                                 :title    (format "aas snippets in %s" mode)
                                 :meta     (list :mode   (symbol-name mode)
                                                 :before before
                                                 :after  after)))))))

(gptel-reinforce-register-artifact
 :name jds/aas-snippets-artifact
 :database "aas-snippets"
 :type "code"
 :initial-text (or (jds/aas-snippets--read) "")
 :summarizer-user-prompt
 (concat
  "Summarize feedback snippet by snippet and mode by mode.\n"
  "Group observations by major-mode first (e.g., org-mode, LaTeX-mode, ess-r-mode).\n"
  "Within each mode, identify which specific trigger keys (visible as the last\n"
  "characters in the :before context) caused problems and what went wrong:\n"
  "wrong expansion text, unexpected trigger, bad :cond predicate, missing snippet, etc.\n"
  "Cross-mode patterns (e.g., a trigger that misfires in multiple modes) should be\n"
  "called out separately.")
 :updater-user-prompt
 (concat
  "Update the aas snippet definitions based on the feedback summary.\n"
  "Work mode by mode and snippet by snippet.\n"
  "When a trigger fires in the wrong context, tighten its :cond predicate.\n"
  "When an expansion text is wrong, fix the string or yas template.\n"
  "When a snippet is consistently missing in a mode, add it.\n"
  "Preserve the structure: keep mode-setup macros and their application calls intact.\n"
  "Do not change helper macro definitions (jds~yas-lambda-expand, jds~aas-insert-math, etc.)\n"
  "— those live outside the artifact block.")
 :post-update-hook #'jds/aas-snippets--write-back)

;; --- LaTeX writing tools -----------------------------------------------------

(gptel-reinforce-register-database
  :name "latex-writing"
  :candidate-fn (lambda ()
                  (when (and (derived-mode-p 'LaTeX-mode) buffer-file-name)
                    (list :context
                          (list :item-key buffer-file-name
                                :title (buffer-name))))))

(gptel-reinforce-define-tool jds/gptel-rewrite
  "Rewrite the selected region according to DIRECTIVE."
  :args (directive)
  :interactive "sDirective: "
  :requires-region t
  :database "latex-writing"
  :system jds/gptel-latex-system
  :prompt-fn (lambda (directive)
               (format "Directive: %s\n\nText to rewrite:\n\n%s"
                       directive
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))))
  :callback :replace-region)

(gptel-reinforce-define-tool jds/gptel-rewrite-with-context
  "Rewrite the selected region using DIRECTIVE, with the full buffer as context."
  :args (directive)
  :interactive "sDirective: "
  :requires-region t
  :database "latex-writing"
  :system jds/gptel-latex-system
  :prompt-fn (lambda (directive)
               (format "Full document for context:\n\n%s\n\n---\n\nDirective: %s\n\nRewrite this passage from the document above:\n\n%s"
                       (buffer-substring-no-properties (point-min) (point-max))
                       directive
                       (buffer-substring-no-properties (region-beginning) (region-end))))
  :callback :replace-region)

(gptel-reinforce-define-tool jds/gptel-draft-at-point
  "Draft new LaTeX content at point based on PROMPT, using the buffer as context."
  :args (prompt)
  :interactive "sDraft: "
  :database "latex-writing"
  :system jds/gptel-latex-system
  :prompt-fn (lambda (prompt)
               (format "Document context:\n\n%s\n\n---\n\nDraft the following in LaTeX, fitting the style and flow of the document above. Return only the LaTeX text:\n\n%s"
                       (buffer-substring-no-properties (point-min) (point-max))
                       prompt))
  :callback :insert-at-point)


;;; gptel-quick ----------------------------------------------------------------
(use-package gptel-quick
  :straight (gptel-quick :type git :host github :repo "karthink/gptel-quick")
  :config
  (keymap-set embark-general-map "?" #'gptel-quick))

;; https://github.com/karthink/gptel-agent
(use-package gptel-agent
  :straight t ;use :ensure t for Elpaca
  :config (gptel-agent-update))         ;Read files from agents directories

(use-package gptel-magit
  :straight t
  :after (gptel magit)
  :hook (magit-mode . gptel-magit-install))


(use-package gptel-aibo
  :straight t
  :after gptel
  :config
  ;; Buffer-local evil bindings beat gptel-mode's evil keymap
  (add-hook 'gptel-aibo-mode-hook
            (lambda ()
              (evil-local-set-key 'normal (kbd "RET") #'gptel-aibo-send)
              (evil-local-set-key 'insert (kbd "RET") #'gptel-aibo-send))))

;;; CLAUDE
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :after web-server
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)
  (defun jds/claude-code-evil-setup ()
    "Send C-o and C-e through to vterm, bypassing evil bindings."
    (evil-local-set-key 'insert (kbd "C-o") #'vterm--self-insert)
    (evil-local-set-key 'insert (kbd "C-e") #'vterm--self-insert)
    (evil-local-set-key 'normal (kbd "C-o") #'vterm--self-insert)
    (evil-local-set-key 'normal (kbd "C-e") #'vterm--self-insert))
  (add-hook 'claude-code-mode-hook #'jds/claude-code-evil-setup)

;;; CLAUDE in R
  ;; Send region to R and return output
  (defun jds/ess-command (cmd)
    "Run CMD synchronously in R and return output string."
    (with-current-buffer (get-buffer "*R*")
      (ess-string-command cmd)))

  (claude-code-ide-make-tool
   :function #'jds/ess-command
   :name "r_eval"
   :description "Evaluate R code in the running ESS/R session and return output"
   :args '((:name "code" :type string :description "R expression to evaluate")))

;;; project aware ripgrep
  (defun jds/project-rg (pattern)
    "Search PATTERN in project using ripgrep."
    (claude-code-ide-mcp-server-with-session-context nil
						     (shell-command-to-string
						      (format "rg -n --no-heading '%s' %s" pattern default-directory))))

  (claude-code-ide-make-tool
   :function #'jds/project-rg
   :name "project_search"
   :description "Search for a pattern in the current project using ripgrep"
   :args '((:name "pattern" :type string :description "Regex pattern to search")))
  )

;;; agent-shell ----------------------------------------------------------------

(use-package agent-shell
  :ensure t
  :config
  (defun jds/agent-shell-backtab-dwim ()
    "Move to the previous completion candidate, or start completion."
    (interactive)
    (condition-case nil
        (if (and (fboundp 'corfu-previous)
                 (bound-and-true-p corfu-mode)
                 (bound-and-true-p corfu--candidates))
            (corfu-previous)
          (completion-at-point))
      (error (completion-at-point))))

  (defun jds/agent-shell-evil-setup ()
    "Install ergonomic Evil bindings for `agent-shell-mode'."
    ;; Insert state should feel like composing in a shell/editor.
    (evil-local-set-key 'insert (kbd "RET") #'newline)
    (evil-local-set-key 'insert (kbd "C-<return>") #'comint-send-input)
    (evil-local-set-key 'insert (kbd "<tab>") #'completion-at-point)
    (evil-local-set-key 'insert (kbd "<backtab>") #'jds/agent-shell-backtab-dwim)
    (evil-local-set-key 'insert (kbd "C-<tab>") #'agent-shell-cycle-session-mode)
    (evil-local-set-key 'insert (kbd "M-p") #'agent-shell-previous-input)
    (evil-local-set-key 'insert (kbd "M-n") #'agent-shell-next-input)
    ;; Normal state should focus on sending and transcript navigation.
    (evil-local-set-key 'normal (kbd "RET") #'comint-send-input)
    (evil-local-set-key 'normal (kbd "C-<tab>") #'agent-shell-cycle-session-mode)
    (evil-local-set-key 'normal (kbd "<tab>") #'agent-shell-next-item)
    (evil-local-set-key 'normal (kbd "<backtab>") #'agent-shell-previous-item)
    (evil-local-set-key 'normal (kbd "g]") #'agent-shell-next-item)
    (evil-local-set-key 'normal (kbd "g[") #'agent-shell-previous-item)
    (evil-local-set-key 'normal (kbd "M-p") #'agent-shell-previous-input)
    (evil-local-set-key 'normal (kbd "M-n") #'agent-shell-next-input))

  (defun jds/agent-shell-remap-permission-keys (text)
    "Make `C-c C-c' allow and `C-c C-k' reject in permission prompt TEXT."
    (let ((pos 0))
      (while (< pos (length text))
        (let* ((next-pos (or (next-single-property-change pos 'keymap text)
                             (length text)))
               (keymap (get-text-property pos 'keymap text)))
          (when keymap
            (let ((accept-command (lookup-key keymap (kbd "y")))
                  (reject-command (lookup-key keymap (kbd "C-c C-c"))))
              (when reject-command
                (define-key keymap (kbd "C-c C-k") reject-command))
              (when accept-command
                (define-key keymap (kbd "C-c C-c") accept-command))))
          (setq pos next-pos))))
    text)


  (setq agent-shell-openai-authentication
	(agent-shell-openai-make-authentication :login t)
	agent-shell-anthropic-authentication
	(agent-shell-anthropic-make-authentication :login t)
	agent-shell-google-authentication
	(agent-shell-google-make-authentication :login t)
	agent-shell-preferred-agent-config 'codex
	;; Make active selections the first thing carried into prompts.
	agent-shell-context-sources '(region files error line)
	;; Prefer the compose/viewport flow over raw shell insertion.
	agent-shell-prefer-viewport-interaction nil
	;; Show context pressure in long-running coding sessions.
	agent-shell-show-context-usage-indicator 'detailed
	agent-shell-show-usage-at-turn-end t)

  (defun jds/agent-shell-dwim (&optional arg)
    "Start `agent-shell', prompting for provider with prefix ARG."
    (interactive "P")
    (let ((agent-shell-preferred-agent-config
           (if arg nil agent-shell-preferred-agent-config)))
      (call-interactively #'agent-shell)))

  (evil-set-initial-state 'agent-shell-viewport-view-mode 'normal)
  (evil-set-initial-state 'agent-shell-viewport-edit-mode 'insert)
  (evil-define-key 'normal agent-shell-viewport-view-mode-map
    (kbd "RET") #'agent-shell-viewport-reply
    (kbd "<tab>") #'agent-shell-viewport-next-item
    (kbd "<backtab>") #'agent-shell-viewport-previous-item
    (kbd "n") #'agent-shell-viewport-next-page
    (kbd "p") #'agent-shell-viewport-previous-page
    (kbd "r") #'agent-shell-viewport-reply
    (kbd "g]") #'agent-shell-viewport-next-item
    (kbd "g[") #'agent-shell-viewport-previous-item
    (kbd "o") #'agent-shell-other-buffer
    (kbd "?") #'agent-shell-viewport-help-menu
    (kbd "q") #'bury-buffer)
  (evil-define-key 'insert agent-shell-viewport-edit-mode-map
    (kbd "C-<return>") #'agent-shell-viewport-compose-send
    (kbd "<tab>") #'completion-at-point
    (kbd "<backtab>") #'jds/agent-shell-backtab-dwim
    (kbd "M-p") #'agent-shell-viewport-previous-history
    (kbd "M-n") #'agent-shell-viewport-next-history)
  (evil-define-key 'normal agent-shell-viewport-edit-mode-map
    (kbd "RET") #'agent-shell-viewport-compose-send
    (kbd "q") #'agent-shell-viewport-compose-cancel
    (kbd "o") #'agent-shell-other-buffer)
  (define-key agent-shell-diff-mode-map (kbd "C-c C-c") #'agent-shell-diff-accept-all)
  (define-key agent-shell-diff-mode-map (kbd "C-c C-k") #'agent-shell-diff-reject-all)
  (advice-add 'agent-shell--make-tool-call-permission-text :around
              (lambda (orig-fn &rest args)
                (jds/agent-shell-remap-permission-keys (apply orig-fn args))))

  (add-hook 'agent-shell-mode-hook #'jds/agent-shell-evil-setup)
  (add-hook 'agent-shell-diff-mode-hook #'evil-emacs-state))


;;; bindings -------------------------------------------------------------------


(jds/localleader-def
  :keymaps '(ess-r-mode-map emacs-lisp-mode-map vterm-mode-map)
  "'" #'claude-code-ide-menu)

;; not mature enough yet
;; (use-package codex-ide
;;   :straight (:type git :host github :repo "dgillis/codex-ide")
;;   :bind ("C-c C-;" . codex-ide-menu))


(jds/localleader-def
  :keymaps '(LaTeX-mode-map)
  "d"  '(:ignore t :which-key "ai")
  "dr" #'jds/gptel-rewrite		; rewrite selection
  "dc" #'jds/gptel-rewrite-with-context	; rewrite with full doc context
  "dd" #'jds/gptel-draft-at-point) ; draft new content at point

(load-config "ai-email.el")

(provide 'ai)
