;;; ai.el --- summary -*- lexical-binding: t -*-

(use-package gptel
  :init
  ;; (setq gptel-model 'claude-sonnet-4-6)
  (setq gptel-model 'claude-haiku-4-5-20251001)
  :config
  ;; --- Claude backend ---
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key (auth-source-pick-first-password :host "api.anthropic.com" :user "apikey")))
  
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

  ;; --- Shared callback factory ---
  (defun jds/gptel--replace-region-callback (start end)
    "Return a gptel callback that replaces START..END with the response."
    (let ((buf (current-buffer)))
      (lambda (response info)
        (if (not response)
            (message "gptel error: %s" (plist-get info :status))
          (with-current-buffer buf
            (delete-region start end)
            (goto-char start)
            (insert response)
            (set-marker start nil)
            (set-marker end nil))))))

  ;; --- Rewrite selection (no document context) ---
  (defun jds/gptel-rewrite (directive)
    "Rewrite the selected region according to DIRECTIVE."
    (interactive "sDirective: ")
    (unless (use-region-p) (user-error "Select a region first"))
    (let* ((start  (copy-marker (region-beginning)))
           (end    (copy-marker (region-end) t))
           (text   (buffer-substring-no-properties start end))
           (prompt (format "Directive: %s\n\nText to rewrite:\n\n%s" directive text)))
      (gptel-request prompt
        :system jds/gptel-latex-system
        :buffer (current-buffer)
        :callback (jds/gptel--replace-region-callback start end))))

  ;; --- Rewrite with full document as context ---
  (defun jds/gptel-rewrite-with-context (directive)
    "Rewrite the selected region using DIRECTIVE, with the full buffer as context."
    (interactive "sDirective: ")
    (unless (use-region-p) (user-error "Select a region first"))
    (let* ((start  (copy-marker (region-beginning)))
           (end    (copy-marker (region-end) t))
           (sel    (buffer-substring-no-properties start end))
           (doc    (buffer-substring-no-properties (point-min) (point-max)))
           (prompt (format "Full document for context:\n\n%s\n\n---\n\nDirective: %s\n\nRewrite this passage from the document above:\n\n%s"
                           doc directive sel)))
      (gptel-request prompt
        :system jds/gptel-latex-system
        :buffer (current-buffer)
        :callback (jds/gptel--replace-region-callback start end))))

  ;; --- Draft new content at point, doc as context ---
  (defun jds/gptel-draft-at-point (prompt)
    "Draft new LaTeX content at point based on PROMPT, using the buffer as context."
    (interactive "sDraft: ")
    (let* ((doc      (buffer-substring-no-properties (point-min) (point-max)))
           (pos      (copy-marker (point)))
           (buf      (current-buffer))
           (full-prompt (format "Document context:\n\n%s\n\n---\n\nDraft the following in LaTeX, fitting the style and flow of the document above. Return only the LaTeX text:\n\n%s"
                                doc prompt)))
      (gptel-request full-prompt
        :system jds/gptel-latex-system
        :buffer buf
        :callback (lambda (response info)
                    (if (not response)
                        (message "gptel error: %s" (plist-get info :status))
                      (with-current-buffer buf
                        (goto-char pos)
                        (insert response)
                        (set-marker pos nil))))))))

;;; gptel-reinforce ------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "gptel-reinforce" user-emacs-directory))
(require 'gptel-reinforce)
(require 'gptel-reinforce-elfeed)


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
