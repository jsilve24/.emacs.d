;;; ai.el --- summary -*- lexical-binding: t -*-

(use-package gptel)

(use-package gptel-quick
  :straight (gptel-quick :type git :host github :repo "karthink/gptel-quick")
  :config
  (keymap-set embark-general-map "?" #'gptel-quick))

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
      (let ((result (ess-string-command cmd)))
        result)))

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

(jds/localleader-def
 :keymaps '(ess-r-mode-map emacs-lisp-mode-map vterm-mode-map)
 "'" #'claude-code-ide-menu)


(provide 'ai)

