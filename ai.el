;;; ai.el --- summary -*- lexical-binding: t -*-

(use-package gptel)

(use-package gptel-quick
  :straight (gptel-quick :type git :host github :repo "karthink/gptel-quick")
  :config
  (keymap-set embark-general-map "?" #'gptel-quick))

;;; CLAUDE 
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(jds/localleader-def
 :keymaps '(ess-r-mode-map emacs-lisp-mode-map)
 "'" #'claude-code-ide-menu)

(provide 'ai)

