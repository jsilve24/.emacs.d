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
  (claude-code-ide-emacs-tools-setup)
  (defun jds/claude-code-evil-setup ()
    "Send C-o and C-e through to vterm, bypassing evil bindings."
    (evil-local-set-key 'insert (kbd "C-o") #'vterm--self-insert)
    (evil-local-set-key 'insert (kbd "C-e") #'vterm--self-insert)
    (evil-local-set-key 'normal (kbd "C-o") #'vterm--self-insert)
    (evil-local-set-key 'normal (kbd "C-e") #'vterm--self-insert))
  (add-hook 'claude-code-mode-hook #'jds/claude-code-evil-setup)) ; Optionally enable Emacs MCP tools

(jds/localleader-def
 :keymaps '(ess-r-mode-map emacs-lisp-mode-map)
 "'" #'claude-code-ide-menu)

(provide 'ai)

