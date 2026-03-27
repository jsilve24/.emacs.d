;;; ai.el --- summary -*- lexical-binding: t -*-

(use-package gptel)

(use-package gptel-quick
  :straight (gptel-quick :type git :host github :repo "karthink/gptel-quick")
  :config
  (keymap-set embark-general-map "?" #'gptel-quick))

(use-package claude-code
  :straight (claude-code :type git :host github :repo "stevemolitor/claude-code.el"
                         :files ("*.el" "grammars"))
  :config
  (defun jds/claude-code-evil-setup ()
    "Send C-o and C-e through to vterm, bypassing evil bindings."
    (evil-local-set-key 'insert (kbd "C-o") #'vterm--self-insert)
    (evil-local-set-key 'insert (kbd "C-e") #'vterm--self-insert)
    (evil-local-set-key 'normal (kbd "C-o") #'vterm--self-insert)
    (evil-local-set-key 'normal (kbd "C-e") #'vterm--self-insert))
  (add-hook 'claude-code-mode-hook #'jds/claude-code-evil-setup))

(provide 'ai)

