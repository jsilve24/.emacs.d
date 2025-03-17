;;; ai.el --- summary -*- lexical-binding: t -*-

(use-package gptel)

(use-package gptel-quick
  :straight (gptel-quick :type git :host github :repo "karthink/gptel-quick")
  :config
  (keymap-set embark-general-map "?" #'gptel-quick))

(provide 'ai)

