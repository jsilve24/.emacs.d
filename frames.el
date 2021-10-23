(use-package frames-only-mode
  :straight (frames-only-mode :type git :host github :repo "davidshepherd7/frames-only-mode" :branch "master")
  :ensure t
  :config
  (frames-only-mode 1))

(provide 'frames)
