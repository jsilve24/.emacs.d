;;; secrets.el --- secret suff -*- lexical-binding: t -*-

(use-package epa
  :config
  (epa-file-enable)
  (setq epa-file-cache-passphrase-for-symmetric-encryption t
	epa-pinentry-mode 'loopback))

(provide 'config-secrets)
