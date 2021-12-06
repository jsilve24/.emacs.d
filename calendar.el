;;; calendar.el --- calendar config -*- lexical-binding: t; -*-

(use-package org-gcal
  :commands (org-gcal-sync org-gcal-fetch org-gcal-post-at-point org-gcal-request-token org-gcal-delete-at-point)
  :config
  (load-file "~/.org-caldav-secrets.el.gpg")
  (setq org-gcal-auto-archive nil))

(provide 'calendar)
;;; calendar.el ends here
