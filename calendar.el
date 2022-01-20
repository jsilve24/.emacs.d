;;; calendar.el --- calendar config -*- lexical-binding: t; -*-

(use-package org-gcal
  :commands (org-gcal-sync org-gcal-fetch org-gcal-post-at-point org-gcal-request-token org-gcal-delete-at-point)
  :config
  (load-file "~/.org-caldav-secrets.el.gpg")
  (setq org-gcal-auto-archive nil))

(add-hook 'after-init-hook #'org-gcal-fetch)

;;;###autoload
(defun jds~async-exchange-calendar-fetch ()
  (interactive)
  (jds/quiet-async-shell-commands "~/bin/get_exchange_cal.sh"))
(add-hook 'after-init-hook #'org-gcal-fetch)

(provide 'calendar)
;;; calendar.el ends here
