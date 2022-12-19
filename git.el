;;; git.el --- magit config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/git
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  magit config

;;
;;; Code:

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (transient-append-suffix 'magit-merge "-A"
    '("-A" "Allow unrelated histories" "--allow-unrelated-histories"))
  (transient-append-suffix 'magit-pull "-A"
    '("-A" "Allow unrelated histories" "--allow-unrelated-histories"))

  ;; allow use of magit-list-repos for overviews
  ;; https://magit.vc/manual/magit/Repository-List.html (documentation)
  (setq magit-repository-directories '(("~/.homesick/repos/arch-dotfiles/" 0)
				       ("~/Dropbox/org/roam/references/" 0)
				       ("/home/jds6696/.local/share/ArchMatic/")
				       ("~/.emacs.d/" 0))
	magit-repolist-columns '(("Name" 25 magit-repolist-column-ident nil)
				 ;; ("Version" 25 magit-repolist-column-version
				 ;;  ((:sort magit-repolist-version<)))
				 ("Flag" 8 magit-repolist-column-flag)
				 ("B<U" 3 magit-repolist-column-unpulled-from-upstream
				  ((:right-align t)
				   (:sort <)))
				 ("B>U" 3 magit-repolist-column-unpushed-to-upstream
				  ((:right-align t)
				   (:sort <)))
				 ;; ("Path" 99 magit-repolist-column-path nil)
				 ))

  ;; https://github.com/magit/ghub/issues/81
  ;; (setq ghub-use-workaround-for-emacs-bug 'force)
  )

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

;; for easier handling of pull-requests on github
(use-package code-review)

(use-package browse-at-remote)

(general-define-key
 :keymaps 'code-review-mode-map
 :states 'n
 "?" #'code-review-transient-api)

(jds/localleader-def
  :keymaps 'forge-topic-mode-map
  "r" #'code-review-forge-pr-at-point)

;; TODO: reinstate
(use-package git-timemachine
  :straight (git-timemachine :type git :host github :repo "emacsmirror/git-timemachine")
)

(provide 'git)
;;; git.el ends here
