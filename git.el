;;; git.el --- magit config -*- lexical-binding: t; -*-

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (transient-append-suffix 'magit-merge "-w" '("-a" "Allow unrelated histories" "--allow-unrelated-histories"))

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
				   (:sort <))))))

(use-package browse-at-remote)

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

;; for easier handling of pull-requests on github
(use-package code-review
  :disabled
  :config
  (general-define-key
   :keymaps 'code-review-mode-map
   :states 'n
   "?" #'code-review-transient-api)
  (jds/localleader-def
  :keymaps 'forge-topic-mode-map
  "r" #'code-review-forge-pr-at-point))


(use-package git-timemachine
  :straight (git-timemachine :type git :host github :repo "emacsmirror/git-timemachine"))

(provide 'config-git)
