;;; system-jump.el --- jump around system quickly -*- lexical-binding: t; -*-

(use-package zoxide
  :commands (zoxide-add zoxide-find-file zoxide-cd zoxide-query zoxide-travel)
  ;; :straight (zoxide :type git :host gitlab :repo "Vonfry/zoxide.el" :branch "master")
  :hook
  ((find-file . zoxide-add)
   (projectile-after-switch-project . zoxide-add)
   (dired-after-readin . zoxide-add))
  :config
  ;; Needed to add the expand-file-name to below to get it working well 
(defun zoxide-add (&optional path &rest _)
  "Add PATH to zoxide database.  This function is called asynchronously."
  (interactive "Dpath: ")
  (unless path
    (setq path (expand-file-name default-directory)))
  (zoxide-run t "add" path)))


(use-package affe
  :commands (affe-find affe-grep)
  :config
  (setq affe-find-command "rg --color=never --files --hidden --ignore-file /home/jds6696/.emacs.d/.affe-ignore")
  ;; (setq affe-find-command (concat
  ;; 			   "find -type f "
  ;; 			   "\! \( -path */.git/* \) "
  ;; 			   "\! \( -path */.dropbox/* \) "
  ;; 			   "\! \( -path */.mail/* \) "
  ;; 			   "\! \( -path */python3\.9/* \) "
  ;; 			   "\! \( -name \".DS_Store\" \) "))
  )

;;;###autoload
(defun jds/affe-find-files-home ()
  "Affe find in home-directory but ignoring some file-types directories."
  (interactive)
    (affe-find "~"))


;;; some shortcuts and utilities taken from doom
;;;###autoload
(defun jds/project-browse (dir)
  "Traverse a file structure starting linearly from DIR."
  (unless (file-directory-p dir)
    (error "Directory %S does not exist" dir))
  (unless (file-readable-p dir)
    (error "Directory %S isn't readable" dir))
  ;; (let ((default-directory (file-truename (expand-file-name dir))))
  ;;   (call-interactively #'find-file)))
  (let ((default-directory dir))
    (call-interactively #'find-file)))


;;;###autoload
(defun jds/open-config ()
  "Open config for current running emacs instance."
  (interactive)
  (jds/project-browse user-emacs-directory))



;;;###autoload
(defun jds/find-file-config ()
  "Recursive search for file in personal config directory."
  (interactive)
  (let ((default-directory user-emacs-directory))
    ;; (project-find-file)
    (projectile-find-file)))


;; ;;;###autoload
(defun jds/find-file-other-project ()
  "Recursive search for file in a different project."
  (interactive)
  (let ((default-directory (completing-read "Project to search:"
                    (projectile-relevant-known-projects))))
    (projectile-find-file)))


(provide 'config-system-jump)
;;; system-jump.el ends here
