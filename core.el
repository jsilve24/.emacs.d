;;; core.el --- Core Config Stuff -*- lexical-binding: t; -*-
;; pretty much just straight, use-package, and general


;; Install Straight -- per straight documentation
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-host-usernames
      '((github . "jsilve24")))


;; Install use-package through straight 
;; Now use-package will use straight.el to automatically install
;; missing packages if you provide :straight t:
(straight-use-package 'use-package)

; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

;; default to install use-package recipes
;; (Note that the variable use-package-always-ensure is
;; associated with package.el, and you should not use it with straight.el.)
(setq straight-use-package-by-default t)

;;; Keybinding Utilities
(use-package general
  :ensure t
  :config
  ;; allow for shorter bindings -- e.g., just using things like nmap alone without general-* prefix
  (general-evil-setup t)

  ;; To automatically prevent Key sequence starts with a non-prefix key errors without the need to
  ;; explicitly unbind non-prefix keys, you can add (general-auto-unbind-keys) to your configuration
  ;; file. This will advise define-key to unbind any bound subsequence of the KEY. Currently, this
  ;; will only have an effect for general.el key definers. The advice can later be removed with
  ;; (general-auto-unbind-keys t).
  (general-auto-unbind-keys)


  (general-create-definer jds/leader-def
    :states '(normal visual motion emacs insert)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer jds/sub-leader-def
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix ","
    :global-prefix "C-,")
  (general-create-definer jds/localleader-def
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "m"
    ;; :global-prefix "C-m"
    )
  (general-create-definer jds/sub-localleader-def
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "\\"
    :global-prefix "C-\\"))


;; Can't get by without which-key
(straight-use-package 'which-key)
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

