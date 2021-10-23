;;; dired.el --- dired setup -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/ranger
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  dired setup
;;
;;; Code:

(use-package ranger
  :straight t
  ;; :commands (deer ranger jds/deer-downloads)
  :ensure t
  ;; :after dired
  :config
  (ranger-override-dired-mode t)

  (setq ranger-parent-depth 1
        ranger-width-parents 0.2
        ranger-width-preview 0.4)

  (link-hint-define-type 'dired-files-and-directories
    :next #'ranger-next-file
    :at-point-p #'dired-file-name-at-point
    :open #'ranger-find-file
    :goto #'dired-goto-file-1
    :vars '(ranger-mode))
  (push 'link-hint-dired-files-and-directories link-hint-types))

(general-define-key
 :keymaps 'ranger-mode-map
 "f"  nil
 "F" nil
 "f"         #'jds/link-hint-goto-link
 "F"         #'link-hint-open-link
 "<backtab>" #'dired-unmark)

;; local bindings
(jds/localleader-def ranger-mode-map
                     ;; "e" #'wdired-change-to-wdired-mode
                     "c" #'dired-rsync
                     "+" #'jds/make-dated-directory
                     "a" #'jds/attach-files
                     "d" #'jds/dragon-dired)


;;; fluff
(use-package all-the-icons-dired
  :straight t
  :after ranger
  :config
  (add-hook 'ranger-mode-hook 'all-the-icons-dired-mode))

(provide 'dired)
;;; dired.el ends here
