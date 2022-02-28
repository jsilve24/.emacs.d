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

;; (use-package ranger
;;   :straight t
;;   ;; :commands (deer ranger jds/deer-downloads)
;;   :ensure t
;;   ;; :after dired
;;   :config
;;   (ranger-override-dired-mode t)

;;   (setq ranger-parent-depth 1
;;         ranger-width-parents 0.2
;;         ranger-width-preview 0.4)

;;   (link-hint-define-type 'dired-files-and-directories
;;     :next #'ranger-next-file
;;     :at-point-p #'dired-file-name-at-point
;;     :open #'ranger-find-file
;;     :goto #'dired-goto-file-1
;;     :vars '(ranger-mode))
;;   (push 'link-hint-dired-files-and-directories link-hint-types))

;; (general-define-key
;;  :keymaps 'ranger-mode-map
;;  "f"  nil
;;  "F" nil
;;  "f"         #'jds/link-hint-goto-link
;;  "F"         #'link-hint-open-link
;;  "<backtab>" #'dired-unmark)

;; ;; local bindings
;; (jds/localleader-def ranger-mode-map
;;                      ;; "e" #'wdired-change-to-wdired-mode
;;                      "c" #'dired-rsync
;;                      "+" #'jds/make-dated-directory
;;                      "d" #'jds/dragon-dired)

;;; setup default dired -- cheatsheet here:
;; https://github.com/daviwil/emacs-from-scratch/blob/8c302a79bf5700f6ef0279a3daeeb4123ae8bd59/Emacs.org#dired
;; more here: https://github.com/Fuco1/dired-hacks/tree/7c0ef09d57a80068a11edc74c3568e5ead5cc15a#dired-open
(use-package dired
  :straight (:type built-in)
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  ;; tell emacs to revert each Dired buffer automatically when revisiting buffer
  (setq dired-auto-revert-buffer t)
  ;; Auto-refresh dired on filesystem change
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

;;; dired-registers
(with-eval-after-load 'dired

  (defvar dired-registers--register-alist
    (mapcar (lambda (c) (cons c nil)) (number-sequence ?a ?z))
    "Alist of symbols and paths, the datastructure of the registers.")

  (defun dired-registers-store (reg &optional path)
    "Store path to register REG (passed as a character). If PATH is not provided, then use default-directory."
    (interactive "c")
    (let ((path
	   (if path path default-directory)))
      (setf (cdr (assoc reg dired-registers--register-alist)) path)))

  (defun dired-registers-goto (reg)
    "Dired to register stored in REG (passed as character)."
    (interactive "c")
    (let ((path (cdr (assoc reg dired-registers--register-alist))))
      (if (file-exists-p path)
	  (dired-single-buffer path)
	(message (format
		  "No valid path stored in regiseter %c"
		  reg)))))
  )




;; stole the below function from dired-hacks-utils.el since it was difficult
;; to just install that one file with straight. 
;;;###autoload
(defun dired-hacks-next-file (&optional arg)
  "Move point to the next file.
Optional prefix ARG says how many lines to move; default is one
line."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (dired-hacks-previous-file (- arg))
    (--dotimes arg
      (forward-line)
      (while (and (or (not (dired-utils-is-file-p))
                      (get-text-property (point) 'invisible))
                  (= (forward-line) 0))))
    (if (not (= (point) (point-max)))
        (dired-move-to-filename)
      (forward-line -1)
      (dired-move-to-filename)
      nil)))


;;;###autoload
(defun jds~avy-dired-cands ()
  (save-excursion
    (save-restriction
      (narrow-to-region (window-start) (window-end (selected-window) t))
      (goto-char (point-min))
      (setq pt (point))
      (dired-hacks-next-file)
      (let ((candidates (list  (cons  (point) (selected-window)))))
	(setq pt (point))
	(dired-hacks-next-file)
	(while (not (equal (point) pt))
	  (setq pt (point))
	  (push (cons  (point) (selected-window)) candidates)
	  (dired-hacks-next-file))
	(nreverse  candidates)))))

;;;###autoload
(defun jds/avy-dired ()
    "Goto a visible file or directory in a dired-buffer."
  (interactive)
  (avy-action-goto (avy-with jds/avy-dired
		     (avy-process (jds~avy-dired-cands)))))

;; don't use multiple buffers
(use-package dired-single)

(use-package dired-open
  :config
  (setq dired-open-extensions '(("docx" . "libreoffice")
				("doc" . "libreoffice")
				("xlsx" . "libreoffice")
				("xls" . "libreoffice"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package dired-copy-paste
  :straight (dired-copy-paste :type git :host github :repo "jsilve24/dired-copy-paste"))

;;;###autoload
(defun jds/dired-copy-dirname-as-kill ()
  "Yank current directory path."
  (interactive)
  (kill-new dired-directory))

;; setup wv, ws, we bindings
(with-eval-after-load 'dired 
  (defun jds~dired-setup-function ()
    "Function to setup buffer local variables. Added to `dired-mode-hook`."
    (general-define-key
     :states 'normal
     :keymaps 'local
     "f" #'jds/avy-dired
     "F" #'link-hint-open-link
     "w"  #'(:ignore t)
     "wo" #'dired-view-file
     ";"  #'(:ignore t)
     ";d" #'dired-copy-paste-do-cut
     "y" '(:ignore t :wk "yank")
     "yd" #'jds/dired-copy-dirname-as-kill
     ;; copy filename 
     "yf" #'dired-copy-filename-as-kill
     ;; copy absolute filepath
     "yy" #'(lambda () (interactive) (dired-copy-filename-as-kill 0))
     ";y" #'dired-copy-paste-do-copy
     ";p" #'dired-copy-paste-do-paste
     ";m" #'dired-registers-store
     ";'" #'dired-registers-goto))
  (add-hook 'dired-mode-hook 'jds~dired-setup-function))

;; local bindings
(jds/localleader-def dired-mode-map
                     ;; "e" #'wdired-change-to-wdired-mode
                     ;; "c" #'dired-rsync
                     "S" #'jds/dired-screenshot
                     "+" #'jds/make-dated-directory
                     "d" #'jds/dragon-dired)
		     


;; (use-package dired+)

;;; dired-sidebar
;; (use-package dired-sidebar
;;   :ensure t
;;   :commands (dired-sidebar-toggle-sidebar))


;;; fluff
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))


;;; mode specific bindings
(general-define-key
 :keymaps 'dired-mode-map
 :states 'n
 "zh" #'dired-hide-dotfiles-mode)

(provide 'dired)
;;; dired.el ends here
