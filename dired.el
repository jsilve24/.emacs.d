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

(use-package dired-registers
  :straight (dired-registers :type git :host github :repo "jsilve24/dired-registers")
  :config
  (dired-registers-store ?d "~/Downloads/")
  (dired-registers-store ?h "~/")
  (dired-registers-store ?c "~/.emacs.d/")
  (dired-registers-store ?g "~/Dropbox/Faculty/Grants/")
  (dired-registers-store ?t "~/Dropbox/Faculty/Teaching/")
  (dired-registers-store ?p "~/Dropbox/Faculty/Presentations/")
  (dired-registers-store ?o "~/Dropbox/org/"))



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
     ";j" #'dired-registers-goto))
  (add-hook 'dired-mode-hook 'jds~dired-setup-function))

;; local bindings
(jds/localleader-def dired-mode-map
                     ;; "e" #'wdired-change-to-wdired-mode
                     ;; "c" #'dired-rsync
                     "S" #'jds/dired-screenshot
                     "+" #'jds/make-dated-directory
                     "d" #'jds/dragon-dired)
		     


;;; fluff
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))


;;; mode specific bindings
(general-define-key
 :keymaps 'dired-mode-map
 :states 'n
 "zh" #'dired-hide-dotfiles-mode)

