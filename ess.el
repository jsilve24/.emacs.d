;;; ess.el ---  R setup -*- lexical-binding: t; -*-

;; setup projectile
(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files "DESCRIPTION"))

;;; main ess setup
(use-package ess
  :mode (("\\.[rR]\\'" . ess-r-mode))
  :commands R ess-r-mode
  :init (require 'ess-site)
  :config
  (setq ess-offset-continued 'straight
	;;ess-use-flymake (not (featurep! :checkers syntax))
	ess-nuke-trailing-whitespace-p t
	ess-style 'DEFAULT
	;;ess-history-directory (expand-file-name "ess-history/" doom-cache-dir)
	ess-indent-with-fancy-comments t
	ess-eval-visibly 'nowait)

  ;; most people say this is annoying -- I need to figure out what it is eventually
  (setq ess-smart-S-assign-key nil)


  (add-hook 'ess-r-mode-hook #'display-fill-column-indicator-mode)

  ;; from here: https://github.com/SteveLane/dot-emacs/blob/master/ess-config.el
  ;; Add in company-mode helpers
  (defun my-ess-company-hook ()
    ;; ensure company-R-library is in ESS backends
    (make-variable-buffer-local 'company-backends)
    (cl-delete-if (lambda (x) (and (eq (car-safe x) 'company-R-args))) company-backends)
    (add-to-list 'company-backends
		 '(company-R-args company-R-objects company-R-library
				  company-dabbrev-code :separate)))
  (add-hook 'ess-mode-hook #'my-ess-company-hook)


  ;; this also makes heading section headers available in consult-outline
  (add-hook 'ess-mode-hook
	    '(lambda ()
	       (outline-minor-mode)
	       (setq outline-regexp "^#.*----"))))



;;; setup polymode

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "markdown"))

(use-package polymode
  :ensure markdown-mode
  :ensure poly-R
  :ensure poly-noweb
  :config
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode)))


(use-package poly-markdown
  :ensure polymode
  :defer t)


;; poly-R
(use-package poly-R
  :ensure polymode
  :ensure poly-markdown
  :ensure poly-noweb
  :defer t)


;; Add yaml to markdown an .yml files
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)))


;;; key bindings

(general-define-key
 :keymaps 'inferior-ess-mode-map
 "C-l" #'comint-clear-buffer)

(general-define-key
 :keymaps '(ess-mode-map inferior-ess-mode-map)
 "C-<" #'r/insert-assign
 "C->" #'r/insert-pipe)

;; ;;;###autoload
;; (defun jds~setup-ess-bindings ()
;;   (evil-local-set-key '(override normal) ",k" #'ess-display-help-on-object))
;; (add-hook 'ess-mode-hook 'jds~setup-ess-bindings)
;; (add-hook 'inferior-ess-mode-hook 'jds~setup-ess-bindings)


(jds/localleader-def
  :keymaps '(ess-mode-map inferior-ess-mode-map)
  "<tab>" '(ess-switch-to-inferior-or-script-buffer :wk "REPL-script switch")
  "vs" #'r/df-sample-small
  "vm" #'r/df-sample-medium
  "vl" #'r/df-sample-large
  ;; predefined keymaps
  "h" 'ess-doc-map
  "x" 'ess-extra-map
  "p" 'ess-r-package-dev-map
  "q" 'ess-dev-map)

(general-define-key
 :keymaps '(polymode-mode-map markdown-mode-map)
 "C-S-i"  #'r/rmd-insert-chunk)

(jds/localleader-def
 :keymaps '(polymode-mode-map markdown-mode-map)
  "rc" #'r/rmd-insert-chunk
  "rr" #'r/rmd-render
  "\\" #'r/rmd-render
  "rd" #'r/draft-rmd)

(general-define-key
 :keymaps 'ess-mode-map
 :states 'i
 [C-return] #'ess-eval-line-and-step)


(general-define-key
 :keymaps 'ess-mode-map
 :states '(n m)
 [C-return] #'ess-eval-function-or-paragraph-and-step)

(general-define-key
 :keymaps 'ess-mode-map
 :states 'v
 [C-return] #'ess-eval-region)


  ;; (map! (:after ess-help
  ;;         (:map ess-help-mode-map
  ;;           :n "q"  #'kill-current-buffer
  ;;           :n "Q"  #'ess-kill-buffer-and-go
  ;;           :n "K"  #'ess-display-help-on-object
  ;;           :n "go" #'ess-display-help-in-browser
  ;;           :n "gO" #'ess-display-help-apropos
  ;;           :n "gv" #'ess-display-vignettes
  ;;           :m "]]" #'ess-skip-to-next-section
  ;;           :m "[[" #'ess-skip-to-previous-section)
  ;;         (:map ess-doc-map
  ;;           "h"    #'ess-display-help-on-object
  ;;           "p"    #'ess-R-dv-pprint
  ;;           "t"    #'ess-R-dv-ctable
  ;;           [up]   #'comint-next-input
  ;;           [down] #'comint-previous-input
  ;;           :i   [C-return] #'ess-eval-line-and-step
  ;;           :nom [C-return] #'ess-eval-function-or-paragraph-and-step
  ;;           :v    [C-return] #'ess-eval-region))
  ;;       (:after ess
  ;;        :map ess-mode-map
  ;;        :localleader
  ;;        "'" #'R
  ;;        [backtab] #'ess-switch-process
  ;;        ;; noweb
  ;;        :prefix "c"
  ;;        "C" #'ess-eval-chunk-and-go
  ;;        "c" #'ess-eval-chunk
  ;;        "d" #'ess-eval-chunk-and-step
  ;;        "m" #'ess-noweb-mark-chunk
  ;;        "p" #'ess-noweb-previous-chunk
  ;;        "n" #'ess-noweb-next-chunk)))

(provide  'ess)

