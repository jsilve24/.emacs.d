;;; stan.el ---  R setup -*- lexical-binding: t; -*-

;;; stan-mode.el
(use-package stan-mode
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  ;;
  :config
  ;; The officially recommended offset is 2.
  (setq stan-indentation-offset 2))

;;; company-stan.el
(use-package company-stan
  :hook (stan-mode . company-stan-setup)
  ;;
  :config
  ;; Whether to use fuzzy matching in `company-stan'
  (setq company-stan-fuzzy nil)
  (defun jds~setup-capf-stan ()
    (make-local-variable 'completion-at-point-function)
    (add-to-list 'completion-at-point-functions
		 (cape-company-to-capf #'company-stan)))
  (add-hook 'stan-mode-hook  'jds~setup-capf-stan))

;;; eldoc-stan.el
(use-package eldoc-stan
  :hook (stan-mode . eldoc-stan-setup)
  ;;
  :config
  ;; No configuration options as of now.
  )

;; https://git.kyleam.com/ob-stan/about/
;; seems pretty outdated
(use-package ob-stan
  :straight (ob-stan :local-repo "~/.emacs.d/local-packages/ob-stan/"))

;;; flycheck-stan.el
;; (use-package flycheck-stan
;;   ;; Add a hook to setup `flycheck-stan' upon `stan-mode' entry
;;   :hook ((stan-mode . flycheck-stan-stanc2-setup)
;;          (stan-mode . flycheck-stan-stanc3-setup))
;;   :config
;;   ;; A string containing the name or the path of the stanc2 executable
;;   ;; If nil, defaults to `stanc2'
;;   (setq flycheck-stanc-executable nil)
;;   ;; A string containing the name or the path of the stanc2 executable
;;   ;; If nil, defaults to `stanc3'
;;   (setq flycheck-stanc3-executable nil))

;;; stan-snippets.el
(use-package stan-snippets
  :hook (stan-mode . stan-snippets-initialize)
  ;;
  :config
  ;; No configuration options as of now.
  )

;; ;;; ac-stan.el (Not on MELPA; Need manual installation)
;; (use-package ac-stan
;;   :load-path "path-to-your-directory/ac-stan/"
;;   ;; Delete the line below if using.
;;   :disabled t
;;   :hook (stan-mode . stan-ac-mode-setup)
;;   ;;
;;   :config
;;   ;; No configuration options as of now.
;;   )

;;; bindings -------------------------------------------------------------------

(jds/localleader-def
  :keymaps 'stan-mode-map
  "'" #'org-edit-src-exit
  "k" #'org-edit-src-abort)
