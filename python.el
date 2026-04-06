;;; python.el ---  python setup -*- lexical-binding: t; -*-


;; (use-package ein
;;   :config
;;   (require 'ein-notebook)
;;   (require 'ein-subpackages))

(use-package eval-in-repl
  :config
  (setq eir-repl-placement 'right)
  (require 'eval-in-repl-python))

;; Both keymaps needed since treesit-auto may remap python-mode to python-ts-mode.
;; Both maps are defined in Emacs's built-in python.el, so with-eval-after-load
;; ensures they exist at binding time.
(with-eval-after-load 'python
  (general-define-key
   :keymaps '(python-mode-map python-ts-mode-map)
   :states '(n m i)
   [C-return] #'eir-eval-in-python))


