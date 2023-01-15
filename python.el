;;; python.el ---  python setup -*- lexical-binding: t; -*-


;; (use-package ein
;;   :config
;;   (require 'ein-notebook)
;;   (require 'ein-subpackages))

(use-package eval-in-repl
  :config
  (setq eir-repl-placement 'right)
  (require 'eval-in-repl-python))

(general-define-key
 :keymaps 'python-mode-map
 :states '(n m i)
 [C-return] #'eir-eval-in-python)


