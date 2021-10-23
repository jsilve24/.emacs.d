;;; spelling.el --- setup spelling config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/spelling
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  setup spelling config
;;
;;; Code:



(use-package ispell
  ;; :straight t
  :init
  (executable-find "aspell")
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"
                            "--encoding=utf-8"
                            "--lang=en_US"))
  :config
  (setq ispell-personal-dictionary "~/.aspell.en.pws"))

;; (use-package spell-fu
;;   :straight (spell-fu  :type git :host github  :repo "emacsmirror/spell-fu" :branch "master")
;;   :when (executable-find "aspell")
;;   :hook (text-mode . spell-fu-mode)
;;   :config
;;   (setq ispell-personal-dictionary "~/.aspell.en.pws"))

(use-package flyspell
  :when (executable-find "aspell")
  :hook (text-mode . flyspell-mode)
  :bind
  (("M-z" . #'flyspell-auto-correct-previous-word)))


;;; recomplete


(provide 'spelling)
;;; spelling.el ends here
