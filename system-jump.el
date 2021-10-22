;;; system-jump.el --- jump around system quickly -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/system-jump
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  jump around system quickly
;;
;;; Code:



(use-package zoxide
  :commands (zoxide-add zoxide-find-file zoxide-cd zoxide-query)
  :straight (zoxide :type git :host gitlab :repo "Vonfry/zoxide.el" :branch "master")
  :hook
  ((find-file-hook . zoxide-add)
   (projectile-after-switch-project-hook . zoxide-add)))


(straight-use-package 'affe)
(use-package affe
  :commands (affe-find))
;; (map! :after consult
;;       :leader
;;       :desc "affe find" "fa" (lambda () (interactive) (affe-find "/home/jds6696/")))

(provide 'system-jump)
;;; system-jump.el ends here
