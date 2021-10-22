;;; themes.el --- theme related config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jds6696@psu.edu>
;; Created: October 21, 2021
;; Modified: October 21, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/themes
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Theme related config
;;
;;; Code:

;;; Fonts

;;; Font stuff

(setq jds/default-font-size 100)
(setq jds/default-variable-font-size 100)

(set-face-attribute 'default nil :font "Fira Code Retina"
                    :height jds/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina"
                    :height jds/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell"
                    :height jds/default-variable-font-size :weight 'regular)


;;; main themes

(straight-use-package 'doom-themes)

(use-package doom-themes
  :init (load-theme 'doom-vibrant t))

;;; modline



(provide 'themes)
;;; themes.el ends here
