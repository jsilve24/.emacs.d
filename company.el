;;; company.el --- company setup -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/company
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  company setup
;;
;;; Code:

(use-package company
  :commands (company-complete-common
	     company-complete
	     company-complete-common-or-cycle
	     company-manual-begin
	     company-grab-line)
  :hook (after-init . global-company-mode)
  ;; :bind ("TAB" . company-indent-or-complete-common)
  ;; :bind ("C-SPC" . company-indent-or-complete-common)
  :init
  (setq company-minimum-prefix-length 2
	company-tooltip-limit 14
	company-tooltip-align-annotation t
	company-require-match 'never


	;; These auto-complete the current selection when
	;; `company-auto-commit-chars' is typed. This is too magical. We
	;; already have the much more explicit RET and TAB.
	company-auto-commit nil

	;; Only search the current buffer for `company-dabbrev' (a backend that
	;; suggests text your open buffers). This prevents Company from causing
	;; lag once you have a lot of buffers open.
	company-dabbrev-other-buffers nil
	;; Make `company-dabbrev' fully case-sensitive, to improve UX with
	;; domain-specific words with particular casing.
	company-dabbrev-ignore-case t
	company-dabbrev-downcase nil

	;; only give company when asked for
	company-idle-delay nil)
  :config

  ;; Buffer-local backends will be computed when loading a major mode, so
  ;; only specify a global default here. `'
  (setq company-backends '(company-capf company-files))
)

;;(with-eval-after-load 'company
    ;; make ESC abort from here: https://github.com/noctuid/general.el/issues/105
 ;; (evil-make-intercept-map company-active-map 'insert)
 ;; (general-def company-active-map [escape] 'company-abort))


(provide 'company)
;;; company.el ends here

