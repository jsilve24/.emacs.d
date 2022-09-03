;;; avy.el --- jumping config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Justin Silverman
;;
;; Author: Justin Silverman <https://github.com/jsilve24>
;; Maintainer: Justin Silverman <jsilve24@gmail.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jsilve24/avy
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  jumping config
;;
;;; Code:


;;; avy
(use-package avy
  :straight t
  :after evil
  :config
  (avy-setup-default)
  (setq avy-keys (number-sequence ?a ?z)
	;; avy-style 'de-bruijn
	avy-style 'at-full
	avy-all-windows '()
	avy-case-fold-search t
	avy-highlight-first t
	avy-single-candidate-jump t
	avy-styles-alst '((avy-goto-line . pre))
	avy-orders-alist '((avy-goto-char . avy-order-closest)
			   (avy-goto-word-0 . avy-order-closest)
			   (avy-goto-line . avy-order-closest)))
  ;; embark act from here https://karthinks.com/software/avy-can-do-anything/
  (defun avy-action-embark (pt)
    (unwind-protect
	(save-excursion
	  (goto-char pt)
	  (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?\; avy-dispatch-alist) 'avy-action-embark))

;; ;;; link-hint
;; ;;;###autoload
;; (defun jds/link-hint-add-embark (link-type)
;;   (link-hint-define-type link-type
;;      :embark #'embark-act))

;; ;;;###autoload
;; (defun jds/link-hint-add-open-new-window (link-type)
;;   (interactive)
;;   (link-hint-define-type link-type
;;     :open-new-window
;;     (lambda (&rest _)
;;       (let* ((fn (plist-get
;; 		  (symbol-plist
;; 		   (intern (concat "link-hint-" (symbol-name link-type)))) :open)))
;; 	;; (select-window (split-window-sensibly-prefer-horizontal))
;; 	(`(apply ',fn ,@_))))))

(use-package link-hint
  :after avy
  :ensure t
  :config
  ;; Use qutebrowser to open urls
  (setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program "qutebrowser")
  ;; Open urls in a new tab instead of window; can also be set in the config file
  (setq browse-url-generic-args '("--target" "tab"))

  ;; to override avy settings
  ;; (setq link-hint-ayy-style 'at)

  ;; ;; add embark to link-hint
  ;; (mapc #'jds/link-hint-add-embark
  ;; 	(mapcar (lambda (x) (intern (replace-regexp-in-string "link-hint-" "" (symbol-name x)))) link-hint-types))
  ;; (mapc #'jds/link-hint-add-open-new-window
  ;; 	(mapcar (lambda (x) (intern (replace-regexp-in-string "link-hint-" "" (symbol-name x)))) link-hint-types))

  ;; (link-hint-define-type 'org-link
  ;;   :open-new-window (lambda ()
  ;; 		       (select-window (split-window-sensibly-prefer-horizontal))
  ;; 		       (link-hint--open-org-link)))

  ;; (defun link-hint-embark-link ()
  ;;   "Run embark-act on link"
  ;;   (interactive)
  ;;   (avy-with link-hint-embark-link
  ;;     (link-hint--one :embark)))

  ;; (defun link-hint-open-new-window ()
  ;;   "Run embark-act on link"
  ;;   (interactive)
  ;;   (avy-with link-hint-open-new-window
  ;;     (link-hint--one :open-new-window)))


  ;; (setq link-hint-avy-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?p ?q ?r ?s ?t ?u ?w ?x ?y ?z))
  )

;; (jds/link-hint-add-embark 'link-hint-mu4e-message)

;; setup ace-window integration for link-hint from here:
;;  https://localauthor.github.io/posts/aw-select.html
(with-eval-after-load 'link-hint
  (defun link-hint-aw-select ()
    "Use avy to open a link in a window selected with ace-window."
    (interactive)
    (unless
	(avy-with link-hint-aw-select
	  (link-hint--one :aw-select))
      (message "No visible links")))

  (defun lh-aw--split-right (window)
    "like aw-move-window-split-right but returns window rather than buffer and does not move current buffer."
    (let ((cur-window (selected-window)))
      (aw-switch-to-window window)
      (split-window-right)
      (call-interactively #'other-window)
      (let ((new-window (selected-window)))
	(select-window cur-window)
	new-window)))
  
  (defun lh-aw--split-below (window)
    "like aw-move-window-split-below but returns window rather than buffer and does not move current buffer."
    (let ((cur-window (selected-window)))
      
      (aw-switch-to-window window)
      (split-window-below)
      (call-interactively #'other-window)
      (let ((new-window (selected-window)))
	(select-window cur-window)
	new-window)))

  (defun lh-aw--split-fair (window)
    "like aw-move-window-split-fair but returns window rather than buffer and does not move current buffer."
    (let* ((cur-window (selected-window))
	   (w (window-body-width window))
	   (h (window-body-height window)))
      (aw-switch-to-window window)
      (if (< (* h aw-fair-aspect-ratio) w)
	  (aw-split-window-horz window)
	(aw-split-window-vert window))
      (call-interactively #'other-window)
      (let ((new-window (selected-window)))
	(select-window cur-window)
	new-window)))

  (defun lh-aw--select (window)
    "Just a placeholder, identify function"
    window)

  (defmacro define-link-hint-aw-select (link-type fn)
    `(progn
       (link-hint-define-type ',link-type
	 :aw-select #',(intern (concat "link-hint--aw-select-"
				       (symbol-name link-type))))
       (defun ,(intern (concat "link-hint--aw-select-"
			       (symbol-name link-type))) (_link)
	 (with-demoted-errors "%s"
	   (if (> (length (aw-window-list)) 1)
	       (let* ((buffer (current-buffer))
		      (aw-dispatch-alist
		       '((?v lh-aw--split-right "Right")
			 (?s lh-aw--split-below "Below")
			 (?p lh-aw--select "Place")
			 (?f lh-aw--split-fair "Fair")
			 (?? aw-show-dispatch-help "Display Help")))
		      (window (aw-select nil))
		      ;; (new-buffer)
		      )
		 (,fn)
		 (sit-for 0.1)		; for exwm buffers to load
		 (setq new-buffer (current-buffer))
		 (switch-to-buffer buffer)
		 (aw-switch-to-window window)
		 (switch-to-buffer new-buffer))
	     (link-hint-open-link-at-point))))))

  (define-link-hint-aw-select button push-button)
  (define-link-hint-aw-select mu4e-attachment link-hint--open-mu4e-attachment)
  (define-link-hint-aw-select mu4e-url link-hint--open-mu4e-url)
  (define-link-hint-aw-select dired-filename dired-find-file)
  (define-link-hint-aw-select file-link find-file)
  (define-link-hint-aw-select shr-url browse-url)
  (define-link-hint-aw-select markdown-link link-hint--open-markdown-link)
  (define-link-hint-aw-select org-agenda-item link-hint--open-org-agenda-item)
  (define-link-hint-aw-select text-url link-hint-open-link-at-point)


  (with-eval-after-load 'org
    ;; The same is almost the case with org-links as well, except that by default org-links are
    ;; opened using find-file-other-window instead of find-file, meaning that the above macro
    ;; wouldn’t work properly. I prefer to change this setting globally, so that org-links are
    ;; opened in the current window, by evaluating the following:
    (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
    (define-link-hint-aw-select org-link org-open-at-point)))

;;; evil-easymotion
(use-package evil-easymotion
  :straight t
  :after avy
  :config
  ;; Use evil-search backend, instead of isearch
  (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
		      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
		      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
		      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
		      :bind ((evil-ex-search-highlight-all nil)))

  ;; Rebind scope of w/W/e/E/ge/gE evil-easymotion motions to the visible
  ;; buffer, rather than just the current line.
  (put 'visible 'bounds-of-thing-at-point (lambda () (cons (window-start) (window-end))))
  (evilem-make-motion evilem-motion-forward-word-begin #'evil-forward-word-begin :scope 'visible)
  (evilem-make-motion evilem-motion-forward-WORD-begin #'evil-forward-WORD-begin :scope 'visible)
  (evilem-make-motion evilem-motion-forward-word-end #'evil-forward-word-end :scope 'visible)
  (evilem-make-motion evilem-motion-forward-WORD-end #'evil-forward-WORD-end :scope 'visible)
  (evilem-make-motion evilem-motion-backward-word-begin #'evil-backward-word-begin :scope 'visible)
  (evilem-make-motion evilem-motion-backward-WORD-begin #'evil-backward-WORD-begin :scope 'visible)
  (evilem-make-motion evilem-motion-backward-word-end #'evil-backward-word-end :scope 'visible)
  (evilem-make-motion evilem-motion-backward-WORD-end #'evil-backward-WORD-end :scope 'visible))

(provide 'avy)
;;; avy.el ends here
