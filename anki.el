;;; anki.el --- JDS anki config -*- lexical-binding: t -*-

(use-package anki-editor
  :straight (:repo "anki-editor/anki-editor")
  :defer t
  ;; :after org
  :commands (anki-editor-mode
	     anki-editor-push-notes
	     anki-editor-push-tree
	     anki-editor-cloze-region-dont-incr
	     anki-editor-cloze-region-auto-incr
	     anki-editor-reset-cloze-number)
  :hook (org-mode . jds/anki-editor-maybe-enable)
  :init
  ;; Change this if AnkiConnect is listening elsewhere.
  (setq anki-editor-server-host "127.0.0.1"
	anki-editor-server-port 8765)
  ;; Store media generated from org exports here before pushing.
  ;; Optional, but nice to keep explicit.
  (setq anki-editor-create-decks t)
  ;; LaTeX fragments are common for your use case.
  ;; This makes it easy to preview math before pushing.
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq anki-editor-latex-style 'mathjax)
  ;; (setq anki-editor-latex-style 'builtin)


  ;; A small convenience: default tags used by card files.
  (defun jds/anki-editor-maybe-enable ()
    (when (and buffer-file-name
	       (string-match-p "/org/anki/" buffer-file-name))
      (anki-editor-mode 1))))


(jds/localleader-def
  :keymaps 'anki-editor-mode-map
  "s" '(:ignore t :which-key "anki")
  "sp" #'anki-editor-push-notes
  "sc" #'anki-editor-cloze-dwim
  "sd" #'anki-editor-delete-notes
  "si" #'anki-editor-insert-note)
