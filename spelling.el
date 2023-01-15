;;; spelling.el --- setup spelling config -*- lexical-binding: t; -*-

(use-package ispell
  :init
  (executable-find "aspell")
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"
                            "--encoding=utf-8"
                            "--lang=en_US"))
  :config
  (setq ispell-personal-dictionary "~/.aspell.en.pws"
	ispell-alternate-dictionary "/usr/share/dict/words"))


(use-package flyspell
  :after evil-easymotion
  :when (executable-find "aspell")
  :hook (text-mode . flyspell-mode)
  :hook (conf-toml-mode . flyspell-mode)
  :hook (slack-message-buffer-mode . flyspell-mode)
  :config
  ;; performance suggestion from wiki
  (setq flyspell-issue-message-flag nil))


(use-package flyspell-correct
  :straight t
  :after flyspell)

;;;###autoload
(defun jds/avy-fix-spelling ()
  "Jump to any Error and open spell checker."
  (save-excursion
    (goto-char (window-start))
    (evil-next-flyspell-error)
    (flyspell-correct-wrapper)))

(with-eval-after-load 'flyspell
  (evilem-make-motion evilem-motion-backward-spell-error #'evil-prev-flyspell-error)
  (evilem-make-motion evilem-motion-forward-spell-error #'evil-next-flyspell-error)


  (defun jds/avy-fix-spelling ()
    "Avy Hinting to Fix Spelling Forward."
    (interactive)
    (save-excursion
      (goto-char (window-start))
      (evilem-motion-forward-spell-error)
      (flyspell-correct-wrapper)))


  (defun jds/evilem-forward-fix-spelling ()
    "Avy Hinting to Fix Spelling Forward."
    (interactive)
    (save-excursion
      (evilem-motion-forward-spell-error)
      (flyspell-correct-wrapper)))

  (defun jds/evilem-backward-fix-spelling ()
    "Avy Hinting to Fix Spelling Forward."
    (interactive)
    (save-excursion
      (evilem-motion-backward-spell-error)
      (flyspell-correct-wrapper))))

;; from here: https://stackoverflow.com/questions/22107182/in-emacs-flyspell-mode-how-to-add-new-word-to-dictionary
;;;###autoload
(defun jds/save-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))


;; from here: 
;; https://emacs.stackexchange.com/questions/2793/is-it-possible-to-auto-correct-spelling-on-space
(defvar jds/abbrev-file "~/.emacs.d/etc/abbrev.el") ;; where to save abbreviations (this is default)

;;;###autoload
(defun jds/flyspell-correct-word-then-abbrev (p)
  "Call `flyspell-correct-wrapper'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
    (call-interactively 'flyspell-correct-wrapper)
    (setq aft (downcase (or (thing-at-point 'word) "")))
    (unless (string= aft bef)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft)
      (write-abbrev-file jds/abbrev-file))))

(setq save-abbrevs t) ;; not strictly needed or used since I updated above function to auto-save 
(setq-default abbrev-mode t)


;;; dictionary and thesaurus
;; need to have wordnet-cli and wordnet-common installed
(use-package synosaurus
  :hook (text-mode . synosaurus-mode)
  :diminish synosaurus-mode
  :disabled t
  :config
  (setq  synosaurus-choose-method 'default))


(use-package powerthesaurus)


(use-package dictionary
  :straight t
  :commands (dictionary-search)
  :init
  (global-set-key (kbd "C-c d") #'dictionary-search)
  :config
  (setq dictionary-server "dict.org"))



(provide 'config-spelling)
