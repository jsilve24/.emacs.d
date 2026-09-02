(defconst jds/roar-tramp-shell-prompt-pattern
  "\\(?:^\\|\r\\)\\[[^]\n]+\\][^%\n]*%[[:blank:]]*"
  "Match the initial Zsh prompt used by Roar submit nodes.")

(with-eval-after-load 'tramp
  ;; Roar presents prompts such as `[jds6696@submit02]~%`, which the
  ;; Emacs 30.2 TRAMP default does not recognize during connection setup.
  (unless (string-match-p
           (regexp-quote jds/roar-tramp-shell-prompt-pattern)
           tramp-shell-prompt-pattern)
    (setq tramp-shell-prompt-pattern
          (concat tramp-shell-prompt-pattern
                  "\\|" jds/roar-tramp-shell-prompt-pattern))))

(defun jds/dired-to-roar-home ()
  "Open the home directory on Roar in Dired."
  (interactive)
  (let ((auth-sources nil))
    (dired "/ssh:jds6696@submit.hpc.psu.edu:~/")))

(defun jds/dired-to-roar-root ()
  "Open the filesystem root on Roar in Dired."
  (interactive)
  (let ((auth-sources nil))
    (dired "/ssh:jds6696@submit.hpc.psu.edu:/")))
