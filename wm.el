;;; wm.el --- EXWM config -*- lexical-binding: t; -*-

;;; exwm main setup
(use-package exwm
  :config
  ;; set the default number of workspaces
  (setq exwm-workspace-number 6)

  ;; (require 'exwm-randr)
  ;; (exwm-randr-enable)

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 15)
  (exwm-systemtray-enable)

  ;; make window divisions clear
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-right-width 1)
  (window-divider-mode 1)

  (display-time-mode 1)
  (display-battery-mode 1)
  
  
  
  ;; Automatically send the mouse cursor to the selected workspace's display
  (setq exwm-workspace-warp-cursor t)


  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; these keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j  ;; Buffer list
      ?\M-\ ))  ;; Ctrl+Space
  
   ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)
	  ([?\s-f] . exwm-layout-toggle-fullscreen)
	  ([?\s-z] . exwm-input-toggle-keyboard)


          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ([?\s-h] . windmove-left)
          ([?\s-l] . windmove-right)
          ([?\s-k] . windmove-up)
          ([?\s-j] . windmove-down)
	  
          ([?\s-H] . +evil/window-move-left)
          ([?\s-L] . +evil/window-move-right)
          ([?\s-K] . +evil/window-move-up)
          ([?\s-J] . +evil/window-move-down)


          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))

	  ;; 's-shift-N': move window to certain workspace
	  ;; ,@(mapcar (lambda (i)
	  ;; 	      `(,(kbd (format "s-%d" i)) .
	  ;; 		(lambda ()
	  ;; 		  (interactive)
	  ;; 		  (exwm-workspace-move-window ,i))))
	  ;; 	    '(?\) ?! ?@ ?# ?$ ?% ?^ ?& ?* ?\) ))


	  ))

  

  (exwm-enable)
  ;; (require 'exwm-config)
  ;; (exwm-config-default)
  )

(use-package app-launcher
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher")
  :config
  (exwm-input-set-key (kbd "s-;") #'app-launcher-run-app))



;;; fixing issues (e.g., ediff)

;; ;; Don't let ediff break EXWM, keep it in one frame
;; (setq ediff-diff-options "-w"
;;       ediff-split-window-function 'split-window-horizontally
;;       ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'wm)
