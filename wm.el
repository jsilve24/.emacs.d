;;; wm.el --- EXWM config -*- lexical-binding: t; -*-

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))


(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

;; This function isn't currently used, only serves as an example how to
;; position a window
;; (defun efs/position-window ()
;;   (let* ((pos (frame-position))
;;          (pos-x (car pos))
;;           (pos-y (cdr pos)))

;;     (exwm-floating-move (- pos-x) (- pos-y))))

(defun efs/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-move-window 2))
    ("qutebrowser" (hide-mode-line-mode))
    ;; ("mpv" (exwm-floating-toggle-floating)
    ;;        (exwm-layout-toggle-mode-line))
    ))

;; This function should be used only after configuring autorandr!
;; (defun efs/update-displays ()
;;   (efs/run-in-background "autorandr --change --force")
;;   (efs/set-wallpaper)
;;   (message "Display config: %s"
;;            (string-trim (shell-command-to-string "autorandr --current"))))




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
  
  
  (efs/run-in-background "nm-applet")
  (efs/run-in-background "dropbox")
  (efs/run-in-background "davmail -server")
  (efs/run-in-background "~/bin/i3-battery-popup")

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)
  
  ;; rebind caps lock
  (start-process-shell-command "capslock" nil "~/bin/capslock.sh")
  
  ;; Automatically send the mouse cursor to the selected workspace's display
  (setq exwm-workspace-warp-cursor t)

  ;; Display all EXWM buffers in every workspace buffer list
  (setq exwm-workspace-show-all-buffers t)


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
      ?\C-\ ;;Ctrl+Space
      ?\M-\ ))  ;; Meta+Space
  
  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

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
          ([?\s-:] . (lambda (command)
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
          ([?\s-!] . (lambda () (interactive) (exwm-workspace-move-window 1)))
          ([?\s-@] . (lambda () (interactive) (exwm-workspace-move-window 2)))
          ([?\s-#] . (lambda () (interactive) (exwm-workspace-move-window 3)))
          ([?\s-$] . (lambda () (interactive) (exwm-workspace-move-window 4)))
          ([?\s-%] . (lambda () (interactive) (exwm-workspace-move-window 5)))
          ([?\s-^] . (lambda () (interactive) (exwm-workspace-move-window 6)))
          ([?\s-&] . (lambda () (interactive) (exwm-workspace-move-window 7)))
          ([?\s-*] . (lambda () (interactive) (exwm-workspace-move-window 8)))
          ([?\s-\(] . (lambda () (interactive) (exwm-workspace-move-window 9)))
          ([?\s-\)] . (lambda () (interactive) (exwm-workspace-move-window 0)))
	  ;; ,@(mapcar (lambda (i j)
	  ;; 	      `(,(kbd (format "s-%d" i)) .
	  ;; 		(lambda ()
	  ;; 		  (interactive)
	  ;; 		  (exwm-workspace-move-window ,j))))
	  ;; 	    '(?\) ?! ?@ ?# ?$ ?% ?^ ?& ?* ?\) )
	  ;; 	    (number-sequence 0-9))


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
