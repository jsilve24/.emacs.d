;; wm.el --- EXWM config -*- lexical-binding: t; -*-

(load-config "autoloads/wm.el")

;; this depends on window.el and associated autoloads, some of the autoloads do depend on evil currently. 

;;; exwm main setup
(use-package exwm
  :init
  ;; default to not replacing existing window manager
  (setq exwm-replace nil)

  :config
  ;; set the default number of workspaces
  (setq exwm-workspace-number 6)
  ;; initial workspace
  (setq exwm-workspace-current-index 1)

  ;; Set the screen resolution (update this to be the correct resolution for your screen!)
  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal")

  ;; This will need to be updated to the name of a display!  You can find
  ;; the names of your displays by looking at arandr or the output of xrandr
  (cond
   ((string= (system-name) "lenovoGen4Sil")
    (setq exwm-randr-workspace-monitor-plist '(1 "eDP-1" 2 "eDP-1" 3 "HDMI-1-0" 4 "HDMI-1-0" 5 "DP-1-2" 6 "DP-1-2")))
   ((string= (system-name) "lenovoGen2Sil")
    (setq exwm-randr-workspace-monitor-plist '(1 "eDP-1" 2 "eDP-1" 3 "HDMI-1-0" 4 "HDMI-1-0" 5 "DP-1-0" 6 "DP-1-0"))))


  (add-hook 'exwm-randr-screen-change-hook #'efs/update-displays)
  (efs/update-displays)

  ;; Load the system tray before exwm-init
  ;; (require 'exwm-systemtray)
  ;; (setq exwm-systemtray-height 15)
  ;; (exwm-systemtray-enable)
  ;; to start manually
  ;; (exwm-systemtray--init)

  ;; make window divisions clear
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-right-width 1)
  (window-divider-mode 1)

  (display-time-mode 1)
  (setq display-time-24hr-format t
	display-time-day-and-date t)
  (display-battery-mode 1)

  (setq exwm-workspace-minibuffer-postion 'bottom)
  ;; (exwm-workspace-display-echo-area-timeout 1)


  ;; (efs/run-in-background "nm-applet")
  (efs/run-in-background "davmail -server")
  (efs/run-in-background "~/bin/i3-battery-popup")
  ;; (efs/run-in-background "caffeine")
  (efs/run-in-background "dropbox start")

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)

  ;; Automatically send the mouse cursor to the selected workspace's display
  (setq exwm-workspace-warp-cursor t)

  ;; Display all EXWM buffers in every workspace buffer list
  (setq exwm-workspace-show-all-buffers t)

  ;; allow pulling exwm buffers where I want (e.g., pull it from another window to current window
  ;; using consult-buffer)
  (setq exwm-layout-show-all-buffers t)

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; these keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
	'(?\C-x
	  ?\C-u
	  ?\C-h
	  ?\M-x
	  ?\M-`
	  ?\s-\t
	  ?\s-u ;; username
	  ?\s-p ;; password
	  ?\M-' ;; nothing at the moment, reserved though
	  ?\M-&
	  ?\M-:
	  ?\C-\M-j ;; Buffer list
	  ?\C-\ 	   ;;Ctrl+Space
	  ?\C-,
	  ?\C-' ;; popups dismisal
	  ?\C-\M-' ;; popup cycle
	  ?\C-\M-\" ;; toggle popup type (e.g., change to normal window)
	  ?\C-\\
	  ?\M-\ )) ;; Meta+Space

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
	`(
	  ([?\s-r] . hydra-resize/body)
	  ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
	  ([?\s-R] . exwm-reset)
	  ;; ([?\s-f] . exwm-layout-toggle-fullscreen)
	  ([?\s-f] . exwm-layout-toggle-fullscreen-or-single-window)
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

	  ;; ([?\s-v] . windmove-display-right)
	  ;; ([?\s-V] . windmove-display-left)
	  ;; ([?\s-s] . windmove-display-down)
	  ;; ([?\s-S] . windmove-display-up)
	  ;; ([?\s-p] . windmove-display-same-window) ;; "at point"

	  ([?\s-q] . delete-window)
	  ([?\s-Q] . jds/kill-buffer-delete-window)
	  ([?\s-d] . kill-current-buffer)
	  ([?\s-\ ] . ace-window)
	  ([?\s-b] . bury-buffer)
	  ([?\s-w] . balance-windows)

	  ;; Launch applications via shell command
	  ([?\s-:] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))

	  ;; Switch workspace
	  ;; ([?\s-w] . exwm-workspace-switch)
	  ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

;; 	  ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
;; 	  (\, @ (mapcar (lambda (i)
;; 			  `(,(kbd (format "s-%d" i)) .
;; 			    (lambda ()
;; 			      (interactive)
;; 			      (exwm-workspace-switch-create ,i))))
;; 			(number-sequence 0 9)))
;; 
;; 	  ;; 's-shift-N': move window to certain workspace
;; 	  ;; ([?\s-!] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 1)))
;; 	  ;; ([?\s-@] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 2)))
;; 	  ;; ([?\s-#] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 3)))
;; 	  ;; ([?\s-$] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 4)))
;; 	  ;; ([?\s-%] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 5)))
;; 	  ;; ([?\s-^] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 6)))
;; 	  ;; ([?\s-&] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 7)))
;; 	  ;; ([?\s-*] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 8)))
;; 	  ;; ([?\s-\(] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 9)))
;; 	  ;; ([?\s-\)] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 0)))
	  ))

  (exwm-input-set-key (kbd "s-`") #'aw-previous-window)
  (exwm-input-set-key (kbd "s-t") #'window-toggle-split-direction)

  (setq exwm-launcher-map (make-sparse-keymap))
  (exwm-input-set-key (kbd "s-<tab>") exwm-launcher-map)

  (exwm-input-set-key (kbd "s-e") #'jds~set-window-dedicated)

  ;; ;; Don't let ediff break EXWM, keep it in one frame
  (setq ediff-diff-options "-w"
	ediff-split-window-function 'split-window-horizontally
	ediff-window-setup-function 'ediff-setup-windows-plain)


  (exwm-enable)
  ;; (require 'exwm-config)
  ;; (exwm-config-default)
  )


(general-define-key
 :keymaps 'exwm-launcher-map
 ;; "q" '((lambda () (interactive) (run-or-raise-or-dismiss "qutebrowser" "qutebrowser")) :wk "qutebrowser")
 "q" '((lambda () (interactive) (start-process "qutebrowser" nil "qutebrowser")) :wk "qutebrowser-new-window")
 "Q" '((lambda () (interactive) (progn (+evil/window-vsplit-and-follow) (start-process "qutebrowser" nil "qutebrowser")))
       :wk "qutebrowser-new-window")
 ;; "y" '((lambda () (interactive) (run-or-raise-or-dismiss "slack" "Slack")) :wk "slack")
 ;; "c" '((lambda () (interactive) (jds/quiet-async-shell-commands "~/bin/capslock.sh")) :wk "capslock.sh")
 "v" 'evil-window-vsplit
 "s" 'evil-window-split
 "S" #'+evil/window-split-and-follow
 "V" #'+evil/window-vsplit-and-follow
 "RET" 'multi-vterm
 "S-<return>" #'(lambda () (interactive) (progn (+evil/window-vsplit-and-follow) (multi-vterm))))

;; allow moving between monitors
(use-package framemove
  :straight (framemove :type git :host github :repo "jsilve24/framemove")
  :config
  (setq framemove-hook-into-windmove t))

;; exwm-helper to move window / buffer to new frame (including fames not currently visible)
(use-package exwm-helper
  :disabled
  :commands eh-current-window-to-workspace-and-follow-by-index eh-current-window-to-workspace-and-follow-by-index
  :straight (exwm-helper :type git :host github :repo "jsilve24/exwm-helper")
  :config
  (setq eh-split-window-function 'jds~new-frame-or-new-window
	eh-last-window-function  '(lambda () (progn  (switch-to-buffer "*scratch*")))))

;; make windmove-display work more universally
;; this line was causing point to jump lines in mu4e header view when opening messages. 
;; this line was also causing problems and trigoring
;; window-size-change-functions on refocus. 
;; (setq switch-to-buffer-obey-display-actions t)

(use-package app-launcher
  :ensure t
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher")
  :config
  (exwm-input-set-key (kbd "s-;") #'app-launcher-run-app))

(with-eval-after-load 'app-launcher
  (with-eval-after-load 'marginalia
    (with-eval-after-load 'embark 
      ;; new completion category called "application"
      (add-to-list 'marginalia-prompt-categories '("Run app: " . application))

      ;; make app-launcher keymap for embark and add nice functions to them
      (embark-define-keymap embark-application-map
	"Keymap for use with app-launcher")
      (add-to-list 'embark-keymap-alist '(application . embark-application-map))
      (define-key embark-application-map (kbd "o") (jds/embark-ace-action app-launcher-run-app))
      (define-key embark-application-map (kbd "v") (jds/embark-split-action app-launcher-run-app +evil/window-vsplit-and-follow))
      (define-key embark-application-map (kbd "s") (jds/embark-split-action app-launcher-run-app +evil/window-split-and-follow))
      )))


;;; setup modeline
(use-package exwm-modeline
  :after (exwm)
  :config 
  (setq exwm-modeline-short t)
  (add-hook 'exwm-init-hook #'exwm-modeline-mode))

;;; setup brightness and volume
(use-package desktop-environment
  :diminish desktop-environment-mode
  :config
  ;; don't let this package set global exwm input keys
  (setq desktop-environment-update-exwm-global-keys nil
	desktop-environment-brightness-get-command "xbacklight -get"
	desktop-environment-brightness-set-command "xbacklight %s"
	desktop-environment-brightness-get-regexp "\\([0-9]+\n\\)"
	desktop-environment-brightness-normal-increment "+10"
	desktop-environment-brightness-normal-decrement "-10"
	desktop-environment-brightness-small-decrement "+5"
	desktop-environment-brightness-small-decrement "-5")
  ;; unset s-l binding
  (progn
    (setf
     (alist-get (elt (kbd "s-l") 0) desktop-environment-mode-map nil t)
     nil)
    (desktop-environment-mode)))


;;;###autoload
(defun jds/screen-awake-rest ()
  "Toggle screen to stay awake versus allow screensaver."
  (interactive)
  (save-match-data
    (let* ((settings (shell-command-to-string "xset q"))
	   (_ (string-match "DPMS is \\(\\w+\\)" settings)))
      (pcase (match-string 1 settings)
	("Disabled" (progn (shell-command "xset s on\nxset +dpms")
			   (message "Can't keep.. eyes... open....")))
	("Enabled" (progn (shell-command "xset s off\nxset -dpms"))
	 (message "We're gonna to RAGE!"))))))
(exwm-input-set-key (kbd "s-c") #'jds/screen-awake-rest)


(defun jds/nm-status ()
  (interactive)
  (async-shell-command "nmcli"))
(exwm-input-set-key (kbd "s-n") (lambda () (interactive)
				  (if popper-open-popup-alist
				      (popper-toggle-latest)
				    (jds/nm-status))))


