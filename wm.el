;;; wm.el --- EXWM config -*- lexical-binding: t; -*-

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/set-wallpaper ()
  (interactive)
  ;; NOTE: You will need to update this to a valid background path!
  (start-process-shell-command
      "feh" nil  "feh --bg-scale /home/jds6696/.local/share/wallpapers/grey-arch.jpg"))

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ("qutebrowser" (exwm-workspace-rename-buffer (format "Qutebrowser: %s" exwm-title)))
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
    ;; ("Firefox" (exwm-workspace-move-window 2))
    ("qutebrowser" (exwm-workspace-rename-buffer (format "Qutebrowser: %s" exwm-title)))
    ;; ("Google-chrome" (hide-mode-line-mode))
    ;; ("mpv" (exwm-floating-toggle-floating)
    ;;        (exwm-layout-toggle-mode-line))
    ))



;; This function should be used only after configuring autorandr!
(defun efs/update-displays ()
  (efs/run-in-background "autorandr --change --force")
  (efs/set-wallpaper)
  (message "Display config: %s"
	   (string-trim (shell-command-to-string "autorandr --current"))))




(defun exwm-layout-toggle-fullscreen-or-single-window ()
  (interactive)
  (if (eq major-mode 'exwm-mode)
      (call-interactively 'exwm-layout-toggle-fullscreen)
    (toggle-single-window)))

;; function that maximizes not just x windows but also emacs buffers
(defvar single-window--last-configuration nil "Last window conf wiguration before calling `delete-other-windows'.")
(defun toggle-single-window ()
  "Un-maximize current window.
If multiple windows are active, save window configuration and
delete other windows.  If only one window is active and a window
configuration was previously save, restore that configuration."
  (interactive)
  (if (= (count-windows) 1)
      (when single-window--last-configuration
        (set-window-configuration single-window--last-configuration))
    (setq single-window--last-configuration (current-window-configuration))
    (delete-other-windows)))


;; function to toggle vertical horizontal splits
;; from wiki: https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.
i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))



;;; exwm main setup
(use-package exwm
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
  (setq exwm-randr-workspace-monitor-plist '(1 "eDP-1" 2 "eDP-1" 3 "HDMI-1-1" 4  "HDMI-1-1" 5 "DP-1-1" 6 "DP-1-1"))

  (add-hook 'exwm-randr-screen-change-hook #'efs/update-displays)
  (efs/update-displays)

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 15)
  (exwm-systemtray-enable)

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
  
  
  (efs/run-in-background "nm-applet")
  (efs/run-in-background "davmail -server")
  (efs/run-in-background "~/bin/i3-battery-popup")
  (efs/run-in-background "caffeine")
  (efs/run-in-background "dropbox")

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
      ?\M-' ;; nothing at the moment, reserved though
      ?\M-&
      ?\M-:
      ?\C-\M-j ;; Buffer list
      ?\C-\    ;;Ctrl+Space
      ?\C-,
      ?\C-' ;; popups dismisal
      ?\C-\M-' ;; popup cycle
      ?\C-\M-\" ;; toggle popup type (e.g., change to normal window) 
      ?\C-\\
      ?\M-\ ))  ;; Meta+Space
  
  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

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

	  ([?\s-v] . windmove-display-right)
	  ([?\s-V] . windmove-display-left)
	  ([?\s-s] . windmove-display-down)
	  ([?\s-S] . windmove-display-up)
	  ([?\s-p] . windmove-display-same-window) ;; "at point"

	  ([?\s-q] . delete-window)
	  ([?\s-Q] . jds/kill-buffer-delete-window)
	  ([?\s-d] . kill-current-buffer)
	  ([?\s-\ ] . ace-window)
	  ([?\s--] . bury-buffer)

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
          ;; ([?\s-!] . (lambda () (interactive) (exwm-workspace-move-window 1)))
          ;; ([?\s-@] . (lambda () (interactive) (exwm-workspace-move-window 2)))
          ;; ([?\s-#] . (lambda () (interactive) (exwm-workspace-move-window 3)))
          ;; ([?\s-$] . (lambda () (interactive) (exwm-workspace-move-window 4)))
          ;; ([?\s-%] . (lambda () (interactive) (exwm-workspace-move-window 5)))
          ;; ([?\s-^] . (lambda () (interactive) (exwm-workspace-move-window 6)))
          ;; ([?\s-&] . (lambda () (interactive) (exwm-workspace-move-window 7)))
          ;; ([?\s-*] . (lambda () (interactive) (exwm-workspace-move-window 8)))
          ;; ([?\s-\(] . (lambda () (interactive) (exwm-workspace-move-window 9)))
          ;; ([?\s-\)] . (lambda () (interactive) (exwm-workspace-move-window 0)))
          ([?\s-!] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 1)))
          ([?\s-@] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 2)))
          ([?\s-#] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 3)))
          ([?\s-$] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 4)))
          ([?\s-%] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 5)))
          ([?\s-^] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 6)))
          ([?\s-&] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 7)))
          ([?\s-*] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 8)))
          ([?\s-\(] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 9)))
          ([?\s-\)] . (lambda () (interactive) (eh-current-window-to-workspace-and-follow-by-index 0)))
	  ;; ,@(mapcar (lambda (i j)
	  ;; 	      `(,(kbd (format "s-%d" i)) .
	  ;; 		(lambda ()
	  ;; 		  (interactive)
	  ;; 		  (exwm-workspace-move-window ,j))))
	  ;; 	    '(?\) ?! ?@ ?# ?$ ?% ?^ ?& ?* ?\) )
	  ;; 	    (number-sequence 0-9))


	  ))

  (defun switch-to-last-buffer ()
  "Switch to last open buffer in current window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
  (exwm-input-set-key (kbd "s-`") #'switch-to-last-buffer)
  (exwm-input-set-key (kbd "s-t") #'window-toggle-split-direction)

  (exwm-enable)
  ;; (require 'exwm-config)
  ;; (exwm-config-default)
  )


(with-eval-after-load 'exwm

  ;; make sure window really closes when killing exwm buffer
  (defun jds/kill-buffer-delete-window ()
    "Simpler than kill-buffer-and-window but that was not working for EXWM windows."
    (interactive)
    (if (not (string= major-mode "exwm-mode"))
	(kill-buffer-and-window)
      (let ((buffer (current-buffer))
	    (window (selected-window)))
	(kill-buffer buffer)
	(delete-window window))))

  ;; from exwm cookbook
  (defun exwm-async-run (name)
    "Run a process asynchronously"
    (interactive)
    (start-process name nil name))

  (defun run-or-raise-or-dismiss (program program-buffer-name)
    "If no instance of the program is running, launch the program.
If an instance already exists, and its corresponding buffer is
displayed on the screen, move to the buffer. If the buffer is not
visible, switch to the buffer in the current window. Finally, if
the current buffer is already that of the program, bury the
buffer (=minimizing in other WM/DE)"
    ;; check current buffer
    (if (string= (buffer-name) program-buffer-name)
	(bury-buffer)
      ;; either switch to or launch program
      (progn
	(if (get-buffer program-buffer-name)
	    (progn
	      (if (get-buffer-window program-buffer-name)
		  (select-window (display-buffer program-buffer-name) nil)
		(exwm-workspace-switch-to-buffer program-buffer-name)))
	  ;; start program
	  (exwm-async-run program)))))
  (setq exwm-launcher-map (make-sparse-keymap))
  (exwm-input-set-key (kbd "s-<tab>") exwm-launcher-map))

(with-eval-after-load 'evil
  (general-define-key
   :keymaps 'exwm-launcher-map
   ;; "q" '((lambda () (interactive) (run-or-raise-or-dismiss "qutebrowser" "qutebrowser")) :wk "qutebrowser")
   "q" '((lambda () (interactive) (exwm-async-run "qutebrowser")) :wk "qutebrowser-new-window")
   "Q" '((lambda () (interactive) (progn (+evil/window-vsplit-and-follow) (exwm-async-run "qutebrowser"))) :wk "qutebrowser-new-window")
   "y" '((lambda () (interactive) (run-or-raise-or-dismiss "slack" "Slack")) :wk "slack")
   "c" '((lambda () (interactive) (async-shell-command "~/bin/capslock.sh")) :wk "capslock.sh")
   "v" 'evil-window-vsplit
   "s" 'evil-window-split
   "S" #'+evil/window-split-and-follow
   "V" #'+evil/window-vsplit-and-follow
   "RET" 'multi-vterm
   "S-<return>" #'(lambda () (interactive) (progn (+evil/window-vsplit-and-follow) (multi-vterm)))))

;; allow moving between monitors
(use-package framemove
  :straight (framemove :type git :host github :repo "jsilve24/framemove")
  :config
  (setq framemove-hook-into-windmove t))


;; exwm-helper to move window / buffer to new frame (including fames not currently visible)
(use-package exwm-helper
  :commands eh-current-window-to-workspace-and-follow-by-index eh-current-window-to-workspace-and-follow-by-index
  :straight (exwm-helper :type git :host github :repo "jsilve24/exwm-helper")
  :config
  (setq eh-split-window-function 'jds~new-frame-or-new-window
	eh-last-window-function  '(lambda () (progn  (switch-to-buffer "*splash*") (kisses-recenter)))))


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
    (define-key embark-application-map (kbd "s") (jds/embark-split-action app-launcher-run-app +evil/window-split-and-follow)))))



;;; setup modeline
;; from here: https://chrishayward.xyz/dotfiles/
;; Define a modeline segment to show the workspace information.
(with-eval-after-load 'doom-modeline
  (doom-modeline-def-segment jds/exwm-workspaces
    (exwm-workspace--update-switch-history)
    (concat
     (doom-modeline-spc)
     (elt (let* ((num (exwm-workspace--count))
		 (sequence (number-sequence 0 (1- num)))
		 (not-empty (make-vector num nil)))
	    (dolist (i exwm--id-buffer-alist)
	      (with-current-buffer (cdr i)
		(when exwm--frame
		  (setf (aref not-empty
			      (exwm-workspace--position exwm--frame))
			t))))
	    (mapcar
	     (lambda (i)
	       (mapconcat
		(lambda (j)
		  ;; (format (if (= i j) "[%s]" " %s ")
		  (format (if (= i j) "[%s]" "")
			  (propertize
			   (apply exwm-workspace-index-map (list j))
			   'face
			   (cond ((frame-parameter (elt exwm-workspace--list j)
						   'exwm-urgency)
				  '(:inherit warning :weight bold))
				 ;; ((= i j) '(:inherit underline :weight bold))
				 ((= i j) '(:weight bold))
				 ((aref not-empty j) '(:inherit success :weight bold))
				 (t `((:foreground ,(face-foreground 'mode-line-inactive))))))))
		sequence ""))
	     sequence))
	  (exwm-workspace--position (selected-frame)))))

  ;; Define a custom modeline to override the default.
  (doom-modeline-def-modeline 'jds/modeline
    ;; '(bar workspace-name jds/exwm-workspaces window-number modals buffer-info remote-host buffer-position word-count parrot selection-info)
    '(bar workspace-name jds/exwm-workspaces window-number modals buffer-info remote-host word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))

  ;; Define a method to load the modeline.
  (defun jds/load-modeline ()
    "Load the default modeline."
    (doom-modeline-set-modeline 'jds/modeline 'default))

  (add-hook 'doom-modeline-mode-hook 'jds/load-modeline)
  ;; (doom-modeline-mode +1)
  (doom-modeline-set-modeline 'dotfiles/modeline 'default))

;; (defun jds/current-workspace ()

;;   )

;; (use-package smart-mode-line
;;   :config
;;   (setq sml/theme 'respectful)
;;   (sml/setup))
;; (use-package mini-modeline
;;   ;; :after smart-mode-line
;;   :config
;;   (setq ring-bell-function 'ignore)
;;   (mini-modeline-mode t)
;;   ;; (setq mini-modeline-l-format '(:eval (format "[%s]" (exwm-workspace--position (selected-frame))))) ;
;;   (setq mini-modeline-l-format '(:eval (format "[%s]" exwm-workspace-current-index))) ;
;;   )
;; (use-package diminish
;;   :after mini-modeline
;;   :config
;;   (diminish 'evil-snipe-local-mode)
;;   (diminish 'projectile-mode)
;;   (diminish 'eldoc-mode)
;;   (diminish 'abbrev-mode)
;;   (diminish 'company-mode)
;;   (diminish 'yas-minor-mode)
;;   (diminish 'mini-modeline-mode)
;;   (diminish 'which-key-mode)
;;   (diminish 'visual-line-mode)
;;   (diminish 'evil-collection-unimpaired-mode))



;;; fixing issues (e.g., ediff)

;; ;; Don't let ediff break EXWM, keep it in one frame
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'wm)
