;;; wm-autoloads.el --- functions for wm.el -*- lexical-binding: t -*-

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ;; ("qutebrowser" (exwm-workspace-rename-buffer (format "Qutebrowser: %s" exwm-title)))
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
    ;; ("qutebrowser" (exwm-workspace-rename-buffer (format "Qutebrowser: %s" exwm-title)))
    ;; ("qutebrowser" (jds~set-window-dedicated))
    ;; ("Google-chrome" (hide-mode-line-mode))
    ;; ("mpv" (exwm-floating-toggle-floating)
    ;;        (exwm-layout-toggle-mode-line))
    ))

;; This function should be used only after configuring autorandr!
(defun efs/update-displays ()
  (efs/run-in-background "autorandr --change")
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



;;; quick setup for teaching
;;;###autoload
(defun jds/setup-projector-and-wacom-E206 ()
  "Quick setup screen mirroring and wacom to HEAD-0 for teaching in Westgate E206"
  (interactive)
  (jds/quiet-async-shell-commands "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DP-1-0 --mode 1920x1080 --pos 1911x0 --rotate normal --output DP-1-1 --off --output HDMI-1-0 --off")
  (run-with-timer 1 nil (lambda ()  
			  (jds/quiet-async-shell-commands "xrandr --output DP-1-0 --same-as eDP-1")))
  (run-with-timer 5 nil (lambda ()
			  (jds/quiet-async-shell-commands "xsetwacom set 'Wacom Intuos BT S Pen stylus' MapToOutput HEAD-0"))))

;;;###autoload
(defun jds/setup-projector-and-wacom-willard-073 ()
  "Quick setup screen mirroring and wacom to HEAD-0 for teaching in Westgate E208"
  (interactive)	
  (jds/quiet-async-shell-commands "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DP-1-0 --off --output DP-1-1 --off --output HDMI-1-0 --mode 1920x1080 --pos 1920x0 --rotate normal")
  (jds/quiet-async-shell-commands "xrandr --output HDMI-1-0 --same-as eDP-1")
  (jds/quiet-async-shell-commands "xsetwacom set 'Wacom Intuos BT S Pen stylus' MapToOutput HEAD-0"))


;;;###autoload
(defun jds/setup-projector-and-wacom-E208 ()
  "Quick setup screen mirroring and wacom to HEAD-0 for teaching in Westgate E208"
  (interactive)
  (jds/quiet-async-shell-commands " xrandr --output eDP-1 --primary --mode 1920x1200 --pos 1920x0 --rotate normal --output HDMI-1 --off --output DP-1 --off --output DP-2 --off --output DP-3 --off --output DP-4 --off --output DP-1-0 --off --output DP-1-1 --off --output DP-1-2 --off --output DP-1-3 --off --output HDMI-1-0 --mode 1920x1080 --pos 0x0 --rotate normal")
  (jds/quiet-async-shell-commands "xrandr --output HDMI-1-0 --same-as eDP-1")
  (jds/quiet-async-shell-commands "xsetwacom set 'Wacom Intuos BT S Pen stylus' MapToOutput HEAD-0"))

;; make sure to connect with dell splitter
;;;###autoload
(defun jds/setup-projector-and-wacom-E210 ()
  "Quick setup screen mirroring and wacom to HEAD-0 for teaching in Westgate E208"
  (interactive)
  (jds/quiet-async-shell-commands "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DP-1-0 --mode 1920x1080 --pos 1920x0 --rotate normal --output DP-1-1 --off --output HDMI-1-0 --off")
  (jds/quiet-async-shell-commands "xrandr --output DP-1-0 --same-as eDP-1")
  (sit-for 15)
  (jds/quiet-async-shell-commands "xsetwacom set 'Wacom Intuos BT S Pen stylus' MapToOutput HEAD-0"))


;; also for quickly launching my personal zoom room
;;;###autoload
(defun jds~launch-zoom-by-conference-number (conf)
  "Use xdg-open to zoom opening CONF meeting ID. CONF should be a string."
  (jds/quiet-async-shell-commands
   (format "xdg-open 'zoommtg://zoom.us/join?action=join&confno=%s'" conf)))

