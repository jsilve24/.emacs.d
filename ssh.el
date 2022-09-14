(defun jds/dired-to-roar-home ()
    "Open tramp and SSH to Roar"
  (interactive)
  (let ((auth-sources nil))
    (dired "/ssh:jds6696@submit.aci.ics.psu.edu:~/")))

(defun jds/dired-to-roar-root ()
    "Open tramp and SSH to Roar"
  (interactive)
  (let ((auth-sources nil))
    (dired "/ssh:jds6696@submit.aci.ics.psu.edu:/")))
