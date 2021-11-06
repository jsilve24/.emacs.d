;;; splash.el --- splash screen -*- lexical-binding: t; -*-

(define-derived-mode splash-mode
  fundamental-mode "Splash"
  "Major mode for showing custom splash screen."
  (splash--setup)
  ;; (run-with-timer 0.1 nil #'splash--setup)
  )

(defvar splash--banner
       "
@@@@@@@@  @@@@@@@@@@    @@@@@@    @@@@@@@   @@@@@@   
@@@@@@@@  @@@@@@@@@@@  @@@@@@@@  @@@@@@@@  @@@@@@@   
@@!       @@! @@! @@!  @@!  @@@  !@@       !@@       
!@!       !@! !@! !@!  !@!  @!@  !@!       !@!       
@!!!:!    @!! !!@ @!@  @!@!@!@!  !@!       !!@@!!    
!!!!!:    !@!   ! !@!  !!!@!!!!  !!!        !!@!!!   
!!:       !!:     !!:  !!:  !!!  :!!            !:!  
:!:       :!:     :!:  :!:  !:!  :!:           !:!   
:: ::::  :::     ::   ::   :::   ::: :::  :::: ::   
: :: ::    :      :     :   : :   :: :: :  :: : :    
"
       "Banner to display on startup.")

(defun splash--setup ()
  (let* ((splash-buffer (get-buffer-create "*splash*"))
	 (height  (- (window-body-height nil) 1))
	 (width (window-total-width nil))
	 (padding-top (- (/ height 2) 5))
	 (padding-left (- (/ width 2) 25)))
    (read-only-mode -1)
    (if (string= major-mode "splash-mode")
	(erase-buffer)
      nil)
    (insert-char ?\n padding-top)
    (put-text-property 0 (length splash--banner) 'face 'font-lock-string-face
		       splash--banner)
    (insert splash--banner)
    (mark-paragraph)
    (indent-region (point) (mark) padding-left)
    (deactivate-mark)
    (read-only-mode 1)
    (display-line-numbers-mode -1)
    (setq-local line-number-mode nil))
  (setq-local cursor-type nil))

;; (add-hook 'window-setup-hook 'jds/splash-setup)
;; (setq initial-buffer-choice #'jds/splash-setup)

(defun splash--reload (arg)
  (if (string= major-mode "splash-mode")
      ;; (run-with-timer 0.1 nil #'splash--setup)
      (splash--setup)
    nil))

(add-hook 'window-size-change-functions #'splash--reload)
;; (add-hook 'window-configuration-change-hook #'splash--reload)

(provide 'splash)
