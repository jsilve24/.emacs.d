;;; term.el --- vterm setup -*- lexical-binding: t; -*-

;; (straight-use-package 'vterm)
;; (use-package vterm
;;   :commands vterm
;;   :hook (vterm-mode . hide-mode-line-mode)
;;   ;; :bind (:map vterm-mode-map
;;   ;; 	      ("C-c C-c" . vterm-send-C-c)
;;   ;; 	      ("C-c C-d" . vterm-send-C-d))
;;   :config
;;   (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
;;   ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
;;   (setq vterm-max-scrollback 10000)
;;   (setq vterm-kill-buffer-on-exit t)
;;   (setq vterm-copy-exclude-prompt t)

;;   ;; don't show line numbers
;;   (add-hook 'vterm-mode-hook  (lambda() (display-line-numbers-mode 0))))

;; ;; (general-define-key
;; ;;  :keymaps 'vterm-mode-map
;; ;;  :state '(n v i m e)
;; ;;  "C-c C-c" #'vterm-send-C-c
;; ;;  "C-c C-d" #'vterm-send-C-d)


(use-package hide-mode-line)

(use-package multi-vterm
        ;; :hook  (vterm-mode . hide-mode-line-mode)
	:config
	(add-hook 'vterm-mode-hook
			(lambda ()
			;; (setq-local evil-insert-state-cursor 'box)
			(evil-insert-state)))
	(define-key vterm-mode-map [return]                      #'vterm-send-return)

	(setq vterm-keymap-exceptions nil)
	(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
	;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
	(setq vterm-max-scrollback 10000)
	(setq vterm-kill-buffer-on-exit t)
	(setq vterm-copy-exclude-prompt t)

	(evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
	(evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
	;; (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
	;; ;; (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
	;; ;; (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
	;; (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
	(evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume))

(jds/localleader-def
  :keymaps 'vterm-mode-map
  :states 'normal
  "c" #'multi-vterm
  "h" #'multi-vterm-next
  "l" #'multi-vterm-prev)

;;; autoloads ------------------------------------------------------------------

;;;###autoload
(defun jds/multi-vterm-same-window (&optional arg)
    "Open multi-vterm in a new window (on EXWM) or new frame. Open new term if universal prefix passed. "
  (interactive "P")
  (if arg
      (multi-vterm)
    (multi-vterm-next)))


;;;###autoload
(defun jds/multi-vterm-new-window-or-frame (&optional arg)
  "Open multi-vterm in a new window (on EXWM) or new frame. Open new term if universal prefix passed.
With two prefixes, disable window balancing, with three prefixes, disable window balancing and open new term."
  (interactive "p")
  (if (not arg)
      (progn
	(jds~new-frame-or-new-window)
	(jds/multi-vterm-same-window))
    (cond
     ((= arg 4) (progn (jds~new-frame-or-new-window)
		       (jds/multi-vterm-same-window t)))

     ((= arg 16) (progn (jds~new-frame-or-new-window t)
			(jds/multi-vterm-same-window)))

     ((= arg 64) (progn (jds~new-frame-or-new-window t)
			(jds/multi-vterm-same-window t))))))

(provide 'term)
;;; term.el ends here
