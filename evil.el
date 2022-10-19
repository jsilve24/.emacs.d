;;; evil.el --- Evil Bindings and Setup -*- lexical-binding: t; -*-

;;; simple undo system

;; (use-package undo-fu
;;   :straight t)

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))

;;; main evil config
(straight-use-package 'evil)
(use-package evil
  :init
  (setq evil-respect-visual-line-mode t) ; needs to be front and center

  (setq evil-want-C-i-jump t
	evil-want-Y-yank-to-eol t
	evil-want-C-u-scroll nil
	evil-want-C-d-scroll nil
	evil-want-keybinding nil
	evil-want-integration t)
  ;; from doom 

  (setq evil-ex-search-vim-style-regexp t
	evil-ex-visual-char-range t
	evil-mode-line-format nil
	evil-symbol-word-search t
	;; if the current state is obvious from the cursor's color/shape, then
	;; we won't need superfluous indicators to do it instead.
	;; evil-default-cursor '+evil-default-cursor-fn
	evil-normal-state-cursor 'box
	evil-emacs-state-cursor  '(hbar . 6) 
	evil-insert-state-cursor 'bar
	evil-visual-state-cursor 'hollow
	evil-replace-state-cursor 'hbar
	;; with the above, dont need modeline indicator
	evil-mode-line-format nil
	;; Only do highlighting in selected window so that Emacs has less work
	;; to do highlighting them all.
	evil-ex-interactive-search-highlight 'selected-window
	;; It's infuriating that innocuous "beginning of line" or "end of line"
	;; errors will abort macros, so suppress them:
	evil-kbd-macro-suppress-motion-error t
	;; evil-undo-system 'undo-fu
	evil-undo-system 'undo-tree)

  :config
  (evil-mode 1)



  ;; done in core.el
  ;; ;; leader key
  ;; ;; set leader in all states
  ;; (evil-set-leader nil (kbd "M-SPC"))
  ;; ;; set leader in normal state
  ;; (evil-set-leader 'motion (kbd "SPC"))
  ;; ;; set leader in normal state
  ;; (evil-set-leader 'operator (kbd "SPC"))
  ;; ;; set leader in normal state
  ;; (evil-set-leader 'normal (kbd "SPC"))
  ;; ;; set local leader
  ;; (evil-set-leader 'normal "\\" t)
  ;; (evil-set-leader 'motion "\\" t)
  ;; (evil-set-leader 'operator "\\" t)

  ;; since using m as prefix
  (general-def 'normal "M" #'evil-set-marker)

  ;; remove ret binding as its pretty much useless but blocks some modes
  (define-key evil-motion-state-map (kbd "RET") nil)



  ;; make normal state the default always
  (setq evil-emacs-state-modes '(vterm-mode))
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)

  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; stop copying each visual state move to the clipboard:
  ;; https://github.com/emacs-evil/evil/issues/336
  ;; grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (advice-add #'evil-visual-update-x-selection :override #'ignore)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; stop cursor creep when switching back to normal mode
  (setq evil-move-cursor-back nil)
  ;; allow cursor to move 1 character past end of line.
  (setq evil-move-beyond-eol t)

  ;; Don't put overwritten text in the kill ring
  (setq evil-kill-on-visual-paste nil)

  ;; Get better undo history in insert mode
  (setq evil-want-fine-undo t)

  ;; deal with whitespace polluting kill-ring
  ;;from here: https://emacs.stackexchange.com/questions/39434/evil-dont-yank-with-only-whitespace-to-register
  ;; (define-key evil-normal-state-map "x" 'delete-forward-char)
  ;; (define-key evil-normal-state-map "X" 'delete-backward-char)
  ;; (evil-define-operator evil-delete-without-register-if-whitespace (beg end type reg yank-handler)
  ;;   (interactive "<R><y>")
  ;;   (let ((text (replace-regexp-in-string "\n" "" (filter-buffer-substring beg end))))
  ;;     (if (string-match-p "^\\s-*$" text)
  ;; 	  (evil-delete beg end type ?_)
  ;; 	(evil-delete beg end type reg yank-handler))))
  ;; (define-key evil-normal-state-map "d" #'evil-delete-without-register-if-whitespace)
  )

;;; evil-collection

(use-package evil-collection
  :straight (evil-collection :type git :host github :repo "emacs-evil/evil-collection"
			     :fork (:host github :repo "jsilve24/evil-collection"))
  :after evil
  :init 
  (setq evil-collection-key-blacklist '("m"))
  :config

  ;; move "m" bindings
  (evil-collection-define-key 'normal 'mu4e-headers-mode-map "M" #'mu4e-headers-mark-for-move)
  (evil-collection-define-key 'normal 'dired-mode-map "M" #'dired-mark)

  (evil-collection-init))


;;; Additional Evil packages




(use-package evil-snipe
  ;; :commands evil-snipe-local-mode evil-snipe-override-local-mode
  :ensure t
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'whole-visible
        evil-snipe-repeat-scope 'whole-visible
        evil-snipe-char-fold t)
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  (evil-define-key 'visual evil-snipe-local-mode-map "q" 'evil-snipe-s)
  (evil-define-key 'visual evil-snipe-local-mode-map "Q" 'evil-snipe-S)
  (evil-define-key 'visual evil-snipe-local-mode-map "x" 'evil-snipe-x)
  (evil-define-key 'visual evil-snipe-local-mode-map "X" 'evil-snipe-X)

  ;; To map [ to any opening parentheses or bracket in all modes:
  (push '(?\[ "[[{(]") evil-snipe-aliases)

  ;; It seems evil-snipe-override-mode causes problems in Magit buffers, to fix this:
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)

  (setq evil-snipe-smart-case t))

(use-package evil-nerd-commenter
  :straight t
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general ([remap comment-line] #'evilnc-comment-or-uncomment-lines))



;;; evil-suround and evil-embrace
(use-package evil-surround
  :straight t
  :ensure t
  :config
  (global-evil-surround-mode 1)
  ;; not sure why its bound to gS or S in visual state but I don't like the asymmetry
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-Surround-region))


(use-package evil-embrace
  :after evil-surround
  :config
  (evil-embrace-enable-evil-surround-integration)
  (add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook))
;; see here: https://github.com/cute-jumper/embrace.el#adding-more-surrounding-pairs
;; for how to add more custom pairs


;;; setup evil alignment (evil-lion)
(use-package evil-lion
  :straight t
  :defer t
  :config
  ;; these need to be called before evil-lion-mode is called
  (setq evil-lion-left-align-key (kbd "z l"))
  (setq evil-lion-right-align-key (kbd "z L"))
  (evil-lion-mode))


(use-package evil-args
  :demand t
  :after evil
  :commands (evil-inner-arg evil-outer-arg
             evil-forward-arg evil-backward-arg
             evil-jump-out-args))


(use-package posframe)

(use-package evil-owl
  :disabled 
  :config
  (setq evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args '(:width 70 :height 20)
        evil-owl-max-string-length 50)
  (evil-owl-mode))


;;; custom evil objects --------------------------------------------------------

;; from here; http://blog.binchen.org/posts/code-faster-by-extending-emacs-evil-text-object/
;; (defun jds~evil-paren-range (count beg end type inclusive)
;;   "Get minimum range of paren text object.
;; COUNT, BEG, END, TYPE is used.  If INCLUSIVE is t, the text object is inclusive."
;;   (let* ((point (point))
;; 	 ;; (parens '("\(\)" "\[\]" "{}" "<>"))
;; 	 (parens '((?\( . ?\))
;; 		   (?\[ . ?\])
;; 		   (?{ . ?})
;; 		   (?< . ?>)))
;; 	 range
;;          found-range)
;;     (dolist (p parens)
;;       (condition-case nil
;; 	  (if (characterp (cdr p))
;; 	      (setq range (evil-select-paren (car p) (cdr p) beg end type count inclusive))
;; 	    (setq range (evil-select-block #'(lambda (&optional cnt) (evil-up-block (car p) (cdr p) cnt))
;; 					   beg end type count inclusive)))
;; 	(error nil))
;;       (when range
;; 	;; (message (format "%s, %s" (nth 0 range) (nth 1 range)))
;;         (cond
;;          (found-range
;;           (when (< (abs (- (car range) point))
;; 		   (abs (- (car found-range) point)))
;;             (setf (nth 0 found-range) (nth 0 range))
;;             (setf (nth 1 found-range) (nth 1 range))))
;;          (t
;;           (setq found-range range)))))
;;     found-range))

;; (evil-define-text-object jds~evil-a-paren (count &optional beg end type)
;;   "Select a paren."
;;   :extend-selection t
;;   (jds~evil-paren-range count beg end type t))

;; (evil-define-text-object jds~evil-inner-paren (count &optional beg end type)
;;   "Select 'inner' paren."
;;   :extend-selection nil
;;   (jds~evil-paren-range count beg end type nil))

;; (define-key evil-inner-text-objects-map "d" #'jds~evil-inner-paren)
;; (define-key evil-outer-text-objects-map "d" #'jds~evil-a-paren)



;; (use-package evil-traces
;;   :straight t
;;   :after evil-ex
;;   :config
;;   (pushnew! evil-traces-argument-type-alist
;;             '(+evil:align . evil-traces-global)
;;             '(+evil:align-right . evil-traces-global))
;;   (evil-traces-mode))

;;; tragets --------------------------------------------------------------------


(use-package targets
  :straight (targets :type git :host github :repo "noctuid/targets.el")

  :config
  (targets-setup)
  (targets-define-composite-to pair-delimiter
    (("(" ")" pair)
     ("[" "]" pair)
     ("{" "}" pair)
     ("<" ">" pair))
    :bind t
    :next-key "N"
    :last-key "L"
    :remote-key "r"
    :keys "d"))
;; (use-package targets
;;   :straight (targets :type git :host github :repo "noctuid/targets.el")
;;   :init
;;   ;; (setq targets-user-text-objects '((pipe "|" nil separator)
;;   ;; 				    (paren "(" ")" pair :more-keys "b")
;;   ;; 				    (bracket "[" "]" pair :more-keys "r")
;;   ;; 				    (curly "{" "}" pair :more-keys "c")))
;;   :config
;;   (targets-define-to paren "(" ")" pair)
;;   (targets-setup t
;; 		 :inside-key nil
;; 		 :around-key nil
;; 		 :remote-key nil))


(provide 'evil)
;;; evil.el ends here

