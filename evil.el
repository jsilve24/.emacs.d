;;; evil.el --- Evil Bindings and Setup -*- lexical-binding: t; -*-

(load-config "autoloads/evil.el")

;;; simple undo system

;; (use-package undo-fu
;;   :straight t)

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))

;;; main evil config
(use-package evil
  :init
  (setq evil-respect-visual-line-mode t) ; needs to be front and center

  (setq evil-want-C-i-jump t
	evil-want-Y-yank-to-eol t
	evil-want-C-u-scroll nil
	evil-want-C-d-scroll nil
	evil-want-keybinding nil
	evil-want-integration t
	evil-ex-search-vim-style-regexp t
	evil-ex-visual-char-range t
	evil-mode-line-format nil
	evil-symbol-word-search t
	;; if the current state is obvious from the cursor's color/shape, then
	;; we won't need superfluous indicators to do it instead.
	;; evil-default-cursor '+evil-default-cursor-fn
	evil-normal-state-cursor 'box
	evil-emacs-state-cursor '(hbar . 6)
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
	evil-undo-system 'undo-tree
	;; stop cursor creep when switching back to normal mode
	evil-move-cursor-back nil
	;; allow cursor to move 1 character past end of line.
	evil-move-beyond-eol t
	;; Don't put overwritten text in the kill ring
	evil-kill-on-visual-paste nil
	;; Get better undo history in insert mode
	evil-want-fine-undo t)

  :config
  (evil-mode 1)

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

  ;; quick change and delete words
  (general-nmap "X" (general-simulate-key "d a w"))
  (general-nmap "C" (general-simulate-key "c i w")))

(use-package evil-collection
  :straight (evil-collection :type git :host github :repo "emacs-evil/evil-collection"
			     :fork (:host github :repo "jsilve24/evil-collection"))
  :after evil
  :diminish evil-collection
  :init
  (setq evil-collection-key-blacklist '("m")
	evil-collection-want-unimpaired-p t ; note this is myown highly customized version
	evil-collection-unimpaired-want-repeat-mode-integration t) 
  :config

  ;; move "m" bindings
  (evil-collection-define-key 'normal 'mu4e-headers-mode-map "M" #'mu4e-headers-mark-for-move)
  (evil-collection-define-key 'normal 'dired-mode-map "M" #'dired-mark)

  ;; don't use in hungry-delete
  (setq evil-collection-mode-list (remove 'hungry-delete evil-collection-mode-list))

  (evil-collection-init))


;;; Additional Evil packages ---------------------------------------------------


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
  (push '(?\( "[[{(]") evil-snipe-aliases)

  ;; It seems evil-snipe-override-mode causes problems in Magit buffers, to fix this:
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)

  (setq evil-snipe-smart-case t))

(use-package evil-nerd-commenter
  :straight t
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general ([remap comment-line] #'evilnc-comment-or-uncomment-lines))


(use-package evil-numbers
  :config
  (setq evil-numbers-use-cursor-at-end-of-number t))


(use-package evil-goggles
  :diminish evil-goggles-mode
  :ensure t
  :config

  ;; add lispyville operators
  (add-to-list 'evil-goggles--commands '(lispyville-yank :face evil-goggles-yank-face :switch evil-goggles-enable-yank :advice evil-goggles--generic-async-advice))
  (add-to-list 'evil-goggles--commands '(lispyville-delete :face evil-goggles-delete-face :switch evil-goggles-enable-delete :advice evil-goggles--generic-blocking-advice))
  (add-to-list 'evil-goggles--commands '(lispyville-change :face evil-goggles-change-face :switch evil-goggles-enable-change :advice evil-goggles--generic-blocking-advice))

  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  ;; (evil-goggles-use-diff-faces)

  ;; default is 0.2
  (setq evil-goggles-duration 0.2))

;; visual preview of evil-ex commands
(use-package evil-traces
  :diminish evil-traces-mode
  :config
  ;; (evil-traces-use-diff-faces) ; if you want to use diff's faces
  (evil-traces-mode))

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

(use-package posframe
  :disabled)

(use-package evil-owl
  :disabled 
  :config
  (setq evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args '(:width 70 :height 20)
        evil-owl-max-string-length 50)
  (evil-owl-mode))


;;; targets --------------------------------------------------------------------

(use-package targets
  :disabled
  :straight (targets :type git :host github :repo "noctuid/targets.el")
  :config
  (targets-setup)

  ;; If i am correct, this acts as a fall-back and only defines it when evil-tex or lispyville are not binding "d"
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

(load-config "autoloads/textobjects.el")

(provide 'config-evil)

