;;; spotify.el --- spotify configuration              -*- lexical-binding: t; -*-

(use-package smudge
  :init
  (setq smudge-status-location nil)

  :config
  (let* ((source (auth-source-search :host "silverman.spotify.com")))
    (setq smudge-oauth2-client-id (plist-get (car source) :user)
	  smudge-oauth2-client-secret (plist-get (car source) :secret)))
  ;; (setq smudge-transport 'connect)
  (global-smudge-remote-mode)

  (defun jds~smudge-status ()
      "Show current smudge status, e.g., tract in echo area."
    (interactive)
    (message smudge-controller-player-status))

  ;; A hydra for controlling spotify.
  (defhydra hydra-spotify (:hint nil)
    "
^Search^                  ^Control^               ^Manage^
^^^^^^^^-----------------------------------------------------------------
_t_: Track               _SPC_: Play/Pause        _+_: Volume up
_m_: My Playlists        _n_  : Next Track        _-_: Volume down
_f_: Featured Playlists  _p_  : Previous Track    _x_: Mute
_u_: User Playlists      _r_  : Repeat            _d_: Device
_c_: Current Track       _s_  : Shuffle           _q_: Quit
"
    ("t" smudge-track-search :exit t)
    ("m" smudge-my-playlists :exit t)
    ("f" smudge-featured-playlists :exit t)
    ("c" jds~smudge-status :exit nil)
    ("u" smudge-user-playlists :exit t)
    ("SPC" smudge-controller-toggle-play :exit nil)
    ("n" smudge-controller-next-track :exit nil)
    ("p" smudge-controller-previous-track :exit nil)
    ("r" smudge-controller-toggle-repeat :exit nil)
    ("s" smudge-controller-toggle-shuffle :exit nil)
    ("+" smudge-controller-volume-up :exit nil)
    ("-" smudge-controller-volume-down :exit nil)
    ("x" smudge-controller-volume-mute-unmute :exit nil)
    ("d" smudge-select-device :exit nil)
    ("q" quit-window "quit" :color blue)))


