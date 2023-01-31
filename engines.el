;;; engines.el --- search engines -*- lexical-binding: t -*-

(use-package engine-mode
  :config
  (engine-mode t)

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine google
    "https://www.google.com/search?hl=en&q=%s"
    :keybinding "s")

  (defengine wikipedia
    "https://en.wikipedia.org/w/index.php?search=%s"
    :keybinding "w")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g"))

(provide 'config-engines)
