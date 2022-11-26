;;; pandoc.el --- summary -*- lexical-binding: t -*-

(use-package pandoc-mode
  :hook (pandoc-mode . pandoc-load-default-settings))
