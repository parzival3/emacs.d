;;; unix.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
(use-package eat
  :defer t
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode))

(use-package vterm
  :defer t)

(provide 'unix)
