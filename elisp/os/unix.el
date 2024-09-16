;;; unix.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Key repeat
;;; on linux in order to increase the key repeat one can use
;;; xset r rate 200 130

(use-package eat
  :defer t
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode))

(use-package vterm
  :defer t)

(use-package eglot
  :defer t
  :config
  (when (executable-find "guix")
    (add-to-list 'eglot-server-programs
      '((c-mode c-ts-mode c++-mode c++-ts-mode) . ("guix" "shell" "clang-toolchain" "glibc" "binutils" "coreutils" "-C" "--" "clangd")))))

(use-package jinx
  :defer t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(provide 'unix)
