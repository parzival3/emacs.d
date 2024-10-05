;;; unix.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

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
      '((c-mode c-ts-mode c++-mode c++-ts-mode) . ("guix" "shell" "clang-toolchain" "glibc" "binutils" "coreutils" "eudev" "-C" "--" "clangd")))))

(use-package jinx
  :defer t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
          ("C-M-$" . jinx-languages)))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-alh"))


(use-package geiser
  :defer t
  :ensure nil
  :config
  ;; I can't use :bind here since it runs a compile time
  (eval `(bind-keys :map geiser-debug-mode-map ,@meow-normal-movement)))

  ;; :init
  ;; (with-eval-after-load 'geiser-guile
  ;;   (add-to-list 'geiser-guile-load-path "~/Git/guix")))

(use-package geiser-guile
  :defer t
  :ensure nil)

(provide 'unix)
