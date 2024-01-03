
(use-package emacs
  :config
  ;; configuring some environment variables
  (setenv "PATH" (concat "C:\\Tools\\LLVM\\bin;" (getenv "PATH")))
  (setenv "PATH" (concat "C:\\Tools\\Git\\bin;" (getenv "PATH")))
  (setenv "PATH" (concat "C:\\Tools\\Git\\usr\\bin;" (getenv "PATH")))

  (add-to-list 'exec-path "C:\\Tools\\LLVM\\bin")

  ;; on windows we need to clean the clipboard before pasting
  (advice-add 'clipboard-yank :around #'et-clean-clipboard-yank)
  (advice-add 'yank :around #'et-clean-clipboard-yank)

  (setq find-program "C:\\Tools\\Git\\usr\\bin\\find.exe"))


(use-package eglot
  :defer t
  :config
  (add-to-list 'eglot-server-programs
                 `(c++-mode . ("c:/Tools/LLVM/bin/clangd.exe"))))

(use-package tramp
  :defer t
  :config
  (setq tramp-use-ssh-controlmaster-options nil)
  (add-to-list 'tramp-connection-properties
	         (list (regexp-quote "/ssh:")
		       "login-args"
		       '(("-tt") ("-l" "%u") ("-p" "%p") ("%c")
		         ("-e" "none") ("%h")))))


(use-package dired
  :config
    (when (eq system-type 'windows-nt)
    (defun et-dired-look ()
      (when (derived-mode-p 'dired-mode)
        (setq-local line-spacing 0.1)
        (setq-local left-margin-width 1)
        (setq-local right-margin-width 1)
        (setq-local word-wrap t)
        (setq-local truncate-lines nil)
        (setq-local truncate-partial-width-windows nil)
        (setq-local wrap-prefix "  ")
        (setq-local dired-listing-switches "-alh --group-directories-first")
        (dired-hide-details-mode 1)))
    (add-hook 'dired-mode-hook #'et-dired-look)))


(use-package compile
  :config
  (setq compilation-scroll-output 'first-error)
  (setq compile-command "msbuild"))


(use-package magit
  :defer t
  :config
  ;; simplify magit status headers
  (setq magit-status-headers-hook '(magit-insert-head-branch-header))

  ;; remove some magit status sections
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)))

(provide 'dos)
