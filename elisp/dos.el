
(use-package eglot
  :defer t
  :straight t
  :config
  (global-set-key (kbd "C-.") 'eglot-code-actions)
  (when (eq system-type `windows-nt)
    (add-to-list 'eglot-server-programs
                 `(c++-mode . ("c:/Tools/LLVM/bin/clangd.exe")))))


  (when (executable-find "git.exe")
    (setq find-program (prin1-to-string
                        (concat
                         (file-name-directory (executable-find "git.exe"))
                         "../usr/bin/find.exe"))))


(use-package tramp
  :defer t
  :config
  (when (eq system-type 'windows-nt)
    (setq tramp-use-ssh-controlmaster-options nil)
    (add-to-list 'tramp-connection-properties
	         (list (regexp-quote "/ssh:")
		       "login-args"
		       '(("-tt") ("-l" "%u") ("-p" "%p") ("%c")
		         ("-e" "none") ("%h"))))))


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

(provide 'dos)
